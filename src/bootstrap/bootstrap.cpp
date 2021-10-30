// Copyright (c) 2021-2021 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include "util.h"

#include <iostream>
#include <thread>
#include <chrono>
#include <fstream>

#include <libtorrent/session.hpp>
#include <libtorrent/add_torrent_params.hpp>
#include <libtorrent/torrent_handle.hpp>
#include <libtorrent/torrent_info.hpp>
#include <libtorrent/alert_types.hpp>
#include <libtorrent/bencode.hpp>
#include <libtorrent/torrent_status.hpp>
#include <libtorrent/read_resume_data.hpp>
#include <libtorrent/write_resume_data.hpp>
#include <libtorrent/error_code.hpp>
#include <libtorrent/magnet_uri.hpp>

namespace {

using clk = std::chrono::steady_clock;

// return the name of a torrent status enum
char const* state(lt::torrent_status::state_t s)
{
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcovered-switch-default"
#endif
  switch(s) {
    case lt::torrent_status::checking_files: return "checking";
    case lt::torrent_status::downloading_metadata: return "dl metadata";
    case lt::torrent_status::downloading: return "downloading";
    case lt::torrent_status::finished: return "finished";
    case lt::torrent_status::seeding: return "seeding";
    case lt::torrent_status::checking_resume_data: return "checking resume";
    default: return "<>";
  }
#ifdef __clang__
#pragma clang diagnostic pop
#endif
}

bool ends_with(std::string const &str, std::string const &suffix) {
    if (str.length() >= suffix.length()) {
        return (0 == str.compare(str.length() - suffix.length(),
                                 suffix.length(),
                                 suffix));
    } else {
        return false;
    }
}

} // anonymous namespace

int main(int argc, char const* argv[]) try
{
  lt::settings_pack pack;
  pack.set_int(lt::settings_pack::alert_mask
    , lt::alert_category::error
    | lt::alert_category::storage
    | lt::alert_category::status);

  lt::session ses(pack);
  clk::time_point last_save_resume = clk::now();

  // load resume data from disk and pass it in as we add the magnet link
  std::string resume_file = (
      GetDefaultDataDir() / ".bootstrap_resume_file").string();
  std::ifstream ifs(resume_file, std::ios_base::binary);
  ifs.unsetf(std::ios_base::skipws);
  std::vector<char> buf{std::istream_iterator<char>(ifs)
    , std::istream_iterator<char>()};

  lt::add_torrent_params magnet = lt::parse_magnet_uri(BOOTSTRAP_MAGNET);
  if (buf.size()) {
    lt::add_torrent_params atp = lt::read_resume_data(buf);
    if (atp.info_hash == magnet.info_hash)
      magnet = std::move(atp);
  }
  magnet.save_path = GetDefaultDataDir().string();
  ses.async_add_torrent(std::move(magnet));

  // this is the handle we'll set once we get the notification of it being
  // added
  lt::torrent_handle h;

  // set when we're exiting
  bool done = false;
  bool renamed = false;
  for (;;) {
    std::vector<lt::alert*> alerts;
    ses.pop_alerts(&alerts);

    for (lt::alert const* a : alerts) {
      if (auto at = lt::alert_cast<lt::add_torrent_alert>(a)) {
        h = at->handle;
      }
      // if we receive the finished alert or an error, we're done
      if (lt::alert_cast<lt::torrent_finished_alert>(a)) {
        h.save_resume_data(lt::torrent_handle::save_info_dict);
        done = true;
      }
      if (lt::alert_cast<lt::torrent_error_alert>(a)) {
        std::cout << a->message() << std::endl;
        done = true;
        h.save_resume_data(lt::torrent_handle::save_info_dict);
      }

      // when we already have the metadata, check the files
      if (!renamed && h.is_valid() && h.has_metadata()) {
        const int n_files = h.get_torrent_info().files().num_files();
        for (int i = 0; i < n_files; i++) {
          std::string path = h.get_torrent_info().files().file_path(i);
          if (ends_with(path, "/bootstrap.dat")) {
            h.rename_file(i, "bootstrap.dat");
          }
          renamed = true;
        }
      }

      // when resume data is ready, save it
      if (auto rd = lt::alert_cast<lt::save_resume_data_alert>(a)) {
        std::ofstream of(resume_file, std::ios_base::binary);
        of.unsetf(std::ios_base::skipws);
        auto const b = write_resume_data_buf(rd->params);
        of.write(b.data(), int(b.size()));
        if (done) goto done;
      }

      if (lt::alert_cast<lt::save_resume_data_failed_alert>(a)) {
        if (done) goto done;
      }

      if (auto st = lt::alert_cast<lt::state_update_alert>(a)) {
        if (st->status.empty()) continue;

        // we only have a single torrent, so we know which one
        // the status is for
        lt::torrent_status const& s = st->status[0];
        std::cout << '\r' << state(s.state) << ' '
          << (s.download_payload_rate / 1000) << " kB/s "
          << (s.total_done / 1000) << " / "
          << (s.total_wanted / 1000) << " kB ("
          << (s.progress_ppm / 10000) << "%) downloaded ("
          << s.num_peers << " peers)\x1b[K";
        std::cout.flush();
      }
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(200));

    // ask the session to post a state_update_alert, to update our
    // state output for the torrent
    ses.post_torrent_updates();

    // save resume data once every 30 seconds
    if (clk::now() - last_save_resume > std::chrono::seconds(30)) {
      h.save_resume_data(lt::torrent_handle::save_info_dict);
      last_save_resume = clk::now();
    }
  }

done:
  std::cout << "\ndone, shutting down" << std::endl;
}
catch (std::exception& e)
{
  std::cerr << "Error: " << e.what() << std::endl;
}

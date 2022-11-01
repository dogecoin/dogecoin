// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_RELEASEDATES_H
#define DOGECOIN_RELEASEDATES_H

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost::posix_time;
const std::string release_date_str = CURRENT_RELEASE_DATE;

const ptime POSIX_RELEASE_DATE  = time_from_string(release_date_str + " 00:00:00");
const ptime POSIX_OBSOLETE_DATE = POSIX_RELEASE_DATE + hours(EXPECTED_DAYS_TO_NEXT_RELEASE * 24);

inline bool dogecoin_release_is_outdated() {
    const ptime current_time = second_clock::local_time();

    return current_time > POSIX_OBSOLETE_DATE;
}

#endif // DOGECOIN_RELEASEDATES_H

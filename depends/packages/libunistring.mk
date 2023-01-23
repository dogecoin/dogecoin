package=libunistring
$(package)_version=1.1
$(package)_download_path=https://ftp.gnu.org/gnu/libunistring/
$(package)_file_name=$(package)-$($(package)_version).tar.gz
$(package)_sha256_hash=a2252beeec830ac444b9f68d6b38ad883db19919db35b52222cf827c385bdb6a

define $(package)_set_vars
  $(package)_config_opts=--disable-shared --enable-static
  $(package)_config_opts_mingw32=--enable-threads=windows
endef

define $(package)_config_cmds
  $($(package)_autoconf) CFLAGS="-fPIC"
endef

define $(package)_build_cmds
  $(MAKE)
endef

define $(package)_stage_cmds
  $(MAKE) DESTDIR=$($(package)_staging_dir) install
endef

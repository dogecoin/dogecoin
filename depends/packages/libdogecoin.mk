package=libdogecoin
$(package)_version=0.1.4
$(package)_download_path=https://github.com/dogecoinfoundation/libdogecoin/archive/refs/tags
$(package)_file_name=v$($(package)_version).tar.gz
$(package)_sha256_hash=e1520ef918dcee30e2f87fbfffc3a40621ffc8f5d824e602cf6ad5cc35ba8c50
$(package)_dependencies=libevent libunistring

define $(package)_preprocess_cmds
  ./autogen.sh
endef

define $(package)_set_vars
  $(package)_config_opts=--disable-shared --enable-static
endef

define $(package)_config_cmds
  $($(package)_autoconf)
endef

define $(package)_build_cmds
  $(MAKE)
endef

define $(package)_stage_cmds
  $(MAKE) DESTDIR=$($(package)_staging_dir) install
endef

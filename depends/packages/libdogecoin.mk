package=libdogecoin
$(package)_version=0.1.5-pre
$(package)_download_path=https://github.com/dogecoinfoundation/libdogecoin/archive/refs/tags
$(package)_file_name=v$($(package)_version).tar.gz
$(package)_sha256_hash=a10311d04cf345cb1f6770e82a71125da9c48e6e9d9365feef31612fe01ee9cb
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

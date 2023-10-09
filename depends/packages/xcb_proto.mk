package=xcb_proto
$(package)_version=1.15
$(package)_download_path=http://xcb.freedesktop.org/dist
$(package)_file_name=xcb-proto-$($(package)_version).tar.gz
$(package)_sha256_hash=0e434af76af722ef9b2dc21066da1cd11e5dd85fc1996d66228d090f9ae9b217

define $(package)_set_vars
  $(package)_config_opts=--disable-shared
  $(package)_config_opts_linux=--with-pic
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

define $(package)_postprocess_cmds
  find -name "*.pyc" -delete && \
  find -name "*.pyo" -delete
endef

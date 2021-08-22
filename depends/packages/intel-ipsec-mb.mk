package=intel-ipsec-mb
$(package)_version=1.0
$(package)_download_path=https://github.com/intel/intel-ipsec-mb/archive/refs/tags
$(package)_file_name=v$(package)_version.tar.gz
$(package)_sha256_hash=03501aea472d3c8fdf8f1f207816eefeaf5e4ebbdc71d88dcb26b2519841bb74

define $(package)_set_vars
$(package)_config_opts=--disable-shared
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

package=intel-ipsec-mb
$(package)_version=1.0
$(package)_download_path=https://github.com/intel/intel-ipsec-mb/archive/refs/tags
$(package)_file_name=v$($(package)_version).tar.gz
$(package)_sha256_hash=03501aea472d3c8fdf8f1f207816eefeaf5e4ebbdc71d88dcb26b2519841bb74

define $(package)_preprocess_cmds
  patch -p1 < $($(package)_patch_dir)/remove_digest_init.patch
endef

define $(package)_build_cmds
  $(MAKE)
endef

define $(package)_stage_cmds
  $(MAKE) DESTDIR=$($(package)_staging_dir) install
endef

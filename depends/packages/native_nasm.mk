package=native_nasm
$(package)_version=2.15.05
$(package)_download_path=http://nasm.us/pub/nasm/releasebuilds/$($(package)_version)
$(package)_file_name=nasm-$($(package)_version).tar.bz2
$(package)_sha256_hash=3c4b8339e5ab54b1bcb2316101f8985a5da50a3f9e504d43fa6f35668bee2fd0

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
  rm -rf share 
endef

package=intel-ipsec-mb
$(package)_version=1.2
$(package)_download_path=https://github.com/intel/intel-ipsec-mb/archive/refs/tags
$(package)_file_name=v$($(package)_version).tar.gz
$(package)_sha256_hash=f680b28369d02dc0978eb9b4bee6da9a132d66c666298e087a2b3e247548d99e
$(package)_patches=remove_digest_init.patch
$(package)_dependencies=native_nasm

define $(package)_set_vars
$(package)_build_opts_mingw32+=CC=$(host)-gcc
$(package)_build_opts+=LDFLAGS="-fstack-protector"
endef

define $(package)_preprocess_cmds
  patch -p1 < $($(package)_patch_dir)/remove_digest_init.patch
endef

define $(package)_stage_cmds
  $(MAKE) NASM=$(build_prefix)/bin/nasm PREFIX=$($(package)_staging_prefix_dir) SHARED=n NOLDCONFIG=y install $($(package)_build_opts)
endef

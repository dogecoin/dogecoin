package=native_mac_alias
$(package)_version=1.1.0
$(package)_download_path=https://github.com/al45tair/mac_alias/archive/
$(package)_download_file=v$($(package)_version).tar.gz
$(package)_file_name=mac_alias-$($(package)_version).tar.gz
$(package)_sha256_hash=b10cb44ecb64fc25283fae7a9cf365d2829377d84e37b9c21100aca8757509be
$(package)_install_libdir=$(build_prefix)/lib/python/dist-packages
$(package)_patches=python3.patch

define $(package)_preprocess_cmds
  patch -p1 < $($(package)_patch_dir)/python3.patch
endef

define $(package)_build_cmds
    python setup.py build
endef

define $(package)_stage_cmds
    mkdir -p $($(package)_install_libdir) && \
    python setup.py install --root=$($(package)_staging_dir) --prefix=$(build_prefix) --install-lib=$($(package)_install_libdir)
endef

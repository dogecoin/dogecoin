package=native_ds_store
$(package)_version=1.1.0
$(package)_download_path=https://github.com/al45tair/ds_store/archive/
$(package)_file_name=v$($(package)_version).tar.gz
$(package)_sha256_hash=a9f4c0755c6be7224ff7029e188dd262e830bb81e801424841db9eb0780ec8ed
$(package)_install_libdir=$(build_prefix)/lib/python/dist-packages
$(package)_dependencies=native_biplist

define $(package)_build_cmds
    python setup.py build
endef

define $(package)_stage_cmds
    mkdir -p $($(package)_install_libdir) && \
    python setup.py install --root=$($(package)_staging_dir) --prefix=$(build_prefix) --install-lib=$($(package)_install_libdir)
endef

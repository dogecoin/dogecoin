package=bdb
$(package)_version=5.1.29
$(package)_download_path=http://download.oracle.com/berkeley-db
$(package)_file_name=db-$($(package)_version).NC.tar.gz
$(package)_sha256_hash=08238e59736d1aacdd47cfb8e68684c695516c37f4fbe1b8267dde58dc3a576c
$(package)_build_subdir=build_unix

define $(package)_set_vars
$(package)_config_opts=--disable-shared --enable-cxx --disable-replication
$(package)_config_opts_mingw32=--enable-mingw
$(package)_config_opts_linux=--with-pic
endef

define $(package)_preprocess_cmds
  sed -i.old 's/__atomic_compare_exchange/__atomic_compare_exchange_db/' src/dbinc/atomic.h && \
  sed -i.old 's/WinIoCtl\.h/winioctl\.h/g' src/dbinc/win_db.h
endef

define $(package)_config_cmds
  ../dist/$($(package)_autoconf)
endef

define $(package)_build_cmds
  $(MAKE) libdb_cxx-5.1.a libdb-5.1.a
endef

define $(package)_stage_cmds
  $(MAKE) DESTDIR=$($(package)_staging_dir) install_lib install_include
endef

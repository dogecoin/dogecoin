package=native_openssl
$(package)_version=1.0.1l
$(package)_download_path=https://www.openssl.org/source
$(package)_file_name=openssl-$($(package)_version).tar.gz
$(package)_sha256_hash=b2cf4d48fe5d49f240c61c9e624193a6f232b5ed0baf010681e725963c40d1d4
define $(package)_set_vars
$(package)_build_config_opts= --prefix=$(build_prefix) no-zlib no-shared no-krb5C linux-generic32 -m32
endef

define $(package)_config_cmds
  ./Configure $($(package)_build_config_opts) &&\
  sed -i "s|engines apps test|engines|" Makefile
endef

define $(package)_build_cmds
  $(MAKE) -j1
endef

define $(package)_stage_cmds
  $(MAKE) INSTALL_PREFIX=$($(package)_staging_dir) -j1 install_sw
endef

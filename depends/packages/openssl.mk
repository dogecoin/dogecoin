package=openssl
$(package)_version=1.1.1
$(package)_version_suffix=s
$(package)_download_path=https://www.openssl.org/source/old/$($(package)_version)
$(package)_file_name=$(package)-$($(package)_version)$($(package)_version_suffix).tar.gz
$(package)_sha256_hash=c5ac01e760ee6ff0dab61d6b2bbd30146724d063eb322180c6f18a6f74e4b6aa
$(package)_patches=secure_getenv.patch getauxval.patch getentropy.patch

define $(package)_set_vars
$(package)_config_opts=--prefix=$(host_prefix) --openssldir=$(host_prefix)/etc/openssl
$(package)_config_opts+=no-camellia
$(package)_config_opts+=no-capieng
$(package)_config_opts+=no-cast
$(package)_config_opts+=no-comp
$(package)_config_opts+=no-dso
$(package)_config_opts+=no-dtls1
$(package)_config_opts+=no-ec_nistp_64_gcc_128
$(package)_config_opts+=no-gost
$(package)_config_opts+=no-heartbeats
$(package)_config_opts+=no-idea
$(package)_config_opts+=no-md2
$(package)_config_opts+=no-mdc2
$(package)_config_opts+=no-rc4
$(package)_config_opts+=no-rc5
$(package)_config_opts+=no-rdrand
$(package)_config_opts+=no-rfc3779
$(package)_config_opts+=no-sctp
$(package)_config_opts+=no-seed
$(package)_config_opts+=no-shared
$(package)_config_opts+=no-ssl-trace
$(package)_config_opts+=no-ssl2
$(package)_config_opts+=no-ssl3
$(package)_config_opts+=no-tests
$(package)_config_opts+=no-unit-test
$(package)_config_opts+=no-weak-ssl-ciphers
$(package)_config_opts+=no-whirlpool
$(package)_config_opts+=no-zlib
$(package)_config_opts+=no-zlib-dynamic
$(package)_config_opts+=$($(package)_cflags) $($(package)_cppflags)
$(package)_config_opts_linux=-fPIC -Wa,--noexecstack
$(package)_config_opts_x86_64_linux=linux-x86_64
$(package)_config_opts_i686_linux=linux-generic32
$(package)_config_opts_arm_linux=linux-generic32
$(package)_config_opts_aarch64_linux=linux-generic64
$(package)_config_opts_mipsel_linux=linux-generic32
$(package)_config_opts_mips_linux=linux-generic32
$(package)_config_opts_powerpc_linux=linux-generic32
$(package)_config_opts_x86_64_darwin=AR=$(host_prefix)/native/bin/x86_64-apple-darwin11-ar
$(package)_config_opts_x86_64_darwin+=RANLIB=$(host_prefix)/native/bin/x86_64-apple-darwin11-ranlib
$(package)_config_opts_x86_64_darwin+=darwin64-x86_64-cc

$(package)_config_opts_x86_64_mingw32=mingw64
$(package)_config_opts_i686_mingw32=mingw

ifneq (,$(findstring clang,$($(package)_cxx)))
$(package)_toolset_$(host_os)=clang
else
$(package)_toolset_$(host_os)=gcc
endif
endef

define $(package)_preprocess_cmds
  patch -p1 < $($(package)_patch_dir)/secure_getenv.patch && \
  patch -p1 < $($(package)_patch_dir)/getauxval.patch && \
  patch -p1 < $($(package)_patch_dir)/getentropy.patch
endef

define $(package)_config_cmds
  CC="$($(package)_cc)" \
  CXXFLAGS="$($(package)_ccflags)" \
  ./Configure $($(package)_config_opts)
endef

define $(package)_build_cmds
  sed -i.old 's/INSTALL_PROGRAMS=apps\/openssl/INSTALL_PROGRAMS=/g' Makefile && \
  $(MAKE) -j1 build_libs libcrypto.pc libssl.pc openssl.pc
endef

define $(package)_stage_cmds
  $(MAKE) DESTDIR=$($(package)_staging_dir) -j1 install_dev
endef

define $(package)_postprocess_cmds
  rm -rf share bin etc
endef

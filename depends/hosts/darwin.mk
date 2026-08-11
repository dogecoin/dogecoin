OSX_MIN_VERSION=10.8
OSX_SDK_VERSION=10.11
OSX_SDK=$(SDK_PATH)/MacOSX$(OSX_SDK_VERSION).sdk
LD64_VERSION=253.9

ifeq ($(strip $(FORCE_USE_SYSTEM_CLANG)),)
# gitian: use the clang that native_cctools downloads and installs into the
# depends prefix, along with the libc++ headers staged beside it.
clang_prog=$(build_prefix)/bin/clang
clangxx_prog=$(clang_prog)++

darwin_CC=$(clang_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) --sysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION)
darwin_CXX=$(clangxx_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) --sysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION) -stdlib=libc++
else
# guix: the downloaded clang is a prebuilt Ubuntu 14.04 binary whose ELF
# interpreter (/lib64/ld-linux-x86-64.so.2) does not exist inside the build
# container, so it cannot be executed. Use the clang on PATH instead, and spell
# out the system include search paths it should use:
#
#  * env -u ... clears the include and library variables, which are set for the
#    NATIVE toolchain so native_cctools and native_cdrkit can be built. Clang
#    honours them when targeting darwin too, which puts the native gcc's
#    libstdc++ ahead of libc++ and fails as "bits/c++config.h file not found",
#    those headers being incomplete without their target-specific bits/ dir.
#
#  * DARWIN_LIBCXX_PREFIX supplies the libc++ headers. gitian gets these from
#    the clang tarball; the SDK carries only the old c++/4.2.1 libstdc++ and
#    the packaged clang ships none.
#
#  * the two -Xclang -internal-externc-isystem flags append the clang resource
#    headers and the SDK's C headers *after* libc++, which is what libc++'s
#    #include_next <string.h> and friends require. Without them the build fails
#    as "'string.h' file not found" from inside libc++.
clang_prog=$(shell command -v clang)
clangxx_prog=$(shell command -v clang++)
clang_resource_dir=$(shell $(clangxx_prog) -print-resource-dir)

darwin_clear_env=env -u C_INCLUDE_PATH -u CPLUS_INCLUDE_PATH \
                     -u OBJC_INCLUDE_PATH -u OBJCPLUS_INCLUDE_PATH -u CPATH \
                     -u LIBRARY_PATH
darwin_sysinc=-Xclang -internal-externc-isystem$(clang_resource_dir)/include \
              -Xclang -internal-externc-isystem$(OSX_SDK)/usr/include

#  * -B$(build_prefix)/bin points clang at the cctools binaries staged there,
#    so it links with x86_64-apple-darwin18-ld rather than whichever ld is on
#    PATH. gitian does not need this because its clang lives in that same
#    directory and finds its siblings; the packaged clang does not, and GNU ld
#    then fails as "unrecognised emulation mode: acosx_version_min", having
#    read -macosx_version_min as -m acosx_version_min.
darwin_CC=$(darwin_clear_env) \
            $(clang_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) -isysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION) \
              -B$(build_prefix)/bin \
              $(darwin_sysinc)
darwin_CXX=$(darwin_clear_env) \
             $(clangxx_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) -isysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION) -stdlib=libc++ \
               -B$(build_prefix)/bin \
               -isystem $(DARWIN_LIBCXX_PREFIX)/include/c++/v1 \
               $(darwin_sysinc)
endif

darwin_CFLAGS=-pipe
darwin_CXXFLAGS=$(darwin_CFLAGS)

darwin_release_CFLAGS=-O2
darwin_release_CXXFLAGS=$(darwin_release_CFLAGS)

darwin_debug_CFLAGS=-O1
darwin_debug_CXXFLAGS=$(darwin_debug_CFLAGS)

darwin_native_toolchain=native_cctools

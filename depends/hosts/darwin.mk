OSX_MIN_VERSION=10.8
OSX_SDK_VERSION=10.11
OSX_SDK=$(SDK_PATH)/MacOSX$(OSX_SDK_VERSION).sdk
LD64_VERSION=253.9

ifeq ($(strip $(FORCE_USE_SYSTEM_CLANG)),)
# Use the clang that native_cctools downloads and installs into the depends
# prefix. This is what gitian does.
clang_prog=$(build_prefix)/bin/clang
clangxx_prog=$(clang_prog)++
else
# Use the clang already on PATH. Required under guix: the downloaded clang is a
# prebuilt Ubuntu 14.04 binary and its ELF interpreter (/lib64/ld-linux-x86-64.so.2)
# does not exist inside the build container, so it cannot be executed there.
clang_prog=$(shell command -v clang)
clangxx_prog=$(shell command -v clang++)
endif

darwin_CC=$(clang_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) --sysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION)
darwin_CXX=$(clangxx_prog) -target $(host) -mmacosx-version-min=$(OSX_MIN_VERSION) --sysroot $(OSX_SDK) -mlinker-version=$(LD64_VERSION) -stdlib=libc++

darwin_CFLAGS=-pipe
darwin_CXXFLAGS=$(darwin_CFLAGS)

darwin_release_CFLAGS=-O2
darwin_release_CXXFLAGS=$(darwin_release_CFLAGS)

darwin_debug_CFLAGS=-O1
darwin_debug_CXXFLAGS=$(darwin_debug_CFLAGS)

darwin_native_toolchain=native_cctools

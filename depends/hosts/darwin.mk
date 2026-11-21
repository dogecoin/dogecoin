# --- Target Versioning ---
# Minimum macOS version to support (Backward compatibility)
OSX_MIN_VERSION=10.8
# Target SDK version for the build environment
OSX_SDK_VERSION=10.11
# Resolved path to the MacOSX SDK
OSX_SDK=$(SDK_PATH)/MacOSX$(OSX_SDK_VERSION).sdk
# Specific linker version to ensure binary consistency
LD64_VERSION=253.9

# --- Toolchain Path Configuration ---
CLANG_PROG=$(build_prefix)/bin/clang
CLANGXX_PROG=$(CLANG_PROG)++

# --- Compiler Definitions (Targeting Darwin) ---
# -target: Defines the host architecture (e.g., x86_64-apple-darwin11)
# --sysroot: Points to the SDK headers and libraries
# -mlinker-version: Forces clang to behave like a specific Apple linker version
DARWIN_CC=$(CLANG_PROG) -target $(host) \
           -mmacosx-version-min=$(OSX_MIN_VERSION) \
           --sysroot $(OSX_SDK) \
           -mlinker-version=$(LD64_VERSION)

DARWIN_CXX=$(CLANGXX_PROG) -target $(host) \
            -mmacosx-version-min=$(OSX_MIN_VERSION) \
            --sysroot $(OSX_SDK) \
            -mlinker-version=$(LD64_VERSION) \
            -stdlib=libc++

# --- Compilation Flags ---
# -pipe: Use pipes instead of temporary files for speed
DARWIN_CFLAGS=-pipe -fPIC
DARWIN_CXXFLAGS=$(DARWIN_CFLAGS)

# Release optimizations (O2 for production performance)
DARWIN_RELEASE_CFLAGS=-O2 -DNDEBUG
DARWIN_RELEASE_CXXFLAGS=$(DARWIN_RELEASE_CFLAGS)

# Debug settings (O1 for balance between speed and debuggability)
DARWIN_DEBUG_CFLAGS=-O1 -g
DARWIN_DEBUG_CXXFLAGS=$(DARWIN_DEBUG_CFLAGS)

# Native toolchain reference (cctools: as, ld, lipo, etc.)
DARWIN_NATIVE_TOOLCHAIN=native_cctools

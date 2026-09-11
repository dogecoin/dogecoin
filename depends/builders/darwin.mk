# MARK: - Darwin Toolchain Base Definitions (Common Tools)
# Define the base paths for standard build tools using "xcrun -f" for reliability.
# We use := (simply expanded variable) to ensure the shell command is executed only once 
# when the Makefile is parsed, improving performance.

build_darwin_CC := $(shell xcrun -f clang)
build_darwin_CXX := $(shell xcrun -f clang++)
build_darwin_AR := $(shell xcrun -f ar)
build_darwin_RANLIB := $(shell xcrun -f ranlib)
build_darwin_STRIP := $(shell xcrun -f strip)
build_darwin_LIBTOOL := $(shell xcrun -f libtool) # Added for consistency
build_darwin_OTOOL := $(shell xcrun -f otool)
build_darwin_NM := $(shell xcrun -f nm)
build_darwin_INSTALL_NAME_TOOL := $(shell xcrun -f install_name_tool)
build_darwin_SHA256SUM = shasum -a 256
build_darwin_DOWNLOAD = curl --location --fail --connect-timeout $(DOWNLOAD_CONNECT_TIMEOUT) --retry $(DOWNLOAD_RETRIES) -o

# ---
# MARK: - Target-Specific Darwin Toolchain (Overrides)
# This section defines the toolchain for the 'darwin' host when building for 'darwin'.
# It uses the base tools defined above and applies specific flags for the target platform.
# Assumes OSX_MIN_VERSION is defined elsewhere.

# Compiler (CC)
# Appends macOS version minimum flag.
darwin_CC := $(build_darwin_CC) -mmacosx-version-min=$(OSX_MIN_VERSION)

# C++ Compiler (CXX)
# Appends macOS version minimum flag and enforces the modern libc++ standard library.
darwin_CXX := $(build_darwin_CXX) -mmacosx-version-min=$(OSX_MIN_VERSION) -stdlib=libc++

# Archiver and other utilities use the base definitions without modification.
darwin_AR := $(build_darwin_AR)
darwin_RANLIB := $(build_darwin_RANLIB)
darwin_STRIP := $(build_darwin_STRIP)
darwin_LIBTOOL := $(build_darwin_LIBTOOL)
darwin_OTOOL := $(build_darwin_OTOOL)
darwin_NM := $(build_darwin_NM)
darwin_INSTALL_NAME_TOOL := $(build_darwin_INSTALL_NAME_TOOL)

# Removed 'darwin_native_toolchain=' as it was unused/empty.

# Bootstrappable Dogecoin Core Builds

This directory builds Dogecoin Core release binaries with [GNU Guix][guix],
targeting the same platforms as the gitian descriptors in
[`contrib/gitian-descriptors`](../gitian-descriptors) and producing artifacts
with the same names and contents.

Where gitian pins a distribution image and freezes the clock with `faketime`,
Guix pins the package definitions themselves and builds each dependency from
source in an isolated container. Given the same commit, two people should get
byte-identical binaries.

## Requirements

* `guix`, with `guix-daemon` running. The daemon may be any version; the build
  runs everything through a pinned Guix regardless (see [The pin](#the-pin)).
* `curl`, `git`, `make`, `getent` on `PATH`.
* Roughly 6 GiB of free space per Linux host, 7.5 GiB per Windows host and
  0.5 GiB for macOS, plus space for the Guix store. `guix-build` refuses to
  start if the target filesystem is short.
* For macOS only, the SDK; see [macOS SDK](#macos-sdk).

## Usage

From the top of a **clean** worktree (`guix-build` refuses to run with modified
tracked files; untracked files are fine):

```sh
./contrib/guix/guix-build
```

That builds every supported host. To build a subset, set `HOSTS`:

```sh
env HOSTS='x86_64-linux-gnu x86_64-w64-mingw32' ./contrib/guix/guix-build
```

Builds take hours. Run them detached rather than in a foreground shell:

```sh
nohup setsid env HOSTS=x86_64-linux-gnu JOBS=8 \
    ./contrib/guix/guix-build > ~/guix-build.log 2>&1 < /dev/null &
tail -F ~/guix-build.log
```

### Supported hosts

| Host triple | Artifacts |
| --- | --- |
| `x86_64-linux-gnu` | `.tar.gz`, `-debug.tar.gz` |
| `i686-linux-gnu` | `.tar.gz`, `-debug.tar.gz` (named `i686-pc-linux-gnu`) |
| `arm-linux-gnueabihf` | `.tar.gz`, `-debug.tar.gz` |
| `aarch64-linux-gnu` | `.tar.gz`, `-debug.tar.gz` |
| `i686-w64-mingw32` | `win32.zip`, `win32-debug.zip`, `win32-setup-unsigned.exe` |
| `x86_64-w64-mingw32` | `win64.zip`, `win64-debug.zip`, `win64-setup-unsigned.exe` |
| `x86_64-apple-darwin18` | `osx64.tar.gz`, `osx-unsigned.dmg`, `osx-unsigned.tar.gz` |

Artifact names follow gitian, which is why the 32-bit Linux tarball is called
`i686-pc-linux-gnu` while the host triple is `i686-linux-gnu`, and why the
Windows and macOS artifacts use the short `win32`/`win64`/`osx64` names.

Qt, and therefore `dogecoin-qt`, is built only for the two x86 Linux hosts,
Windows and macOS; `depends/packages/packages.mk` defines no Qt packages for
arm or aarch64, so those tarballs contain four binaries rather than five. This
matches gitian.

### Output

Everything lands under `guix-build-<version>/output/`:

```
output/<host>/                     the artifacts, plus SHA256SUMS.part
output/dist-archive/               the source tarball the build used
```

`<version>` is the tag if `HEAD` is tagged, otherwise the short commit hash, so
off-tag builds are named after their commit.

## Environment variables

| Variable | Meaning |
| --- | --- |
| `HOSTS` | Space-separated host triples to build. Defaults to all of them. |
| `JOBS` | Parallelism. Defaults to `nproc`. |
| `SOURCES_PATH` | Cache for downloaded dependency sources. Shared across hosts and builds; worth setting to somewhere persistent. |
| `BASE_CACHE` | Cache for built dependency packages, keyed per host. |
| `SDK_PATH` | Where macOS SDKs live. Defaults to `depends/SDKs`. |
| `GUIX_CHANNEL_URL` | Where to fetch the pinned Guix from. See [The pin](#the-pin). |
| `SUBSTITUTE_URLS` | Additional substitute servers to trust. |
| `V` | Set to anything for verbose builds. |
| `FORCE_VERSION` | Override the version string used in artifact names. |
| `ADDITIONAL_GUIX_COMMON_FLAGS` | Passed to every `guix` invocation. |
| `ADDITIONAL_GUIX_TIMEMACHINE_FLAGS` | Passed to `guix time-machine`. |
| `ADDITIONAL_GUIX_ENVIRONMENT_FLAGS` | Passed to `guix environment`. |

`SOURCES_PATH`, `BASE_CACHE`, `SDK_PATH`, `OUTDIR_BASE` and `PROFILES_BASE` are
treated as precious: [`guix-clean`](guix-clean) will not remove them.

## The pin

Builds run under Guix commit
`8e2f32cee982d42a79e53fc1e9aa7b8ff0514714`, which is the `v1.4.0` tag, via
`guix time-machine`. Pinning the Guix commit rather than package versions is
what makes the result stable over time: package *definitions* change too.

The first run fetches and authenticates that commit, which takes a while and
verifies tens of thousands of signatures. It is a one-time cost.

If fetching from Savannah is slow or hits a libgit2 redirect problem, point
`GUIX_CHANNEL_URL` at a local clone:

```sh
git clone --mirror https://git.savannah.gnu.org/git/guix.git ~/guix-repo.git
env GUIX_CHANNEL_URL=file://$HOME/guix-repo.git ./contrib/guix/guix-build
```

The commit is authenticated against the channel introduction either way, so a
local mirror does not weaken the check.

## macOS SDK

macOS builds need `MacOSX10.11.sdk`, which Apple does not permit redistributing.
`guix-build` checks for it before starting and aborts if it is absent.

The project mirrors the extracted SDK:

```sh
mkdir -p depends/SDKs
curl -O https://depends.dogecoincore.org/MacOSX10.11.sdk.tar.gz
# sha256: bec9d089ebf2e2dd59b1a811a38ec78ebd5da18cbbcd6ab39d1e59f64ac5033f
tar -C depends/SDKs -xf MacOSX10.11.sdk.tar.gz
```

[`doc/README_osx.md`](../../doc/README_osx.md) describes extracting it from
Xcode yourself, which is only necessary if the mirror is unavailable.

## Differences from the gitian build

The two systems produce the same artifacts, but a few mechanisms differ because
Guix cannot do what gitian does:

* **The macOS compiler.** `depends/packages/native_cctools.mk` downloads a
  prebuilt clang for gitian. That binary cannot execute inside the Guix
  container, which has no `/lib64` and so cannot supply its ELF interpreter.
  Setting `FORCE_USE_SYSTEM_CLANG=1`, which `libexec/build.sh` does, makes
  depends use the packaged clang instead: the same LLVM 6.0.1 release, with
  libc++ from the `libcxx-6` package.
* **Timestamps.** gitian wraps tools in `faketime`. Guix sets
  `SOURCE_DATE_EPOCH` from the commit date instead, and
  `depends/patches/native_cdrkit/cdrkit-source-date-epoch.patch` teaches
  `genisoimage` to honour it so the `.dmg` reproduces.
* **`check-symbols` on macOS** is not run, matching `gitian-osx.yml`. The
  Mach-O checks in `contrib/devtools/symbol-check.py` assert a deployment
  target and SDK this tree does not build against.
* **The combined `-win-unsigned.tar.gz`** that gitian assembles from both
  Windows installers is not produced: each host builds in its own container and
  cannot see the other's output. The per-host
  `-win{32,64}-setup-unsigned.exe` are produced as usual.

## Cleaning

```sh
./contrib/guix/guix-clean
```

removes build trees and outputs while keeping the caches, SDKs and garbage
collector roots. Removing `BASE_CACHE` or `SOURCES_PATH` by hand only costs
rebuild time.

## When something fails

The build stops at the first failing package. Guix keeps the failed build tree
and prints its location; the per-derivation log has the detail:

```sh
guix log /gnu/store/<hash>-<package>.drv
ls -t /var/log/guix/drvs/*/*.bz2 | head
```

To test a fix without repeating the whole build, realize the single derivation:

```sh
guix time-machine --commit=8e2f32cee982d42a79e53fc1e9aa7b8ff0514714 \
    -- build /gnu/store/<hash>-<package>.drv
```

Dependency build trees under `depends/work/build/<host>/<package>` persist
between runs, so removing one forces just that package to rebuild.

[guix]: https://guix.gnu.org

GitHub Actions CI
=================

Dogecoin Core uses [GitHub Actions](https://docs.github.com/en/actions) to
automate building and testing on every pull request and push to master.

Workflow files are located in `.github/workflows/`:

- **ci.yml** — Builds and tests the project across a matrix of platforms
  (Linux, Windows, macOS, ARM) using the dependency generator in the
  [depends dir](/depends). This mirrors the Gitian release build environment
  as closely as possible.

- **linter.yml** — Runs lightweight checks that do not require a full build:
  - Translation file consistency (`contrib/devtools/check-translations.py`)
  - Vendored subtree integrity (`contrib/devtools/git-subtree-check.sh`)
  - Trailing whitespace in C++ source files
  - C++ formatting against `src/.clang-format` for changed files

- **codeql-analysis.yml** — Runs GitHub's CodeQL static analysis for
  security vulnerability detection.

Build matrix
------------

The CI matrix in `ci.yml` tests a wide range of configurations to catch
platform-specific failures early. All builds use the dependency generator
rather than system packages to ensure reproducibility.

No local state is maintained between runs. Dependencies are cached by GitHub
Actions and invalidated automatically when the dependency generator changes.

Formatting
----------

The project uses `clang-format` with the configuration in `src/.clang-format`.
Before submitting a pull request, you can auto-format your changes locally with:

```
git diff -U0 HEAD~1.. | ./contrib/devtools/clang-format-diff.py -p1 -i -v
```

The `linter.yml` workflow enforces this on every pull request automatically.

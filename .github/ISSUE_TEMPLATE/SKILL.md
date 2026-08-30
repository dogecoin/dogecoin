---
name: dogecoin-core-dev
description: Working knowledge of the Tsukimarf/dogecoin repository (a fork of Dogecoin Core, itself derived from Bitcoin Core) — build system, directory layout, branch/versioning strategy, and contribution conventions. Use this skill whenever the user asks to build, patch, review, or open issues/PRs against this repo, work with its C++ node/wallet code, RPC interface, or anything under .github/ (issue templates, workflows). Also use when comparing this fork against upstream dogecoin/dogecoin.
---

# Dogecoin Core Dev (Tsukimarf/dogecoin)

Reference for working in `Tsukimarf/dogecoin` — a fork of `dogecoin/dogecoin`
(Scrypt PoW, adapted from Bitcoin Core). Use this to orient quickly instead of
re-deriving repo structure from scratch each session.

## Fetching repo content

- GitHub's HTML pages block automated fetch (robots disallowed). Always use
  the GitHub API or raw content instead:
  - Directory listing: `https://api.github.com/repos/Tsukimarf/dogecoin/contents/<path>?ref=master`
  - File content: `https://raw.githubusercontent.com/Tsukimarf/dogecoin/master/<path>`
- The API is unauthenticated here and rate-limited (~60 req/hr). Prefer
  `raw.githubusercontent.com` for file bodies; only hit the `api.github.com`
  contents endpoint when you need a directory listing.

## Repo facts

- **Upstream**: `dogecoin/dogecoin`. Community links (Reddit r/dogeducation,
  Discord) point to the upstream community, not a fork-specific one — reuse
  them in any templates/docs unless the user says otherwise.
- **`master`** is explicitly unstable since Aug 2024 — don't build production
  binaries from it without checking out a tagged version first.
- **Versioning**: `major.minor.patch`.
- **Branches**: `master` (unstable/dev), `<version>-maint` (stable, patched by
  maintainers via cherry-pick), `<version>-dev` (upcoming release, unstable),
  archive branches (frozen). **PRs target `master`.**
- **Ports**: P2P 22556 / RPC 22555 (mainnet), 44556 / 44555 (testnet), 18444 /
  18332 (regtest). RPC ports should never be exposed publicly.
- **License**: MIT (see `COPYING`).

## Directory layout (inherited from Bitcoin/Dogecoin Core)

- `src/` — C++ node, wallet, RPC, consensus code. This is the core of most
  code-review or patch requests.
- `doc/` — `getting-started.md`, `fee-recommendation.md`, `FAQ.md`,
  `doc/intl/README.md` (translated READMEs).
- `INSTALL.md` — build/installation guide; check here first for build-flag or
  dependency questions rather than assuming Bitcoin Core's flags apply as-is.
- `CONTRIBUTING.md` — contribution process.
- `.github/ISSUE_TEMPLATE/` — `bug_report.md`, `feature_request.md`, and
  `config.yml` (chooser config; see below).
- `COPYING` — MIT license text.

## Issue templates

`bug_report.md` and `feature_request.md` follow a consistent shape:
YAML frontmatter (`name`, `about`, `title`), an HTML-comment scope/community
disclaimer, then `#`-titled sections with HTML-comment field hints (no bold
required-field markers, no GitHub form YAML — these are classic Markdown
issue templates, not the newer `.yml` form schema). Match that shape exactly
when adding templates: frontmatter + disclaimer comment + `# Title` + bolded
field labels with comment hints underneath.

`config.yml` controls the chooser: set `blank_issues_enabled: false` to force
template use, and route general/wallet-recovery questions to
`contact_links` (Reddit, Discord) rather than the issue tracker.

## When asked to build

1. Check `INSTALL.md` and `doc/` in the actual repo state before assuming a
   Bitcoin-Core-style `./autogen.sh && ./configure && make` flow — Dogecoin
   Core forks have historically diverged on dependency versions (Berkeley DB,
   Boost) and build flags.
2. Confirm which branch/tag the user wants — default to a tagged release, not
   `master`, if the goal is a working binary rather than active development.

## When asked for PRs/issues

- Target `master`.
- Use the existing template shape (see above) rather than inventing new
  section headers.
- Point general questions (not bugs/features) to Reddit/Discord per
  `config.yml`, not to a new issue.
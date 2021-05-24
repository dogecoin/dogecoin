---
name: Bug report
about: Report a bug to help us improve
title: "[BUG] - "
labels: bug
assignees: ''

---

**Internal/External**
*Internal*   if an IOHK staff member.
*External*   otherwise.

**Area**
*Token Locking*    Related to Token Locking (Allegra).
*Native Tokens*    Related to Native Tokens (Mary).
*Other*   Any other topic (Delegation, Ranking, ...).

**Summary**
A clear and specific description of what the bug is.

**Steps to reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**System info (please complete the following information):**
- OS Name: [e.g. Ubuntu]
- OS Version [e.g. 20.04]
- Node version (output of `cardano-node --version`)
- CLI version  (output of `cardano-cli --version`)

If the git revision is reported to be `0000000000000000000000000000000000000000`, please use `cabal build` instead of `cabal install` to build the binaries because `cabal install` does not record the git revision in the binaries.

**Screenshots and attachments**
- If applicable, add screenshots, config files and/or logs to help explain the problem.

**Additional context**
Add any other context about the problem here.

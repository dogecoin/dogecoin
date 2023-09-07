# Contributing to Dogecoin Core

Dogecoin Core is open source software, and we would welcome contributions
which improve the state of the software. For those wanting to discuss changes,
or look for work that needs doing, please see:

* [Help requests](https://github.com/dogecoin/dogecoin/labels/help%20wanted)
* [Projects](https://github.com/dogecoin/dogecoin/projects)
* [Dogecoindev on reddit](https://www.reddit.com/r/dogecoindev/)

## Branch Strategy

Dogecoin Core's default branch is intentionally a stable release, so that anyone
downloading the code and compiling it gets a stable release. Active development
occurs on branches named after the version they are targeting, for example the
1.14.4 branch is named `1.14.4-dev`. When raising PRs, please raise against the
relevant development branch and **not** against the `master` branch.

## Contributor Workflow

The codebase is maintained using the "contributor workflow" where everyone
without exception contributes patch proposals using "pull requests". This
facilitates social contribution, easy testing and peer review.

To contribute a patch, the workflow is as follows:

  - Fork the repository in GitHub, and clone it your development machine.
  - Create a topic branch from the relevant development branch.
  - Commit changes to the branch.
  - Test your changes, which **must** include the unit and RPC tests passing.
  - Push topic branch to your copy of the repository.
  - Raise a Pull Request via GitHub.

The coding conventions in the [developer notes](doc/developer-notes.md) must be
adhered to.

In general [commits should be atomic](https://en.wikipedia.org/wiki/Atomic_commit#Atomic_commit_convention)
and diffs should be easy to read. For this reason do not mix any formatting
fixes or code moves with actual code changes.

Commit messages should be verbose by default consisting of a short subject line
(50 chars max), a blank line and detailed explanatory text as separate
paragraph(s); unless the title alone is self-explanatory (like "Corrected typo
in init.cpp") then a single title line is sufficient. Commit messages should be
helpful to people reading your code in the future, so explain the reasoning for
your decisions. Further explanation [here](http://chris.beams.io/posts/git-commit/).

Please refer to the [Git manual](https://git-scm.com/doc) for more information
about Git.

The body of the pull request should contain enough description about what the
patch does together with any justification/reasoning. You should include
references to any discussions (for example other tickets or mailing list
discussions). At this stage one should expect comments and review from other
contributors. You can add more commits to your pull request by committing them
locally and pushing to your fork until you have satisfied feedback.


## Squashing Commits

If your pull request is accepted for merging, you may be asked by a maintainer
to squash and or [rebase](https://git-scm.com/docs/git-rebase) your commits
before it will be merged. The basic squashing workflow is shown below.

    git checkout your_branch_name
    git rebase -i HEAD~n
    # n is normally the number of commits in the pull
    # set commits from 'pick' to 'squash', save and quit
    # on the next screen, edit/refine commit messages
    # save and quit
    git push -f # (force push to GitHub)

If you have problems with squashing (or other workflows with `git`), you can
alternatively enable "Allow edits from maintainers" in the right GitHub
sidebar and ask for help in the pull request.

Please refrain from creating several pull requests for the same change.
Use the pull request that is already open (or was created earlier) to amend
changes. This preserves the discussion and review that happened earlier for
the respective change set.

The length of time required for peer review is unpredictable and will vary
between pull requests.


## Pull Request Philosophy

Pull Requests should always be focused. For example, a pull request could add a
feature, fix a bug, or refactor code; but not a mixture. Please avoid submitting
pull requests that attempt to do too much, are overly large, or overly complex
as this makes review difficult.


### Features

When adding a new feature, thought must be given to the long term technical debt
and maintenance that feature may require after inclusion. Before proposing a new
feature that will require maintenance, please consider if you are willing to
maintain it (including bug fixing). If features get orphaned with no maintainer
in the future, they may be removed.


### Refactoring

Dogecoin Core is a direct fork of Bitcoin Core and therefore benefits from as
little refactoring as possible on code that is created upstream. If you see any
structural issues with upstream code, please propose these fixes for
[bitcoin/bitcoin](https://github.com/bitcoin/bitcoin) and future Dogecoin Core
releases will automatically benefit from these.

When refactoring Dogecoin-specific code, please keep refactoring requests short,
low complexity and easy to verify.

### Experimental features and optimizations

In some cases where a pull request introduces a new feature or optimization,
reviewers or maintainers can request the feature to be introduced as an
experimental-only feature, meaning that the feature will not be released in
provided binaries but will be available for self-compilation for those who
wish to test it. Experimental features are still expected to be complete and
the process to be followed for all contribution guidelines as outlined in this
document.

For more information, see the
[experimental feature documentation](./doc/experiments.md)


## "Decision Making" Process

The following applies to code changes to Dogecoin Core, and is not to be
confused with overall Dogecoin Network Protocol consensus changes. All consensus
changes **must** be ratified by miners; a proposal to implement protocol changes
does not guarantee activation on the mainnet, not even when a binary gets
released by maintainers.

Whether a pull request is merged into Dogecoin Core rests with the repository
maintainers.

Maintainers will take into consideration if a patch is in line with the general
principles of Dogecoin; meets the minimum standards for inclusion; and will
take into account the consensus among frequent contributors.

In general, all pull requests must:

  - have a clear use case, fix a demonstrable bug or serve the greater good of
    Dogecoin;
  - be peer reviewed;
  - have unit tests and functional tests;
  - follow code style guidelines;
  - not break the existing test suite;
  - where bugs are fixed, where possible, there should be unit tests
    demonstrating the bug and also proving the fix. This helps prevent
    regressions.

The following patch types are expected to have significant discussion before
approval and merge:

- Consensus rule changes (through softfork or otherwise)
- Policy changes
- Maturing experimental features into production

While each case will be different, one should be prepared to expend more time
and effort than for other kinds of patches because of increased peer review
and consensus building requirements.


### Peer Review

Anyone may participate in peer review which is expressed by comments in the pull
request. Typically, reviewers will review the code for obvious errors, as well as
test out the patch set and opine on the technical merits of the patch.
Repository maintainers take into account the peer review when determining if
there is consensus to merge a pull request.

Maintainers reserve the right to weigh the opinions of peer reviewers
using common sense judgement and also may weight based on meritocracy: Those
that have demonstrated a deeper commitment and understanding towards Dogecoin
(over time) or have clear domain expertise may naturally have more weight, as
one would expect in all walks of life.

Where a patch set proposes to change the Dogecoin consensus, it must have been
discussed extensively, be accompanied by widely discussed documentation and have
a generally widely perceived technical consensus of being a worthwhile change,
based on the judgement of the maintainers.


### Merging pull requests

Maintainers can only merge pull requests after any maintainer, other than the
author of a pull request, has approved the code according to the decision
making process outlined above.

Maintainers must keep pull requests open for at least 24 hours after approval
to merge is given, to allow anyone to voice a concern that may have been missed
in review, or request more time to investigate a suspected issue. If a situation
arises where more time has been requested but cannot be granted, at maintainer
discretion, a new issue or pull request should be opened to address the defect
or discuss improved alternatives. Requests for time and maintainer decision
making are expected to be clearly documented on the pull request discussion on
Github.

Maintenance tasks and time-critical patches can be exempted from this rule if
these are clearly marked as such, at maintainer discretion.

## Copyright

By contributing to this repository, you agree to license your work under the 
MIT license unless specified otherwise in `contrib/debian/copyright` or at 
the top of the file itself. Any work contributed where you are not the original 
author must contain its license header with the original author(s) and source.

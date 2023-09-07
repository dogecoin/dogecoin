Experimental features
----------------------

Features can be marked as experimental when the functionality is desired but
further analysis, testing or follow-up work is needed to make sure that the
feature is fully ready for rollout to every node in the network. This can
specifically help the introduction of performance updates or other lower-level
improvements where positive and/or negative effects may take a longer time to
test. The PR benefits from being merged because this makes it easier for
testers to pick and choose sets of features to experiment with in their custom
built Dogecoin Core deployments.

## Enabling experimental features

Experiments can be enabled by passing `--enable-experimental` AND the desired
experimental feature to `./configure`:

### Current experiments

| Feature     | Configure flag         | Description
| :---------- | :--------------------- | :-----
| Scrypt SSE2 | `--enable-scrypt-sse2` | SSE2 asm for Scrypt functions
| SHA ARMv8   | `--with-armv8-crypto`  | SHA1/256 intrinsics for ARMv8-crypto capable processors
| SHA ARMv82  | `--with-armv82-crypto` | SHA512 intrinsics for ARMv8.2-crypto capable processors
| SHA AVX2    | `--with-intel-avx2`    | SHA1/256/512 intrinsics using Intel AVX2 extensions, depends on intel-ipsec-mb

## Requirements

1. An experimental feature shall be controlled by a configuration flag inside
   `configure.ac` that explicitly enables the feature.
2. The feature shall by default be disabled.
3. The feature shall call the `DOGECOIN_REQUIRE_EXPERIMENTAL` macro in
   `configure.ac`, to enforce that `--enable-experimental` was passed.
4. All code blocks related to the feature shall be guarded by a preprocessor
   check on the configuration flag, to prevent leaking code into releases.
5. Inside each experimental code block, a call shall be made to the
   `DOGECOIN_REQUIRE_EXPERIMENTAL` macro from `support/experimental.h`, to
   clearly mark the code as experimental and ease review and troubleshooting.
6. Only when an experimental feature matures into production shall the above
   requirements be voided.

## Lifecycle

Experimental features move through different stages. By default, each experiment
is "maturing", which means no decision is made whether the feature will be
included in a release, and the feature does not have to be ready for inclusion
at that time. This gives interested parties time to test, suggest changes and
form an opinion about the benefits of the feature without this blocking release.

Once there is consensus that a feature is beneficial, it needs to be prepared
for generic inclusion. This means that while it retains experimental status,
the feature is tested to not break any builds on known platforms, after which
it can be matured into production code in a separate PR.

It is also possible that an experiment is abandoned instead of included in a
release, for example when it lacks a maintainer or when another implementation
is proposed for the same functionality that is more desirable.

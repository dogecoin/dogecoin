# Coding Standards

> **Created by Grok (xAI)**  
> This document was produced by analyzing the provided C++ source (Dogecoin Core / Bitcoin Core lineage) and extracting the actual naming, formatting, and structural conventions. It has been updated to target **C++14 or later** and to recommend modern best practices (`nullptr`, `auto`, range-for, etc.) while preserving the existing style.

This document describes the coding standards observed in this codebase (Bitcoin Core / Dogecoin Core style). Future changes should follow these conventions so that generated or modified code remains consistent with the existing style.

## Copyright and File Headers

Every source file begins with a copyright header in this exact form:

```cpp
// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2016 The Bitcoin Core developers
// Copyright (c) 2021-2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.
```

Update the Dogecoin Core years when making substantial new contributions. Preserve existing copyright lines.

## Includes

- Use double quotes for project headers: `#include "chain.h"`
- Place the `using namespace std;` directive after includes when the file makes heavy use of the standard library.

## Naming Conventions

| Kind                  | Style                          | Examples                                      |
|-----------------------|--------------------------------|-----------------------------------------------|
| Classes / types       | `C` prefix + PascalCase        | `CBlockIndex`, `CChain`, `CBlockHeader`       |
| Methods               | PascalCase                     | `GetBlockHeader`, `SetTip`, `GetAncestor`     |
| Member variables      | Mixed (often type-prefixed)    | `nVersion`, `nHeight`, `pprev`, `pskip`, `vChain`, `hashMerkleRoot` |
| Local variables       | camelCase or type-prefixed     | `pindex`, `nStep`, `vHave`, `heightWalk`      |
| Parameters            | camelCase                      | `consensusParams`, `fCheckPOW`, `pindex`      |
| Boolean flags         | `f` prefix                     | `fCheckPOW`, `fNegative`, `fOverflow`         |
| Integers / heights    | `n` prefix                     | `nVersion`, `nTime`, `nBits`, `nNonce`, `nHeight` |
| Pointers              | `p` prefix                     | `pprev`, `pindex`, `pskip`                    |
| Vectors               | `v` prefix                     | `vChain`, `vHave`                             |
| Hashes                | `hash` prefix                  | `hashPrevBlock`, `hashMerkleRoot`             |

## Formatting

- Indent with **4 spaces**. Never use tabs.
- Opening brace of a function, method, or control structure goes on the **same line**:

  ```cpp
  void CChain::SetTip(CBlockIndex *pindex) {
      if (pindex == nullptr) {
          vChain.clear();
          return;
      }
      // ...
  }
  ```

- Pointer and reference declarators have a space before the `*` or `&`:

  ```cpp
  CBlockIndex *pindex
  const CBlockIndex *pindex
  ```

- Align consecutive assignments with extra spaces when it improves readability:

  ```cpp
  block.nVersion       = nVersion;
  block.hashMerkleRoot = hashMerkleRoot;
  block.nTime          = nTime;
  block.nBits          = nBits;
  block.nNonce         = nNonce;
  ```

- Put a space after keywords (`if`, `while`, `for`) and around binary operators.
- Prefer early returns over deep nesting.

## Comments

- Use `//` for single-line comments and `/* ... */` for multi-line explanatory blocks.
- Comments should explain **why**, especially for non-obvious algorithms, edge cases, or performance choices.
- Examples of expected comment density:

  ```cpp
  /* Moved here from the header, because we need auxpow and the logic
     becomes more involved.  */
  ```

  ```cpp
  // We need to compute 2**256 / (bnTarget+1), but we can't represent 2**256
  // as it's too large for a arith_uint256. However, as 2**256 is at least as large
  // as bnTarget+1, it is equal to ((2**256 - bnTarget - 1) / (bnTarget+1)) + 1,
  // or ~bnTarget / (nTarget+1) + 1.
  ```

- Document complex helper logic (skip heights, proof calculations, locator construction, etc.) inline.

## Const Correctness

- Mark methods `const` whenever they do not modify the object.
- Prefer `const` parameters and return types where appropriate.
- Provide both const and non-const overloads when needed (see `GetAncestor`).

## Pointers and Null Checks

- Prefer `nullptr` over `NULL` (C++11 and later).
- Explicitly check pointers before dereferencing:

  ```cpp
  if (pindex == nullptr) {
      return nullptr;
  }
  ```

- Use assertions for invariants that must hold:

  ```cpp
  assert(pindexWalk->pprev);
  ```

## Small Helpers

- Mark pure, small utility functions `static inline` and keep them near their use site or at file scope:

  ```cpp
  int static inline InvertLowestOne(int n) { return n & (n - 1); }
  ```

## Arithmetic and Overflow Safety

- Prefer `arith_uint256` for large integer work.
- Explicitly document overflow or precision limitations.
- Clamp or saturate results when values can exceed `int64_t` range (see `GetBlockProofEquivalentTime`).

## Control Flow and Algorithms

- Prefer clear, well-commented algorithms over clever one-liners when the logic is non-trivial (skiplist ancestor walking, exponential locator steps, etc.).
- Use standard algorithms (`std::lower_bound`, lambdas) when they improve clarity.
- Keep related logic together; move complex methods out of headers when they require additional dependencies.

## Modern C++ (C++14+)

Target at least **C++14**. Prefer modern idioms where they improve safety and clarity without conflicting with the existing style:

- Use `nullptr` instead of `NULL`.
- Prefer `auto` for complex types when the type is obvious from the initializer.
- Use range-based `for` loops when appropriate.
- Prefer `std::unique_ptr` / `std::shared_ptr` for new ownership; do not introduce them into existing raw-pointer APIs without a broader refactor.
- Use `override` and `final` on virtual functions.
- Prefer braced initialization `{}` for most objects.
- Use `constexpr` where values are known at compile time.
- Avoid `using namespace std;` in headers. It is acceptable in `.cpp` files when the file already uses it heavily, but prefer explicit `std::` in new code.

## General Principles

- Match the surrounding style for naming, formatting, and structure.
- Modernize language features (`nullptr`, `auto`, range-for, etc.) when touching code, provided it stays consistent with the rest of the file.
- Preserve existing comments and copyright headers.
- Favor readability, const-correctness, and explicitness over brevity, especially in consensus-critical code.
- When in doubt, copy the pattern from the nearest similar function, then apply the modern C++ rules above.

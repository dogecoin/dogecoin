# PAT Coding Standards

## Overview

The Paw Aggregation Technique (PAT) project adheres to Dogecoin Core-inspired coding standards, bridging Python prototyping rigor with production C++ requirements. This ensures maintainable, secure, and portable code suitable for cryptocurrency infrastructure.

## Python Prototype Standards

### Type Safety & Documentation
- **Type Hints**: All functions use `typing` module annotations (e.g., `List[bytes]`, `Optional[AggregationStrategy]`)
- **PEP-257 Docstrings**: Google-style format with `Args:`, `Returns:`, `Raises:` sections
- **Descriptive Naming**: camelCase for classes (`PatAggregator`), snake_case for functions/variables (`aggregate_signatures`)

### Security & Cryptography
- **Secure Primitives**: Use `hashlib` and `cryptography` library over raw operations
- **Constant-Time Operations**: Avoid timing leaks in cryptographic comparisons
- **Entropy Handling**: `os.urandom()` for randomness, with clear security notes
- **Input Validation**: Comprehensive bounds checking with custom `PatError` exceptions

### Thread Safety & Memory Management
- **No Mutable Globals**: Thread-safe design with no shared state between instances
- **Context Managers**: Safe resource handling where applicable
- **Immutable Constants**: Class-level constants for configuration data

### Testing & Quality Assurance
- **Unit Tests**: Comprehensive `unittest` suite (see `pat/src/tests/`)
- **Coverage**: 80%+ coverage on core functionality
- **Error Scenarios**: Edge cases, invalid inputs, exception handling
- **Mocking**: `unittest.mock` for external dependencies

## C++ Port Standards (Future Production)

### Core Libraries & Dependencies
- **Boost.Crypto**: For cryptographic operations (RSA, ECDSA, hash functions)
- **OpenSSL**: Fallback for cryptographic primitives
- **Dilithium**: NIST-standard post-quantum signature library
- **Boost.Test**: Unit testing framework

### Memory Management & Safety
- **Smart Pointers**: `std::unique_ptr` and `std::shared_ptr` for automatic resource management
- **RAII Pattern**: Resource acquisition is initialization
- **No Raw Pointers**: Except in performance-critical sections with documented ownership

### Build System Integration
- **Autotools**: Integration with Dogecoin Core's `autogen.sh`/`configure`/`make` system
- **CMake Support**: Optional CMake build for cross-platform compatibility
- **Dependency Management**: System packages via `apt`/`brew` or vendored copies

### Testing & Validation
- **Google Test**: Comprehensive unit and integration tests
- **Fuzz Testing**: libFuzzer integration for robustness
- **Static Analysis**: Clang Static Analyzer, Coverity Scan
- **Continuous Integration**: GitHub Actions with multiple compilers/platforms

## Development Workflow

### Code Organization
```
src/pat/                    # Core implementation
├── pat_benchmark.py       # Main aggregator implementation
├── extensions/            # Advanced features (quantum, proofs, multi-chain)
└── tests/                 # Unit test suite

docs/                      # Documentation
├── academic_paper.tex     # Research paper
├── CODING_STANDARDS.md    # This file
└── README.md             # Project overview
```

### Quality Gates
- **Linting**: `pylint`/`flake8` for Python, `clang-format` for C++
- **Type Checking**: `mypy` for Python static analysis
- **Security Review**: Manual and automated security audits
- **Performance Benchmarks**: Regression testing against performance baselines

## Security Considerations

### Cryptographic Security
- **Post-Quantum Readiness**: Dilithium ML-DSA-44 signatures for quantum resistance
- **Side-Channel Resistance**: Constant-time implementations where feasible
- **Key Management**: Secure key generation and storage patterns

### Implementation Security
- **Input Sanitization**: All external inputs validated and bounded
- **Exception Safety**: Strong exception guarantees in critical paths
- **Memory Safety**: Bounds checking and overflow protection

## References

- [Dogecoin Core Developer Documentation](https://github.com/dogecoin/dogecoin/blob/master/doc/developer-notes.md)
- [Bitcoin Core Coding Style](https://github.com/bitcoin/bitcoin/blob/master/doc/developer-notes.md)
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [Python Enhancement Proposals](https://peps.python.org/pep-0008/)

## Contributing

All contributions must adhere to these standards. New code should include:
- Type hints and comprehensive docstrings
- Unit tests with 80%+ coverage
- Security review for cryptographic operations
- Documentation updates

For questions about these standards, refer to the Dogecoin Core development community or open an issue in this repository.

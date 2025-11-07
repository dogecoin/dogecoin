# PAT Unit Tests

Comprehensive unit test suite for the Paw Aggregation Technique (PAT) implementation, aligned with Dogecoin Core's unit testing ethos (equivalent to `make check`).

## Overview

This test suite provides thorough coverage of the PAT aggregation functionality with a focus on:

- **Security**: Tamper detection, cryptographic validation, entropy handling
- **Performance**: Compression ratios, scaling behavior, memory efficiency
- **Reliability**: Edge cases, error handling, input validation
- **Compatibility**: All aggregation strategies, cross-strategy consistency

## Test Coverage

- **PatAggregator Class**: 80%+ coverage across all public methods
- **Aggregation Strategies**: All enums (threshold, merkle_batch, logarithmic, stacked_multi)
- **Security Features**: Tamper detection, verification methods, entropy validation
- **Edge Cases**: Empty inputs, invalid strategies, boundary conditions

## Running Tests

### Basic Execution
```bash
# From pat/src/ directory
python -m unittest discover tests/
python -m unittest tests.test_pat_aggregator

# Specific test class
python -m unittest tests.test_pat_aggregator.TestPatAggregator

# Specific test method
python -m unittest tests.test_pat_aggregator.TestPatAggregator.test_logarithmic_compression_large
```

### With Coverage (Recommended)
```bash
# Install coverage tools
pip install coverage pytest pytest-cov

# Run with coverage report
python -m pytest tests/ --cov=../pat_benchmark.py --cov-report=html --cov-report=term

# View HTML report
open htmlcov/index.html
```

### Dogecoin Core Style
```bash
# Equivalent to 'make check' in Dogecoin Core
python -m unittest discover tests/ -v

# With XML output for CI/CD
python -m pytest tests/ --junitxml=test-results.xml
```

## Test Structure

### Test Organization
- **Setup/Teardown**: Proper fixture management for each test
- **Descriptive Names**: `test_logarithmic_compression_large`, `test_tamper_detection_merkle`
- **Isolation**: Each test is independent with proper mocking
- **Comprehensive**: 25+ test methods covering all functionality

### Key Test Categories

#### Functional Tests
- **Basic Aggregation**: All strategies with 10-100 signatures
- **Compression Validation**: Ratios >1, output validation
- **Strategy Comparison**: Cross-strategy consistency and performance

#### Security Tests
- **Tamper Detection**: Verification with modified signatures
- **Entropy Handling**: Proper random generation mocking
- **Input Validation**: Malformed input rejection

#### Edge Cases
- **Empty Lists**: Proper PatError raising
- **Invalid Strategies**: Error handling for unknown enums
- **Boundary Conditions**: Single signatures, large batches

#### Performance Tests
- **Scaling**: 10→100 signature performance characteristics
- **Memory Efficiency**: Large batch handling validation
- **Compression Improvement**: Scaling behavior verification

## Dependencies

- `unittest` (built-in Python standard library)
- `unittest.mock` (built-in Python standard library)
- Parent `pat_benchmark.py` module

Optional for enhanced testing:
- `pytest` for better test discovery and reporting
- `coverage` for code coverage analysis
- `pytest-cov` for pytest/coverage integration

## Dogecoin Core Alignment

Tests follow Dogecoin Core testing principles:

- **Comprehensive Coverage**: Equivalent to Core's extensive test suites
- **Security Focus**: Cryptographic validation matches Core's security testing
- **Error Handling**: PatError exceptions align with Core's error management
- **Performance Validation**: Scaling tests match Core's performance requirements
- **Clean Execution**: No side effects, proper cleanup

## Continuous Integration

For CI/CD pipelines, add:

```yaml
# GitHub Actions example
- name: Run PAT Tests
  run: |
    cd pat/src
    python -m pytest tests/ --cov=pat_benchmark.py --cov-report=xml
    # Coverage threshold: 80%
```

## Maintenance

- **Adding Tests**: Follow naming convention `test_[feature]_[condition]`
- **Mocking**: Use `unittest.mock` for external dependencies
- **Coverage**: Maintain >80% coverage on PatAggregator methods
- **Documentation**: Update this README when adding significant test categories

## Troubleshooting

- **Import Errors**: Ensure `PYTHONPATH` includes `pat/` directory
- **Coverage Issues**: Run from correct directory, check path to source files
- **Mock Failures**: Verify mock targets match actual import paths

---

*Tests maintain Dogecoin Core quality standards while thoroughly validating PAT's post-quantum signature aggregation capabilities.*

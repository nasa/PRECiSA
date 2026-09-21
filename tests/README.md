# PRECiSA Test Organization

This directory contains acceptance tests and regression baselines for PRECiSA's floating-point error analysis.

## Directory Structure

### `acceptance/`
JSON test descriptors that drive acceptance testing via `tools/testAcceptance.sh`.

Each JSON file specifies:
- Input `.pvs` and `.input` files (relative paths from the test descriptor location)
- Expected error bounds (`stableError`, `unstableError`)
- Optional baseline certificate paths for regression testing
- Optional flags (e.g., `--max-depth`)

**Running tests**:
```bash
# Single test
tools/testAcceptance.sh tests/acceptance/sine.test.json

# All tests
tools/testAllAcceptance.sh

# With certificate regression checking
tools/testAcceptance.sh --check-certificates tests/acceptance/sine.test.json

# With PVS typecheck (requires PVS installation)
tools/testAcceptance.sh --typecheck tests/acceptance/sine.test.json
```

### `baseline/`
Regression baseline captured from main branch for certificate verification.

**Purpose**: Ensures PRECiSA's certificate generation remains unchanged when code is modified.

**Contents**:
- PVS certificate files (`*_cert.pvs`, `*_num_cert.pvs`) for 29 benchmarks

**Verifying no regression**:
```bash
# Run all tests with certificate checking
tools/testAllAcceptance.sh --check-certificates
```

See `baseline/README.md` for details and regeneration instructions.

## Unit Tests

Unit tests are in `PRECiSA/tests/` (Haskell Tasty framework):
- `AbstractSemanticsTest.hs` - Tests for abstract semantics
- `Kodiak/ErrorComputationTest.hs` - Tests for Kodiak integration
- And many more...

**Running unit tests**:
```bash
cd PRECiSA
cabal v2-test
```

## Test Output

PRECiSA generates certificate files alongside the input `.pvs` file:
- `<name>_real.pvs` - Real-valued specification
- `<name>_cert.pvs` - Symbolic certificates
- `<name>_num_cert.pvs` - Numerical certificates

## Benchmarks

Benchmark programs are in `benchmarks/`:
- `FPBench/` - FPBench suite (28 programs from fpbench.org)
- `FPCore/` - FPCore format examples
- `daidalus/` - DAIDALUS air traffic management benchmarks

Each benchmark has:
- `.pvs` file - Program definition
- `.input` file - Input ranges
- Corresponding acceptance test descriptor in `tests/acceptance/`

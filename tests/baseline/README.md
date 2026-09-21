# PRECiSA Regression Baseline

Captured from main branch (commit dbe8ac5f) on 2026-09-18.

These files serve as a regression baseline to ensure that code changes don't
unintentionally alter PRECiSA's certificate generation.

## Contents

This baseline includes **29 benchmarks** from the FPBench suite and acceptance tests:

**FPBench benchmarks** (28 tests):
- azimuth, carbonGas, cubicSpline, doppler1-3, hartman3, hartman6, himmilbeau
- jet, kepler0-2, logExp, polycarp, predatorPrey, rigidBody1-2
- sine, sineOrder3, sphere, sqroot, styblinski, t_div_t1
- turbine1-3, verhulst

**Special cases**:
- `if_expression3`: From tests/acceptance/pvs-features/

Each test includes two PVS certificate files:
- Symbolic certificate (`*_cert.pvs`) - Contains proof obligations and lemmas
- Numerical certificate (`*_num_cert.pvs`) - Contains numerical error bounds

**Note**: JSON output is NOT stored in this baseline. JSON error values are verified 
via approximate equality checking in `testAcceptance.sh` using `checkErrorDifference.py`.
Only PVS certificate files require byte-for-byte comparison to detect regressions in 
proof structure.

## Verification

The baseline is automatically checked by the test infrastructure:

```bash
# Run all acceptance tests with certificate checking
tools/testAllAcceptance.sh --check-certificates

# Run a single test with certificate checking
tools/testAcceptance.sh --check-certificates tests/acceptance/azimuth.test.json
```

The `--check-certificates` flag performs:
1. **Approximate error checking** (via `checkErrorDifference.py`) - validates error bounds
2. **Byte-for-byte certificate comparison** (via `diff`) - detects any changes to PVS proofs

Test descriptors in `tests/acceptance/*.test.json` specify baseline paths:
```json
{
  "name": "azimuth",
  "stableError": 1.32E-13,
  "baselineCertFile": "../baseline/azimuth_cert.pvs",
  "baselineNumCertFile": "../baseline/azimuth_num_cert.pvs"
}
```

## Regenerating the Baseline

If main branch changes certificate generation (intentionally):

1. Switch to main branch: `git checkout main`
2. Rebuild: `cd PRECiSA && cabal v2-install`
3. Generate fresh baseline:
   ```bash
   # Generate for all benchmarks
   tests="azimuth carbonGas cubicSpline doppler1 doppler2 doppler3 hartman3 hartman6 \
     himmilbeau jet kepler0 kepler1 kepler2 logExp polycarp predatorPrey rigidBody1 \
     rigidBody2 sine sineOrder3 sphere sqroot styblinski t_div_t1 turbine1 turbine2 \
     turbine3 verhulst"
   
   for b in $tests; do
     pvsFile="benchmarks/FPBench/$b.pvs"
     inputFile="benchmarks/FPBench/$b.input"
     precisa "$pvsFile" "$inputFile"
     cp benchmarks/FPBench/${b}_cert.pvs benchmarks/FPBench/${b}_num_cert.pvs tests/baseline/
   done
   
   # Generate if_expression3 separately
   precisa tests/acceptance/pvs-features/if_expression3.pvs \
     tests/acceptance/pvs-features/if_expression3.input
   cp tests/acceptance/pvs-features/if_expression3_cert.pvs \
     tests/acceptance/pvs-features/if_expression3_num_cert.pvs tests/baseline/
   ```
4. Review changes: `git diff tests/baseline/`
5. If intentional, commit the updated baseline
6. Update this README with new commit hash and date

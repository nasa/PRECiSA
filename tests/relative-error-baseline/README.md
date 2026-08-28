# Relative error regression baseline

Captured before the relative-error feature was added, from commit 77df0da --
except `if_expression3.json`, captured later from a `master` build for the
reason given below.

These files pin PRECiSA's output with the relative-error feature absent or
disabled. Output must be byte-identical whenever `--relative-error` is not
passed.

`if_expression3` is here because the three FPBench benchmarks all report
irrational-looking error bounds, and those render the same however aeson
reaches them. `if_expression3` reports `0` and `2` -- INTEGRAL doubles, whose
rendering differs between aeson's `Scientific` path and its `E.double` path
(`0` versus `0.0`). Defining `toEncoding` on `AnalysisResultFun` instead of
`toJSON` silently switched paths and drifted the flag-off output; nothing in
the other three noticed. Only its JSON is pinned: its certificates live in
`tests/acceptance/pvs-features/` and are regenerated in place, so `git status`
already guards them.

Regenerate the comparison with:

    export PATH="$HOME/.ghcup/bin:$PATH"
    PRECISA=$(cabal list-bin precisa)
    for b in azimuth sine sqroot; do
      $PRECISA --json benchmarks/FPBench/$b.pvs benchmarks/FPBench/$b.input \
        | diff - tests/relative-error-baseline/$b.json || echo "REGRESSION in $b"
      diff benchmarks/FPBench/${b}_cert.pvs tests/relative-error-baseline/${b}_cert.pvs \
        || echo "CERT REGRESSION in $b"
      diff benchmarks/FPBench/${b}_num_cert.pvs tests/relative-error-baseline/${b}_num_cert.pvs \
        || echo "NUM CERT REGRESSION in $b"
    done
    $PRECISA --json tests/acceptance/pvs-features/if_expression3.pvs \
                    tests/acceptance/pvs-features/if_expression3.input \
      | diff - tests/relative-error-baseline/if_expression3.json \
      || echo "REGRESSION in if_expression3"

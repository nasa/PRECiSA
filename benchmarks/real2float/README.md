Benchmarks from the paper [Certified Roundoff Error Bounds Using
Semidefinite Programming](http://arxiv.org/pdf/1507.03331)

Tests with floating-point variables and without simplification
--------------------------------------------------------------

`make taylor-a-fp-no-simpl`: performs all tests with the simplified rounding model and with the approximate optimization.

`make taylor-c-fp-no-simpl`: performs all tests with the simplified rounding model and with the exact optimization.

`make taylor-a-fp-proof`: performs all tests and records proof certificates in the `hol` directory (with the approximate optimization). Proof certificates can be verified in HOL Light (see `hol/verify_a.hl`).

`make taylor-c-fp-proof`: performs all tests and records proof certificates in the `hol` directory (with the exact optimization). Proof certificates can be verified in HOL Light (see `hol/verify_c.hl`).
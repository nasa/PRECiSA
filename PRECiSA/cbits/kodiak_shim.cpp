// PRECiSA-owned translation layer over Kodiak's C API.
//
// Kodiak reports failures by throwing kodiak::Growl. Its own C adapter does not
// catch it, so the exception escapes into the Haskell FFI and terminates the
// process (std::terminate / SIGABRT). Haskell cannot catch a C++ exception; a
// C++ caller can, because extern "C" governs linkage, not unwinding. Each
// wrapper below therefore catches and reports a status code instead.
//
// Kodiak itself is NOT modified: this shim lives in PRECiSA and treats Kodiak
// as an external library.
//
// Only Codiak.h is included. kodiak::Growl derives from std::exception
// (Kodiak/src/types.hpp:270), so its message is reachable through what()
// without any Kodiak internal header.
//
// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------
//
// Not every Growl is the same kind of event. Division by an interval that
// contains zero is a legitimate mathematical outcome (the bound is unbounded);
// every other Growl -- and every other exception -- is a genuine failure that
// must never be mistaken for a valid bound. The wrappers discriminate between
// the two and hand back what() so PRECiSA can report why it failed.
//
// ---------------------------------------------------------------------------
// Entry points wrapped, and why
// ---------------------------------------------------------------------------
//
//   minmax_system_maximize   -- Growl from Node.cpp:584 during evaluation.
//   minmax_system_minmax     -- the same evaluation, run for both bounds at
//                               once. Used by the function-call error
//                               abstraction to enclose a callee's real-valued
//                               body, so it evaluates caller-supplied
//                               expressions over caller-supplied boxes and
//                               throws for exactly the same reasons.
//   real_create_division     -- Growl from Real.cpp:214 at construction, when
//                               the divisor is a literal interval containing
//                               zero.
//   the four bound getters   -- MinMax::{lb,ub}_of_{min,max} throw on their own
//                               (MinMax.cpp:42-64) whenever the corresponding
//                               point set is empty. MinMaxSystem::minmax calls
//                               minmax_.init(), which empties mm_ and the point
//                               sets, and the divide-by-zero throw fires in the
//                               very first evaluate() -- so in exactly the state
//                               that yields DIV_BY_ZERO above, every getter also
//                               throws. Wrapping maximize alone would just move
//                               the abort one call later. An infeasible box
//                               leaves mm_ empty on the success path too, so
//                               this is a pre-existing crash path independent of
//                               division.
//   paver_pave               -- Growl from Node.cpp:584 during evaluation, for
//                               exactly the reason minmax_system_maximize
//                               throws. The paver is a SEPARATE driver -- its
//                               own branch-and-bound (Paver.cpp:524) over its
//                               own Bool formula (NewPaver.hpp:evaluate) -- but
//                               it evaluates the same Real nodes, so a divisor
//                               whose enclosure contains zero aborts its
//                               recursion just as readily. Reached only with
//                               --paving.
//   paver_save_paving        -- writes the paving out (Paving::save,
//                               Paver.cpp:343). It evaluates nothing, so it
//                               raises no Growl of its own; it is wrapped
//                               because it is the paver's other exit and can
//                               still fail (allocation while formatting the
//                               boxes), and because a caller has to be able to
//                               tell a failed WRITE from a failed PAVING. The
//                               two have nothing in common: one is a filesystem
//                               or memory problem, the other says the formula
//                               could not be evaluated over the box.
//
// Paver entry points deliberately NOT wrapped, because they cannot throw:
//
//   paver_create             -- `new NewPaver(name)` and, for an empty name, an
//                               ostringstream for the default ID. No Kodiak
//                               check runs.
//   paver_register_variable  -- System::var (System.cpp:132) computes a
//                               resolution and pushes; there is no check to
//                               fail.
//   paver_set_maxdepth       -- a plain assignment
//                               (BranchAndBoundDF.hpp:47-49).
//   paver_set_precision      -- System::set_precision (System.hpp:74) calls
//                               set_tolerance, which DOES Growl -- but only for
//                               a negative tolerance (System.cpp:28-36), and the
//                               tolerance it passes is pow(10, precision),
//                               positive for every int. The corresponding
//                               min-max setters are unwrapped for the same
//                               reason.
//
// ---------------------------------------------------------------------------
// Caller contract on a nonzero status from minmax_system_maximize or
// minmax_system_minmax
// ---------------------------------------------------------------------------
//
//  1. The MinMaxSystem is DEAD. Branch-and-bound aborted mid-recursion, so any
//     bounds it holds cover only part of the box and are NOT a valid enclosure.
//     Do not read them.
//  2. Reusing it is SILENTLY UNSOUND. MinMaxSystem::acc_ (the pruning
//     accumulator, MinMaxSystem.hpp:56) is written by accumulate() and read by
//     prune(), but minmax() never resets it -- it resets min_or_max_, expr_,
//     dexpr_ and minmax_ only. A reused system prunes against stale bounds and
//     returns a too-small maximum with no diagnostic whatsoever.
//  3. Dropping or destroying it IS safe. Everything is value-typed and Real is
//     intrusively refcounted, so unwinding releases nodes correctly.
//
// The Haskell wrappers in Kodiak/Kodiak.hs enforce (1) and (2) structurally by
// refusing to hand back a usable handle on the failure path.
//
// ---------------------------------------------------------------------------
// Caller contract on a nonzero status from paver_pave
// ---------------------------------------------------------------------------
//
// The Paver is DEAD in the same sense: Paver::pave clears paving_ and then
// fills it from branch-and-bound (Paver.cpp:524-534), so an aborted run leaves
// a paving covering only the part of the box that was explored before the
// throw, and never reaches set_varbox/set_type. Saving it would write out a
// plausible-looking .paving file that is not a paving of the box the user
// asked about. So a failed pave must not be followed by a save; Kodiak.Paver
// enforces that by throwing before it gets there.

#include "Codiak.h"

#include <cstring>
#include <exception>

// Status codes returned by every precisa_* wrapper below.
#define PRECISA_KODIAK_OK          0
#define PRECISA_KODIAK_DIV_BY_ZERO 1
#define PRECISA_KODIAK_ERROR       2

// The only Growl condition that is a legitimate mathematical outcome rather
// than a failure. Thrown from Real.cpp (construction) and Node.cpp (eval);
// both messages end with this exact text.
//
// The match is deliberately this exact substring and nothing looser. Kodiak
// raises Growl for many other zero/domain conditions whose messages read
// similarly -- "ifnz cannot be evaluated when the first argument is an interval
// containing zero", "denumerator of a rational expression cannot be zero",
// "sqrt expects a nonnegative interval", "ln expects a positive interval",
// "acos expects an interval in [-1,1]" -- and every one of those is a failure.
static const char *PRECISA_DIV_BY_ZERO_MSG =
    "division by an interval that contains zero";

// Copies message into errbuf (always NUL-terminated when a buffer is given).
static void precisa_copy_message(const char *message, char *errbuf, int errbuflen) {
    if (errbuf != 0 && errbuflen > 0) {
        std::strncpy(errbuf, message, (size_t)(errbuflen - 1));
        errbuf[errbuflen - 1] = '\0';
    }
}

// Copies what() into errbuf and classifies it.
static int precisa_classify(const char *what, char *errbuf, int errbuflen) {
    precisa_copy_message(what, errbuf, errbuflen);
    return std::strstr(what, PRECISA_DIV_BY_ZERO_MSG) != 0
             ? PRECISA_KODIAK_DIV_BY_ZERO
             : PRECISA_KODIAK_ERROR;
}

static int precisa_unknown(char *errbuf, int errbuflen) {
    precisa_copy_message("unknown non-std::exception", errbuf, errbuflen);
    return PRECISA_KODIAK_ERROR;
}

extern "C" int precisa_minmax_system_maximize(CMinMaxSystem sys, CReal e,
                                              char *errbuf, int errbuflen) {
    try {
        minmax_system_maximize(sys, e);
        return PRECISA_KODIAK_OK;
    } catch (const std::exception &ex) {
        return precisa_classify(ex.what(), errbuf, errbuflen);
    } catch (...) {
        return precisa_unknown(errbuf, errbuflen);
    }
}

extern "C" int precisa_minmax_system_minmax(CMinMaxSystem sys, CReal e,
                                            char *errbuf, int errbuflen) {
    try {
        minmax_system_minmax(sys, e);
        return PRECISA_KODIAK_OK;
    } catch (const std::exception &ex) {
        return precisa_classify(ex.what(), errbuf, errbuflen);
    } catch (...) {
        return precisa_unknown(errbuf, errbuflen);
    }
}

extern "C" int precisa_real_create_division(CReal num, CReal den, CReal *out,
                                            char *errbuf, int errbuflen) {
    // Defined before the try, so a caller that ignores the status dereferences
    // NULL immediately rather than reading stack garbage as a Real*.
    if (out != 0) *out = 0;
    try {
        CReal result = real_create_division(num, den);
        if (out != 0) *out = result;
        return PRECISA_KODIAK_OK;
    } catch (const std::exception &ex) {
        return precisa_classify(ex.what(), errbuf, errbuflen);
    } catch (...) {
        return precisa_unknown(errbuf, errbuflen);
    }
}

// The four bound getters. Each throws when the corresponding point set is
// empty; see the header comment above.
//
// Three of the four have callers today: the maximize path reads both
// maximum_* bounds, and the min-max path reads minimum_lower_bound and
// maximum_upper_bound. minimum_upper_bound is wrapped pre-emptively for
// symmetry, so that no future caller has to reach past the shim to the raw,
// throwing Kodiak entry point. Keep it.
#define PRECISA_WRAP_BOUND(name)                                              \
    extern "C" int precisa_##name(CMinMaxSystem sys, double *out,             \
                                  char *errbuf, int errbuflen) {              \
        if (out != 0) *out = 0.0;                                             \
        try {                                                                 \
            double result = name(sys);                                        \
            if (out != 0) *out = result;                                      \
            return PRECISA_KODIAK_OK;                                         \
        } catch (const std::exception &ex) {                                  \
            return precisa_classify(ex.what(), errbuf, errbuflen);            \
        } catch (...) {                                                       \
            return precisa_unknown(errbuf, errbuflen);                        \
        }                                                                     \
    }

PRECISA_WRAP_BOUND(minmax_system_maximum_lower_bound)
PRECISA_WRAP_BOUND(minmax_system_maximum_upper_bound)
PRECISA_WRAP_BOUND(minmax_system_minimum_lower_bound)
PRECISA_WRAP_BOUND(minmax_system_minimum_upper_bound)

#undef PRECISA_WRAP_BOUND

// The paver. See the header comment for why only these two of the seven paver
// entry points are wrapped, and for the caller contract on a failed pave.

extern "C" int precisa_paver_pave(CPaver p, CBool e,
                                  char *errbuf, int errbuflen) {
    try {
        paver_pave(p, e);
        return PRECISA_KODIAK_OK;
    } catch (const std::exception &ex) {
        return precisa_classify(ex.what(), errbuf, errbuflen);
    } catch (...) {
        return precisa_unknown(errbuf, errbuflen);
    }
}

extern "C" int precisa_paver_save_paving(CPaver p, CString filename,
                                         char *errbuf, int errbuflen) {
    try {
        paver_save_paving(p, filename);
        return PRECISA_KODIAK_OK;
    } catch (const std::exception &ex) {
        return precisa_classify(ex.what(), errbuf, errbuflen);
    } catch (...) {
        return precisa_unknown(errbuf, errbuflen);
    }
}

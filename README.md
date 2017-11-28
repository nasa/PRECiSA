<pre>

Notices:

Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
   All Rights Reserved.

Disclaimers:

No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM
FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION,
IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE.
THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF
ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE
SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE,
IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT
SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING
ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT
SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS,
AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

</pre>

About PRECiSA
-------------
PRECiSA 2.0 (Program Round-off Error Certifier via Static Analysis) is a fully automatic analyzer for the estimation of round-off errors of floating-point valued functional expressions.
[Here](http://precisa.nianet.org/) you can try the PRECiSA web-interface and find more information on the tool.

The input to PRECiSA are the following files:

* A program `example.pvs` composed of floating-point valued functions. In its current version, PRECiSA accepts a subset of the language of the Prototype Verification System (PVS), including LET expressions, IF-THEN-ELSE constructions, function calls, and floating point values and operations such as: addition, multiplication, division, subtraction, floor, square root, trigonometric functions, logarithm, and exponential. For example:
   ```
   example: THEORY
   BEGIN
   IMPORTING float@aerr754dp

	example (X,Y: unb_double) : unb_double =
		IF (X >= RtoD(0) AND Y < RtoD(4))
		THEN IF (Y > RtoD(0))
			 THEN Dadd(X,Y)
			 ELSE Dmul(X,Y)
			 ENDIF
        ELSE Dsub(X,Y) ENDIF

   END example
   ```

* A file `example.input` containing initial ranges for the input variables of the program. For example:
   ```
   example(X,Y): X in [-10,10], Y in [0, 40]
   ```

* A file `example.path` containing a set of decision paths/sub-programs of interests. The user can specify these paths by listing the paths/sub-programs of interests as a list of True and False. For instance, in the example above, the path [True, True] corresponds to the sub-program Dadd(X,Y), the path [False] to the subprogram Dsub(X,Y), and the path [True] to the subprogram IF (Y > RtoD(0)) THEN Dadd(X,Y) ELSE Dmul(X,Y) ENDIF. The analysis is done for all the execution paths in the program, or better for all combination of real/FP execution path in the program). For the selected sub-programs, a dedicated error expression representing the round-off error estimation is computed. For the others, the tool will generate an overall error which is the maximum of all the round-off error estimations corresponding to these sub-programs. If the user does not select any sub-program of interest (None), the tool will produce the overall round-off error for the stable cases (when real and floating-point execution flows coincide) and the one for the unstable cases (when real and floating-point execution flows diverge). If the user is interested in a precise analysis of the entire program (All), the analysis will generate a semantic element for each combination of real/FP execution path in the program.
Examples of possible input for the decision pahts are the following:
   ```
   example(X,Y): None
   ```
   or
   ```
   example(X,Y): All
   ```
   or
   ```
   example(X,Y): [True, True]
   ```

More examples can be found in the [PRECiSA benchmarks folder](https://github.com/nasa/PRECiSA/tree/master/benchmarks/PRECiSA).

The analysis performed by PRECiSA results in one upper-bound of the floating-point round-off error for each decision path of interest, an overall upper-bound for the decision paths not of interest, and an overall upper-bound for the unstable test cases (when real and floating-point flows diverge).
Additionally, PRECiSA generates two PVS theories:

* a theory containing a collection of lemmas stating symbolic accumulated round-off error estimations for the input program, and
* a theory containing a collection of lemmas stating numerical accumulated round-off error estimations.  The numerical estimations are computed using Kodiak.

All the generated lemmas are equipped with PVS proof scripts that automatically discharge them.


Prerequisites
-------------
PRECiSA with the following instructions may run only on Linux and Mac OS X operating systems.

To install PRECiSA you will need to install:

* The [Glorious Haskell Compiler](https://www.haskell.org/ghc/) and its package manager Cabal, both available as part of the [Haskell platform](https://www.haskell.org/platform/),
* [CMake](https://cmake.org/),
* the [NASA Kodiak library](https://github.com/nasa/Kodiak) to compute numerical error bounds.

To verify the certificates generated by PRECiSA you will need to install:

* [PVS version 6.0](http://pvs.csl.sri.com/),
* the [NASA PVS library](https://github.com/nasa/pvslib).


How to install PRECiSA
----------------------
1. Install [Kodiak](https://github.com/nasa/Kodiak) producing `libKodiakDynamic.so` (for Linux) or `libKodiakDynamic.dylib` (for MacOS).

2. Set the environment variable KODIAK_LIB with the directory in which the `libKodiakDynamic.so` (or `libKodiakDynamic.dylib`) is present.
   In a bourne shell it can be set like this:
   ```
   $ export KODIAK_LIB=<directory-containing-libKodiakDynamic.so>
   ```

3. Go to the root repository directory (it depends on where you have downloaded it and how you have named it).
   ```
   $ cd <your-root-repository-directory>
   ```

4. Create a `build` directory:
   ```
   $ mkdir build
   ```

5. Invoke CMake on the root of the repository from the build directory
   ```
   $ cd build
   $ cmake ../PRECiSA
   ```

6. Invoke CMake's build command on the build directory
   ```
   $ cmake --build .
   ```

At this point, the executable should be in the current `build` directory. You can add the current directory to your `PATH` variable or install (copy) it to your place of choice.


How to use PRECiSA
------------------
We assume that `precisa` (the executable of PRECiSA) is in the current directory.

To use PRECiSA run:
```
$ ./precisa "<example>.pvs" "<example>.input" "<example>.path" <max_depth> <precision> <displayed_precision> <stable-test-assumption?> <max-num-lemmas>
```

- the first parameter is the path to the PVS program to be analyzed;
- the second one is the path to the file that indicates the initial values for the input variables of the input program;
- the third one is the path to the file that indicates the decision paths of interest for every function in the program;
- the fourth, fifth, and sixth parameters are options for the branch-and-bound search: the maximum depth, the precision, and the displayed precision, respectively;
- the seventh parameter is a boolean value indicating if the Stable Test Assumption is used (True) or not (False). If this parameter is set to True, real and floating-point execution flows are assumed to coincide. Therefore, the analysis can be unsound since the cases where the execution paths diverge (unstable cases) are not considered;
- the last parameter is the maximum number of lemmas allowed to be generated by PRECiSA. This avoids having certificates too big to be treated. If your program generates a huge number of lemmas, this means probably that you have several nested if-then-else. In this case, try to run PRECiSA setting some decision paths of interests, or try to run it with the Stable Test Assumption set to True.

An example of how to execute PRECiSA is the following:
```
$ ./precisa "example.pvs" "example.input" "example.path" 7 14 2 False 40
```


How to verify the PVS certificates
----------------------------------

[PVS version 6.0](http://pvs.csl.sri.com) and the development version
of the [NASA PVS Library](https://github.com/nasa/pvslib) are required
to proof-check the symbolic and the numerical certificates generated by PRECiSA in
PVS. Furthermore, the directory
`PVS` has to be added to the Unix environment variable
`PVS_LIBRARY_PATH`.  Depending upon your shell, one of the following lines
has to be added to your startup script.  In C shell (csh or tcsh), put this line in
`~/.cshrc`, where `<precisapvsdir>` is the absolute path to the
directory `PVS`:

~~~
setenv PVS_LIBRARY_PATH "<precisapvsdir>:$PVS_LIBRARY_PATH"
~~~

In Borne shell (bash or sh), put this line in either `~/.bashrc or ~/.profile`:

~~~
export PVS_LIBRARY_PATH="<precisapvsdir>:$PVS_LIBRARY_PATH"
~~~

You can use the proveit shell script to automatically check the proofs in the symbolic and numerical certificates generated by PRECiSA. For example, if you analyzed the program `example.pvs`, in the same folder you will find two files: `cert_example.pvs` and `num_cert_example.pvs`.

To check the correctness of the PVS theories in `cert_example.pvs` and `num_cert_example.pvs` you can run:
```
$ proveit -sc cert_example.pvs
$ proveit -sc num_cert_example.pvs
```

### PVS basic troubleshooting ###
If the PVS verification is not behaving as expected, try cleaning the PVS binaries in the NASA PVS library. Simply run cleanbin-all in the NASA PVS library folder of your installation and try again.


Contact information
-------------------
If you have any question or problem, please contact:

* [Laura Titolo](laura.titolo@nianet.org) (for PRECiSA)
* [Mariano Moscato](mariano.moscato@nianet.org) (for PVS)
* [Marco A. Feliu](marco.feliu@nianet.org) (for Kodiak)

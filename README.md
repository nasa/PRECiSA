![PRECiSA Logo](logo.svg "PRECiSA")
=======

PRECiSA (Program Round-off Error Certifier via Static Analysis) is a fully automatic static analyzer for floating-point programs.
Given a floating-point program, PRECiSA computes a sound over-approximation of the round-off error that may occur, In addition, it generates PVS certificates ensuring the correctness of such approximation.

PRECiSA supports conditionals, let-in expressions, non-recursive function calls, predicates, and a wide variety of mathematical operators (including trigonometric functions).

You can use PRECiSA via [command-line](#floating-point-round-off-error-estimation) or with [VSCode-PRECiSA](vscode-precisa) a toolkit that provides a convenient user interface to perform floating-point analysis with PRECiSA.


# Installation

PRECiSA runs on Linux and Mac OS X operating systems.

You can install PRECiSA in two different ways:
- automatically with a *Makefile*
- manually with *cabal*

## Prerequisites

To build and install PRECiSA you need:
* The [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (version `>=8.10.7`) and
* its package manager [Cabal](https://cabal.readthedocs.io/en/3.6/) (version `>=3.6.2.1`).

If you install PRECiSA using the *Makefile* option you also need:
- `Make`
- `wget` or `curl` HTTP clients
- `git`
- `CMake`
- *C99/C++17* compilers

Alternatively, a *Dockerfile* is provided to create a container with all the previous prerequisites that can be used to build PRECiSA.


To verify certificates generated by PRECiSA you need:
* [PVS version 7.1](http://pvs.csl.sri.com/),
* the [NASA PVS library](https://github.com/nasa/pvslib).

If you want to use the SMT optimization you need [FPRock](https://github.com/nasa/FPRoCK).


## Build

### Automated

A *Makefile* is provided to automate the downloading and building of *Kodiak* and its dependencies, and to configure and build PRECiSA.

 The only command that needs to be run from the folder containing the `Makefile` file is:
```sh
$ make
```

If you want to install PRECiSA inside a container, a *Dockerfile* is provided. Given a standard *Docker* installation and the PRECiSA repository path `<PRECiSA-path>`,
the commands to install PRECiSA inside the container are:
```sh
$ cd <PRECiSA-path>
$ docker build -t precisa-container .
...
$ docker run -v <PRECiSA-path>:/precisa  -it precisa-container /bin/bash
docker$ cd /precisa
docker$ make
...
docker$ echo "Now precisa is available for the current session"
docker$ precisa
...
```

### Manual

The following instructions assume access to a *Bash* shell.

1. Install [Kodiak](https://github.com/nasa/Kodiak) producing a dynamic library `libkodiak.so` (for Linux) or `libkodiak.dylib` (for MacOS).

2. Make accessible the *Kodiak* dynamic library to your linker and final executable.
   Store the path of the folder containing the dynamic library in an auxiliary shell variable.
   ```
   $ export KODIAK_LIB_DIR=<directory-containing-libkodiak.so>
   ```
   In Linux the `LD_LIBRARY_PATH` environment variable has to include the path contained in `KODIAK_LIB_DIR`.
   You can add the following line to your shell initialization script (for example, `~/.profile` or `~/.bashrc`) to make the `LD_LIBRARY_PATH` change permanent.
   ```
   $ export LD_LIBRARY_PATH="$KODIAK_LIB_DIR:$LD_LIBRARY_PATH"
   ```
   In Mac the linker itself will store the path of the dynamic library inside the executable automatically, so no additional step here is needed.

3. Configure the *cabal* project inside the `PRECiSA` sub-directory of the repository
   ```
   $ cd <repository-root>/PRECiSA
   $ cat > cabal.project.local <<EOF
   optimization: True

   package precisa
      extra-lib-dirs: $KODIAK_LIB_DIR
   EOF
   ```

4. Build and install the `precisa` executable using *cabal* *nix*-style builds
   ```
   $ cabal v2-install
   ```

5. Run the `precisa` executable
   ```
   $ ~/.cabal/bin/precisa
   ```


# Floating-Point Round-Off Error Estimation

The input to the PRECiSA round-off error estimator are the following files:

* A program `example.pvs` composed of floating-point valued functions. In its current version, PRECiSA accepts a subset of the language of the Prototype Verification System (PVS), including LET-IN expressions, IF-THEN-ELSE constructions, function calls, and floating-point values and operations such as: addition, multiplication, division, subtraction, floor, square root, trigonometric functions, logarithm, and exponential. For example:
   ```
   example: THEORY
   BEGIN
   IMPORTING float@aerr754dp

	example (X,Y: double) : double =
		IF (X >= 0 AND Y < 4)
		THEN IF (Y > 0)
			 THEN X+Y
			 ELSE X*Y
			 ENDIF
        ELSE X-Y ENDIF

   END example
   ```

* A file `example.input` containing initial ranges for the input variables of the program. For example:
   ```
   example(X,Y): X in [-10,10], Y in [0, 40]
   ```

More examples can be found in the [PRECiSA benchmarks folder](benchmarks).

The analysis performed by PRECiSA results in one upper-bound of the floating-point round-off error for each decision path of interest, an overall upper-bound for the rest of decision paths, and an overall upper-bound for the unstable test cases (when real and floating-point flows diverge).
Additionally, PRECiSA generates two PVS theories:

* a theory containing a collection of lemmas stating symbolic accumulated round-off error estimations for the input program, and
* a theory containing a collection of lemmas stating numerical accumulated round-off error estimations.  The numerical estimations are computed using Kodiak.

All the generated lemmas are equipped with PVS proof scripts that automatically discharge them.


## How to run the PRECiSA round-off error estimator

We assume that `precisa` (the executable of PRECiSA) is in the current directory.

To launch the round-off error analysis of PRECiSA with the default parameters run:
```
$ ./precisa "example.pvs" "example.input"
```
- the first parameter is the path to the PVS program to be analyzed;
- the second one is the path to the file that indicates the initial values for the input variables of the input program;

### Command Line Options

- Options for the branch-and-bound search used to compute the numerical estimation:
  - `--max-depth 7` (or `-d 7`) is the maximum depth of the branch-and-bound exploration with a default value of `7`.
  - `--precision 14` (or `-p 14`) is the negative exponent of `10` representing the numerical precision used.
  It has a default value of `14` which stands for a precision of <span style="white-space: nowrap;"><math>10<sup>-14<sup></math></span>.

- `--assume stability` (or `-s`) if this option is activated, real and floating-point execution flows are assumed to coincide (Stable Test Assumption). Therefore, the analysis can be unsound since the cases where the execution paths diverge (unstable cases) are not considered.

- `--fpcore` if this option is activated, PRECiSA accepts the input file in [FPCore](https://fpbench.org/spec/fpcore-2.0.html) syntax.

- `--unfold-fun-calls` if this option is activated, the body of each function call is unfolded in the error expression before it is globally optimized. This option may lead to more accurate results.

- `--smt-optimization` if this option is activated, PRECiSA checks the satisfiability of each path condition by calling an external SMT solver through the FPRoCK tool. In this way, it is possible to detect and remove the spurious execution paths, improving the accuracy of the round-off error estimation.

An example of how to execute PRECiSA by manually setting some options is the following:
```
$ ./precisa "example.pvs" "example.input" --paths "example.path" --max-depth 7 --precision 14
```


## How to verify the PVS certificates

[PVS version 7.1](http://pvs.csl.sri.com) and the development version
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


## Version

*PRECiSA v-4.0.3* (May 2024)

## Contact information
If you have any question or problem, please contact:

* [Laura Titolo](mailto:laura.titolo@nasa.gov) (for PRECiSA)
* [Mariano Moscato](mailto:mariano.m.moscato@nasa.gov) (for PVS)
* [Marco A. Feliu](mailto:marco.feliu@nasa.gov) (for Kodiak and installation issues)
* [Paolo Masci](mailto:paolo.m.masci@nasa.gov) (for VSCode-PRECiSA)
* [César Muñoz](mailto:cesar.a.munoz@nasa.gov) (for PRECiSA at NASA)

## Additional Contributors

* Rocco Salvia, University of Utah (at time of contribution)
* Caleb Chan, University of Washington
* Nikson Bernardes Fernandes Ferreira, University of Brasilia

## Related Publications

- Rocco Salvia, Laura Titolo, Marco A. Feliú, Mariano M. Moscato, César A. Muñoz, Zvonimir Rakamaric:
  A Mixed Real and Floating-Point Solver. NFM 2019

- Laura Titolo, César A. Muñoz, Marco A. Feliú, Mariano M. Moscato:
  Eliminating Unstable Tests in Floating-Point Programs. LOPSTR 2018

- Laura Titolo, Marco A. Feliú, Mariano M. Moscato, César A. Muñoz:
  An Abstract Interpretation Framework for the Round-Off Error Analysis of Floating-Point Programs. VMCAI 2018

- Mariano M. Moscato, Laura Titolo, Aaron Dutle, César A. Muñoz:
  Automatic Estimation of Verified Floating-Point Round-Off Errors via Static Analysis. SAFECOMP 2017


## License and Copyright Notice

The code in this repository is released under NASA's Open Source Agreement.  See the directory [`LICENSES`](LICENSES).

<pre>

Notices:

Copyright 2022 United States Government as represented by the Administrator of the National Aeronautics
and Space Administration. All Rights Reserved.

Disclaimers
No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND,
EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY
THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT,
ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT
DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT,
IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF
ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS
RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS
ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL
SOFTWARE, AND DISTRIBUTES IT "AS IS."

Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES
GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.
IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED
ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY
AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS,
AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY
FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

</pre>

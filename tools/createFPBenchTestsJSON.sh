#!/bin/bash

ABSPATH_SOURCE="$(realpath ${BASH_SOURCE[0]})"
ABSPATH_FPBENCH="$(realpath $(dirname $ABSPATH_SOURCE)/../benchmarks/FPBench)"
FPBENCH="$(grealpath --relative-to=$(pwd) $ABSPATH_FPBENCH)"
createTestJSON="$(dirname $ABSPATH_SOURCE)/createTestJSON.sh"

while read -r name stableError unstableError; do
  echo -n "Creating JSON for $name..."
  $createTestJSON "$name" "$FPBENCH/$name.pvs" "$FPBENCH/$name.input" "$stableError" "${unstableError:-null}"
  echo "DONE"
done <<-EOF
  azimuth      1.32E-13
  hartman3	   2.33E-14
  hartman6	   4.16E-13
  logExp	     6.77E-15
  sphere	     9.99E-15
  cubicSpline	 1.33E-14 2.70E+01
  polycarp 	   1.83E-15 6.00E-01
  Styblinski	 3.80E-14 5.16E+01
  carbonGas	   7.19E-09
  doppler1	   2.64E-13
  doppler2	   4.14E-13
  doppler3	   1.22E-13
  himmilbeau	 1.00E-12
  jet	         2.27E-11
  kepler0	     1.10E-13
  kepler1	     4.63E-13
  kepler2	     2.44E-12
  predatorPrey 1.84E-16
  rigidBody1	 2.95E-13
  rigidBody2	 3.61E-11
  sine      	 1.13E-15
  sineOrder3	 1.27E-15
  sqroot	     6.83E-16
  t_div_t1     5.72E-14
  turbine1     2.59E-14
  turbine2     3.12E-14
  turbine3     1.69E-14
  verhulst     4.19E-16
EOF

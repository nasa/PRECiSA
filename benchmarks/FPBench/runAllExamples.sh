#!/bin/bash

PRECiSA_FOLDER=../../PRECiSA
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
function realpath { echo $(cd $(dirname $1); pwd)/$(basename $1); }

cd ${SCRIPTPATH}
shopt -s extglob
EXAMPLES=""
for ex in !(*_cert|*_real).pvs ; do
  EXAMPLES+="$(realpath ${ex}) "
done

cd ${PRECiSA_FOLDER}
for ex in ${EXAMPLES}; do
  echo
  echo "Running ${ex}..."
  echo
  cabal run --verbose=0 exe:precisa -- "${ex}" "${ex%.pvs}.input"
  echo "----------------------------------------------------------------------"
done

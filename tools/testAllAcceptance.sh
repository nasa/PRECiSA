#!/usr/bin/env bash

ABSPATH_SOURCE="$(realpath ${BASH_SOURCE[0]})"
CHECKER="$(dirname $ABSPATH_SOURCE)/testAcceptance.sh"
ACCEPTANCE_FOLDER="$(dirname $ABSPATH_SOURCE)/../tests/acceptance"

cd $ACCEPTANCE_FOLDER

exit_code=0
for t in *.test.json; do
    echo -n .
    $CHECKER $t || exit_code=$?
done

if [[ $exit_code -ne 0 ]]; then
    echo "Some tests failed!"
    exit $exit_code
fi

echo "OK"

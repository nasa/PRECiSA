#!/bin/bash

name=${1}
pvsFile=${2}
inputFile=${3}
stableError=${4:-"null"}
unstableError=${5:-"null"}

jq -n \
  --arg name "$name" \
  --arg pvsFile "$pvsFile" \
  --arg inputFile "$inputFile" \
  --argjson stableError "$stableError" \
  --argjson unstableError "$unstableError" \
  '{"name": $name, "pvsFile": $pvsFile, "inputFile": $inputFile, "stableError": $stableError, "unstableError": $unstableError}' > $name.test.json

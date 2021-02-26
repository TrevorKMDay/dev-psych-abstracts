#!/bin/bash

in=${1}

cat ${in} | \
    tr '\n' '\t' | \
    sed -e 's/\t\t\t/\n/g' \
        -e 's/\t\t/------/g' \
        -e 's/\t/ /g'      \
        -e 's/------/\t/g'

echo

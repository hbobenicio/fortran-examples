#!/bin/bash
set -eu -o pipefail

set -x

gfortran -Wall -Wextra -std=f2018 -fimplicit-none -o fwhich main.f90

{ set +x; } 2> /dev/null


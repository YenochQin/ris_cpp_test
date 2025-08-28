#!/bin/bash

# Advanced Fortran USE statement modernizer
# Usage: ./fix_use_statements.sh <directory>

set -e

if [ $# -eq 0 ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

DIR="$1"
cd "$DIR"

echo "Fixing USE statements in $DIR"

# Process all .f90 files
for f in *.f90; do
    if [ -f "$f" ]; then
        echo "Processing: $f"
        
        # Replace USE vast_kind_param with direct iso_fortran_env use
        # and remove the ONLY clause
        sed -i '/USE vast_kind_param/c\      use iso_fortran_env, only: real64, int32, int64, real128' "$f"
        
        # Alternative: remove ONLY clause but keep vast_kind_param
        # sed -i 's/USE vast_kind_param, ONLY:.*/USE vast_kind_param/' "$f"
    fi
done

echo "USE statements updated!"
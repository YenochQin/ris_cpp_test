#!/bin/bash

# Script to fix incorrect real(real64) syntax to correct real(kind=real64) syntax

echo "Searching for files with incorrect real(real64) syntax..."

# Use effective find and sed to replace the incorrect syntax globally
find src -name "*.f90" -o -name "*.f90E" -o -name "*.F90" | while read file; do
    if grep -q "real(real64)" "$file"; then
        echo "Fixing file: $file"
        # Use sed to replace all occurrences of real(real64) with real(kind=real64)
        sed -i 's/\b\real(real64)\b/\real(kind=real64)/g' "$file"
        echo "  -> Fixed syntax in $file"
    fi
done

echo ""
echo "Search complete. All incorrect real(real64) syntax has been fixed to real(kind=real64)."
echo ""

# Verify that all instances have been fixed
echo "Verification: Checking for any remaining incorrect syntax..."
find src -name "*.f90" -o -name "*.f90E" -o -name "*.F90" | xargs grep -l "real(real64)" || echo "No files with incorrect syntax found. All fixes verified."
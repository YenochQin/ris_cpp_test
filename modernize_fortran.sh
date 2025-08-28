#!/bin/bash

# Fortran 95 to Fortran 2023 Modernization Script
# Usage: ./modernize_fortran.sh <directory>

set -e

if [ $# -eq 0 ]; then
    echo "Usage: $0 <directory>"
    echo "Example: $0 src/lib/libmod"
    exit 1
fi

DIR="$1"

if [ ! -d "$DIR" ]; then
    echo "Error: Directory $DIR does not exist"
    exit 1
fi

echo "Modernizing Fortran code in directory: $DIR"

# Change to target directory
cd "$DIR"

# Backup original files
echo "Creating backups..."
for f in *.f90; do
    if [ -f "$f" ]; then
        cp "$f" "$f.backup"
    fi
done

echo "Applying modernizations..."

# 1. Replace old-style type declarations with F2023 equivalents
for f in *.f90; do
    if [ -f "$f" ]; then
        echo "Processing: $f"
        
        # Type modernization
        sed -i 's/REAL\*8/real(real64)/g' "$f"
        sed -i 's/DOUBLE PRECISION/real(real64)/g' "$f" 
        sed -i 's/REAL(KIND=8)/real(real64)/g' "$f"
        sed -i 's/INTEGER\*4/integer(int32)/g' "$f"
        sed -i 's/INTEGER(KIND=4)/integer(int32)/g' "$f"
        sed -i 's/COMPLEX\*16/complex(real64)/g' "$f"
        sed -i 's/LOGICAL\*4/logical/g' "$f"
        
        # Replace legacy vast_kind_param usage with iso_fortran_env types
        sed -i 's/REAL(DOUBLE)/real(real64)/g' "$f"
        sed -i 's/INTEGER(LONG)/integer(int64)/g' "$f"
        sed -i 's/INTEGER(SHORT)/integer(int32)/g' "$f"
        sed -i 's/REAL(EXTENDED)/real(real128)/g' "$f"
        
        # Case-insensitive variants
        sed -i 's/real(double)/real(real64)/g' "$f"
        sed -i 's/integer(long)/integer(int64)/g' "$f"
        sed -i 's/integer(short)/integer(int32)/g' "$f"
        sed -i 's/real(extended)/real(real128)/g' "$f"
        
        # Update USE statements to use iso_fortran_env where needed
        if grep -q "USE vast_kind_param" "$f"; then
            # Add iso_fortran_env import if file uses real64, int32, etc.
            if grep -q "real(real64)\|integer(int32)\|integer(int64)\|real(real128)" "$f"; then
                # Check if iso_fortran_env is already imported
                if ! grep -q "use.*iso_fortran_env" "$f"; then
                    # Add import after the vast_kind_param line
                    sed -i '/USE vast_kind_param/a\   use iso_fortran_env, only: real64, real128, int32, int64' "$f"
                fi
            fi
        fi
        
        # Format modernization (optional - be careful with this)
        # sed -i 's/^C/!/g' "$f"  # Convert C comments to ! comments
        
    fi
done

echo "Modernization complete!"
echo ""
echo "Summary of changes made:"
echo "- REAL*8 → real(real64)"
echo "- DOUBLE PRECISION → real(real64)" 
echo "- REAL(KIND=8) → real(real64)"
echo "- INTEGER*4 → integer(int32)"
echo "- INTEGER(KIND=4) → integer(int32)"
echo "- COMPLEX*16 → complex(real64)"
echo "- Legacy vast_kind_param aliases → iso_fortran_env types"
echo ""
echo "Backup files created with .backup extension"
echo "Test compilation before committing changes!"

# Optional: Show diff summary
echo ""
echo "Files modified:"
for f in *.f90; do
    if [ -f "$f.backup" ]; then
        if ! diff -q "$f" "$f.backup" > /dev/null; then
            echo "  $f"
        fi
    fi
done
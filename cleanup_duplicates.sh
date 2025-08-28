#!/bin/bash

# Script to remove duplicate iso_fortran_env imports

echo "Cleaning duplicate iso_fortran_env imports..."

# Find all .f90 files and process them
find src/ -name "*.f90" -type f | while read -r file; do
    # Check if file has duplicate iso_fortran_env imports
    if grep -q "use iso_fortran_env" "$file"; then
        duplicate_count=$(grep -c "use iso_fortran_env" "$file")
        if [ "$duplicate_count" -gt 1 ]; then
            echo "Processing: $file (found $duplicate_count imports)"
            
            # Create temp file with only the first iso_fortran_env import
            temp_file=$(mktemp)
            
            # Process the file line by line
            first_iso_found=false
            while IFS= read -r line; do
                if echo "$line" | grep -q "use iso_fortran_env"; then
                    if [ "$first_iso_found" = false ]; then
                        # Keep the first occurrence
                        echo "$line" >> "$temp_file"
                        first_iso_found=true
                    fi
                    # Skip subsequent occurrences
                else
                    echo "$line" >> "$temp_file"
                fi
            done < "$file"
            
            # Replace original file
            mv "$temp_file" "$file"
        fi
    fi
done

echo "Cleanup complete!"
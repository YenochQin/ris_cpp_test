#!/bin/bash
# UNUSED FILE REMOVAL SCRIPT
# This script removes unused library files from the GRASP codebase

echo "=== REMOVING UNUSED FILES AND DIRECTORIES ==="

# 1. UNUSED DIRECTORIES (not referenced in active CMakeLists.txt)
echo "Removing unused directories..."

# MPI directory (commented out in main CMake)
if [ -d "src/lib/mpi90" ]; then
    echo "Removing src/lib/mpi90/ (MPI support not enabled)"
    rm -rf src/lib/mpi90/
fi

# Tool directory (commented out in main CMake) 
if [ -d "src/tool" ]; then
    echo "Removing src/tool/ (tools not built)"
    rm -rf src/tool/
fi

# 2. UNUSED INTERFACE FILES (created during modernization but not used)
echo "Removing unused interface files from libmod..."

UNUSED_INTERFACES=(
    "src/lib/libmod/configuration_interface.f90"
    "src/lib/libmod/csf_interface.f90"
    "src/lib/libmod/debug_setup_interface.f90"
    "src/lib/libmod/factorial_interface.f90"
    "src/lib/libmod/isotope_shift_calculation_interface.f90"
    "src/lib/libmod/mixblock_interface.f90"
    "src/lib/libmod/mixing_coefficients_interface.f90"
    "src/lib/libmod/structure_sum_interface.f90"
    "src/lib/libmod/summary_interface.f90"
    "src/lib/libmod/user_interaction_interface.f90"
)

for file in "${UNUSED_INTERFACES[@]}"; do
    if [ -f "$file" ]; then
        echo "Removing $file"
        rm "$file"
    fi
done

# 3. UNUSED MODERNIZATION FILES (mentioned in guide but not built)
echo "Removing unused modernization files from libmod..."

UNUSED_MODERN=(
    "src/lib/libmod/computational_parameters_M.f90"
    "src/lib/libmod/mathematical_utilities.f90" 
    "src/lib/libmod/modern_file_operations.f90"
    "src/lib/libmod/precision_definitions_M.f90"
)

for file in "${UNUSED_MODERN[@]}"; do
    if [ -f "$file" ]; then
        echo "Removing $file"
        rm "$file"
    fi
done

# 4. OLD MAIN PROGRAM FILE (replaced by isotope_shift.f90)
if [ -f "src/appl/ris4/relativistic_isotope_shift.f90" ]; then
    echo "Removing old main program: src/appl/ris4/relativistic_isotope_shift.f90"
    rm "src/appl/ris4/relativistic_isotope_shift.f90"
fi

# 5. CLEANUP OTHER POTENTIAL UNUSED FILES
echo "Checking for other unused files..."

# Check for backup files
find . -name "*.f90~" -delete 2>/dev/null && echo "Removed backup files"
find . -name "*.bak" -delete 2>/dev/null && echo "Removed .bak files"

# Check for unused CMakeLists.txt in removed directories
if [ -f "src/lib/mpi90/CMakeLists.txt" ]; then
    echo "Note: MPI CMakeLists.txt was removed with directory"
fi

if [ -f "src/tool/CMakeLists.txt" ]; then
    echo "Note: Tool CMakeLists.txt was removed with directory"
fi

echo ""
echo "=== CLEANUP COMPLETE ==="
echo "Removed unused files and directories from GRASP codebase"
echo ""
echo "Files that remain and are actively used:"
echo "- src/lib/libmod/precision_definitions.f90 (modernized precision)"
echo "- src/appl/ris4/isotope_shift.f90 (modernized main program)"
echo "- All other files in active CMake targets"
echo ""
echo "Next step: Run 'make clean && make' to verify build still works"
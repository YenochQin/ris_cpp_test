# Fortran 95 to 2023 Modernization Guide

This document outlines the key modernizations implemented in the GRASP RIS codebase upgrade from Fortran 95 to Fortran 2023.

## Summary of Modernizations

### 1. Precision Definitions (`precision_definitions_M.f90`)

**Before (Fortran 95):**
```fortran
module vast_kind_param
   integer, parameter :: double = selected_real_kind(13)
   integer, parameter :: extended = selected_real_kind(30)
end module vast_kind_param
```

**After (Fortran 2023):**
```fortran
module precision_definitions
   use, intrinsic :: iso_fortran_env, only: real32, real64, real128
   integer, parameter :: working_precision_kind = real64  ! ~15 decimal digits
   integer, parameter :: extended_precision_kind = real128  ! ~33 decimal digits
end module precision_definitions
```

**Benefits:**
- Uses standard intrinsic types for better portability
- Descriptive variable names improve code readability
- Guaranteed precision levels across different compilers

### 2. Parameter Definitions (`computational_parameters_M.f90`)

**Before:**
```fortran
module parameter_def
   integer, parameter :: NNNP = 590    ! Cryptic name
   integer, parameter :: NNNW = 127    ! No documentation
end module parameter_def
```

**After:**
```fortran
module computational_parameters
   !> Number of points in the radial integration grid
   integer, parameter :: radial_grid_points = 590
   
   !> Maximum number of atomic orbitals in the calculation  
   integer, parameter :: max_orbitals_count = 127
end module computational_parameters
```

**Benefits:**
- Self-documenting variable names
- Comprehensive documentation for each parameter
- Clear purpose and usage explanation

### 3. Error Handling (`modern_file_operations.f90`)

**Before:**
```fortran
open(unit=10, file='data.txt', status='old')
read(10, *) data
close(10)
```

**After:**
```fortran
call open_input_file(file_handle, 'data.txt', .true., status, errmsg)
if (status /= 0) then
   write(error_unit, '(A,A)') 'ERROR: ', trim(errmsg)
   return
end if
```

**Benefits:**
- Automatic unit number assignment with `newunit`
- Comprehensive error handling with status codes and messages
- Resource management with safe file closure
- Structured error propagation

### 4. Pure and Elemental Procedures (`mathematical_utilities.f90`)

**Before:**
```fortran
function factorial(n) result(fact)
   integer :: n, fact
   ! Implementation with potential side effects
end function factorial
```

**After:**
```fortran
pure recursive function factorial_calculation(input_number) result(factorial_result)
   integer(standard_integer_kind), intent(in) :: input_number
   real(working_precision_kind) :: factorial_result
   ! Pure implementation - no side effects, can be parallelized
end function factorial_calculation
```

**Benefits:**
- `pure` procedures enable compiler optimizations and parallelization
- `elemental` procedures work element-wise on arrays
- Explicit `intent` declarations improve code safety and clarity

### 5. Modern Program Structure

**Before:**
```fortran
PROGRAM RIS
   USE vast_kind_param, ONLY: DOUBLE
   IMPLICIT NONE
   REAL(DOUBLE) :: DR2
   ! Minimal error handling
   ! Basic variable names
END PROGRAM RIS
```

**After:**
```fortran
program relativistic_isotope_shift
   use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
   use precision_definitions, only: working_precision_kind
   
   implicit none
   
   ! Descriptive variable names
   real(working_precision_kind) :: mass_ratio_squared
   integer :: calculation_status
   character(len=256) :: error_message
   
   ! Comprehensive error handling throughout
end program relativistic_isotope_shift
```

**Benefits:**
- Descriptive program and variable names
- Structured error handling with status codes and messages  
- Modern intrinsic module usage
- Enhanced user feedback and documentation

## Key Fortran 2023 Features Implemented

### 1. **Intrinsic Module Usage**
- `iso_fortran_env` for standard types and units
- Portable precision definitions
- Standard error and output units

### 2. **Enhanced Error Handling**
- `iostat=` and `iomsg=` parameters for I/O operations
- Structured error propagation
- Comprehensive status checking

### 3. **Modern I/O Operations**
- `newunit=` parameter for automatic unit assignment
- Safe file handle management
- Resource cleanup with error handling

### 4. **Procedure Enhancements**
- `pure` procedures for side-effect-free operations
- `elemental` procedures for array operations
- Explicit `intent` declarations for all parameters

### 5. **Generic Interfaces**
- Type-safe operations with generic procedures
- Overloaded operations for different data types
- Enhanced code reusability

### 6. **Documentation Standards**
- Comprehensive module documentation with `!>` comments
- Parameter documentation explaining purpose and usage
- Clear variable naming conventions

## Migration Benefits

1. **Improved Maintainability**: Self-documenting code with descriptive names
2. **Enhanced Reliability**: Comprehensive error handling and validation
3. **Better Performance**: Pure procedures enable compiler optimizations
4. **Increased Portability**: Standard intrinsic types work across compilers
5. **Modern Development**: Follows current Fortran best practices
6. **Type Safety**: Enhanced type checking and validation

## Backward Compatibility

All legacy parameter names are preserved as aliases to ensure existing code continues to work while gradually migrating to the new descriptive names.

## Build System Updates

The CMake configuration has been updated to:
- Target Fortran 2023 standard
- Enable comprehensive compiler warnings
- Support both legacy and modern modules
- Provide proper module installation

## Next Steps

1. Gradually migrate remaining modules to use modern naming conventions
2. Add comprehensive unit tests using modern testing frameworks
3. Implement coarray parallelization where appropriate
4. Consider object-oriented design for complex data structures
5. Add performance benchmarks to validate optimization benefits
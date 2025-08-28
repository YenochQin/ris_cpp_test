# FORTRAN 2023 MODERNIZATION STRATEGY
# Using -Werror=implicit-interface for Code Refactoring

## Overview

This build configuration is set up to help identify and prioritize Fortran 95 → Fortran 2023 modernization efforts using strict compiler checking.

## Current Configuration

### CMake Version: 3.26+
- **Requirement**: Updated from 3.6 to 3.26 for better Fortran 2023 support
- **Your Version**: 3.31.6 ✅ (fully compatible)
- **Benefits**: Enhanced Fortran language support, better module dependency tracking

### Compiler Flags:
```
-std=f2023                    # Target Fortran 2023 standard
-fno-automatic               # Legacy GRASP requirement
-fallow-argument-mismatch    # Handle MPI compatibility
-Werror=implicit-interface   # 🎯 STRICT: Find refactoring targets
```

## Using -Werror=implicit-interface for Modernization

### Purpose
This flag turns **implicit interface warnings into errors**, helping you identify:

1. **Missing explicit interfaces**
2. **Legacy procedure calls**  
3. **Functions without proper interface blocks**
4. **Subroutines that need modernization**

### Workflow

#### Step 1: Build and Identify Issues
```bash
cd build
make -j$(nproc) 2>&1 | tee build_errors.log
```

The build will **FAIL** at the first file with implicit interfaces, showing you exactly what needs to be modernized first.

#### Step 2: Analyze Error Messages
Look for errors like:
```
Error: Procedure 'some_function' called with implicit interface
Error: Implicit interface for procedure 'some_subroutine'
```

#### Step 3: Prioritization Strategy
**Fix in this order:**
1. **Core libraries first** (libmod, lib9290)
2. **Most referenced functions** (high impact)
3. **Interface modules** (*_I.f90 files)
4. **Application-specific code** (ris4/)

#### Step 4: Modernization Techniques

**For each identified issue:**

**A. Add Explicit Interface:**
```fortran
! OLD (implicit interface):
call some_subroutine(arg1, arg2)

! NEW (explicit interface):
interface
   subroutine some_subroutine(arg1, arg2)
      integer, intent(in) :: arg1
      real, intent(out) :: arg2
   end subroutine
end interface
call some_subroutine(arg1, arg2)
```

**B. Create Interface Module:**
```fortran
module some_interface
   interface
      subroutine some_subroutine(arg1, arg2)
         integer, intent(in) :: arg1
         real, intent(out) :: arg2
      end subroutine
   end interface
end module
```

**C. Modernize Procedure Declaration:**
```fortran
! OLD:
subroutine some_subroutine(arg1, arg2)
integer arg1
real arg2

! NEW:
subroutine some_subroutine(arg1, arg2)
   integer, intent(in) :: arg1
   real, intent(out) :: arg2
```

#### Step 5: Iterative Process
1. Fix the errors shown by `-Werror=implicit-interface`
2. Rebuild to find next set of issues
3. Continue until all implicit interfaces are resolved
4. Remove the `-Werror=implicit-interface` flag for final build

## Expected Modernization Sequence

### Phase 1: Foundation Modules (~1-2 weeks)
- `libmod/` modules with implicit interfaces
- `parameter_def_M.f90`, `vast_kind_param_M.f90`
- Core data structure modules

### Phase 2: Mathematical Libraries (~2-3 weeks)  
- `lib9290/` utility functions
- Interface modules (*_I.f90)
- BLAS/LAPACK interface compatibility

### Phase 3: Specialized Libraries (~1-2 weeks each)
- `libdvd90/` (Davidson diagonalization)
- `libmcp90/` (Multiconfiguration perturbation)
- `librang90/` (Angular momentum - largest library)

### Phase 4: Applications (~1 week)
- `ris4/` application code
- Already partially modernized with `isotope_shift.f90`

## Benefits of This Approach

### ✅ **Systematic**: Compiler guides you to exactly what needs fixing
### ✅ **Prioritized**: Most critical issues surface first  
### ✅ **Comprehensive**: Catches all implicit interface issues
### ✅ **Testable**: Each fix can be verified immediately

## Fallback Strategy

If `-Werror=implicit-interface` is too strict initially:

1. **Use warning mode first:**
   ```bash
   cmake -DCMAKE_Fortran_FLAGS="-std=f2023 -fno-automatic -fallow-argument-mismatch -Wimplicit-interface" ..
   ```

2. **Gradually increase strictness:**
   - Start with `-Wimplicit-interface` (warnings)
   - Progress to `-Werror=implicit-interface` (errors)
   - Add other strict flags as code improves

## Monitoring Progress

Track your modernization progress:
```bash
# Count remaining implicit interface issues
make 2>&1 | grep -c "implicit interface"

# Count interface modules created
find src/ -name "*_I.f90" | wc -l

# Measure code modernization
grep -r "intent(" src/ | wc -l  # Modern parameter declarations
```

## Final Goal

Complete modernization will result in:
- ✅ All procedures have explicit interfaces
- ✅ Modern parameter declarations (intent in/out/inout)
- ✅ Clean compilation with strict Fortran 2023 flags
- ✅ Better code maintainability and safety
- ✅ Foundation for further modernization (pure/elemental procedures, etc.)

---
**Next Step**: Run `make` to see your first batch of implicit interface errors! 🚀
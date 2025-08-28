# FORTRAN DEPENDENCY ANALYSIS REPORT
# Generated for GRASP RIS4 Isotope Shift Calculation System
# Analysis Date: August 28, 2024

## EXECUTIVE SUMMARY
This report maps the module dependencies across the GRASP codebase to prevent
downstream breakages when modifying syntax and modernizing Fortran code.

## 1. CORE MODULE HIERARCHY

### 1.1 Foundation Modules (libmod/)
These are the base modules that most other modules depend on:

**Type/Precision Definitions:**
- `vast_kind_param` (legacy) -> `parameter_def` (legacy) -> `precision_definitions` (modernized)
- Contains: DOUBLE, working_precision_kind, extended_precision_kind

**Core Data Modules:**
- `orb_C` - Orbital definitions and data
- `eigv_C` - Eigenvalue data  
- `prnt_C` - Print control and NVEC parameter
- `debug_C` - Debug settings including CUTOFF threshold
- `default_C` - Default program settings
- `iounit_C` - I/O unit definitions

### 1.2 Mathematical Libraries (lib9290/)
**Interface Modules Required by isotope_shift.f90:**
- `getyn_I` - User interaction (getyn() function)
- `setdbg_I` - Debug setup (setdbg() subroutine)
- `factt_I` - Factorial table initialization (factt() subroutine)

**Other Critical lib9290 interfaces:**
- `setcon_I`, `setiso_I`, `setpot_I` - Configuration setup
- `convrt_I` - Unit conversion utilities
- `quad_I`, `rint_I` - Integration routines

### 1.3 Angular Momentum Library (librang90/)
- Contains ~100+ interface modules for angular coefficient calculations
- Critical for RIS calculations: `dracah_I`, `onescalar_I`, `recop1_I`

## 2. RIS4 APPLICATION DEPENDENCIES

### 2.1 Main Program (isotope_shift.f90)
**DIRECT DEPENDENCIES:**
```fortran
use, intrinsic :: iso_fortran_env, only: error_unit, output_unit  ! Fortran 2023
use precision_definitions, only: working_precision_kind           ! Modernized
use default_C                                                     ! libmod
use iounit_C                                                      ! libmod
use debug_C                                                       ! libmod
use getyn_I, only: getyn                                         ! lib9290
use setdbg_I, only: setdbg                                       ! lib9290
use getmixblock_I, only: getmixblock                             ! ris4
use setcon_I, only: setcon                                       ! lib9290
use setsum_I, only: setsum                                       ! ris4
use strsum_I, only: strsum                                       ! ris4
use factt_I, only: factt                                         ! lib9290
use ris_cal_I, only: ris_cal                                     ! ris4
```

**DEPENDENCY CHAIN:**
isotope_shift.f90 → ris_cal_I → ris_cal.f90 → vast_kind_param + parameter_def + ~50 other modules

### 2.2 RIS4 Interface Modules
**Critical Interfaces (ris4/):**
- `ris_cal_I` - Main calculation interface
- `getmixblock_I` - Mixing coefficient handling  
- `setsum_I` - Calculation summary setup
- `strsum_I` - Structure sum calculations
- `densread_I`, `densnew_I` - Density calculations
- `sigma_1_I`, `sigma_2_I` - Mass shift parameters

## 3. MODERNIZATION IMPACT ANALYSIS

### 3.1 Safe Modifications
**✅ LOW RISK - These can be safely modernized:**
- `isotope_shift.f90` - Already modernized, isolated main program
- `precision_definitions.f90` - New module, provides aliases for backward compatibility
- Interface modules (*_I.f90) - Only contain procedure signatures

**✅ MEDIUM RISK - Careful testing required:**
- Module implementation files (*_C.f90) if only internal changes
- Mathematical utility modules with pure/elemental procedure additions

### 3.2 HIGH RISK Modifications
**⚠️ HIGH RISK - These changes could break downstream dependencies:**

**Parameter/Type Modules:**
- `vast_kind_param_M.f90` - Used by ~200+ files
- `parameter_def_M.f90` - Contains NNNW, NNNP, KEYORB used everywhere
- Any changes to precision kinds or array size parameters

**Core Data Modules:**
- `orb_C.f90` - Orbital data used by all calculation modules
- `eigv_C.f90` - Eigenvalue data structures
- `prnt_C.f90` - Contains NVEC used in array declarations

**Interface Signature Changes:**
- Any modifications to subroutine/function signatures in *_I.f90 files
- Changes to module variable accessibility (PUBLIC/PRIVATE)

### 3.3 Modernization Strategy
**Phase 1 (COMPLETED):** 
- ✅ Main program modernization (isotope_shift.f90)
- ✅ New precision definitions with legacy aliases
- ✅ Fix arithmetic IF statements in isolated modules

**Phase 2 (SAFE):**
- Add pure/elemental attributes to mathematical functions
- Modernize I/O with iostat/iomsg in implementation files
- Add comprehensive documentation

**Phase 3 (REQUIRES FULL TESTING):**
- Migrate from vast_kind_param to precision_definitions
- Update character*N declarations to modern character(len=N)
- Convert labeled DO loops to modern constructs

## 4. CRITICAL DEPENDENCY WARNINGS

### 4.1 Circular Dependencies
**POTENTIAL ISSUES IDENTIFIED:**
- Some *_C modules may have circular dependencies through common blocks
- Interface modules (*_I) depend on implementation modules for compilation

### 4.2 Build Order Requirements
**MANDATORY BUILD SEQUENCE:**
1. libmod/ (foundation modules)
2. lib9290/ (mathematical libraries)  
3. libdvd90/, libmcp90/, librang90/ (specialized libraries)
4. ris4/ application modules
5. Final executable linking

### 4.3 Module File Dependencies (.mod files)
The CMake build system handles .mod file dependencies automatically, but manual compilation requires:
- Foundation modules built first
- Interface modules built before implementation modules
- All dependencies available before final linking

## 5. RECOMMENDATIONS

### 5.1 Before Making Changes
1. **Always test compilation** of dependent modules after any modification
2. **Use git branches** for experimental modernizations
3. **Run full test suite** after any interface changes
4. **Check .mod file timestamps** to ensure proper rebuild

### 5.2 Safe Modernization Practices
1. **Maintain backward compatibility** through aliases
2. **Add new features** alongside old ones before deprecating
3. **Document all interface changes** thoroughly
4. **Use compiler warnings** to identify obsolescent features

### 5.3 Future Fortran 2023 Migration
1. **Create migration scripts** for systematic updates
2. **Establish testing framework** for validation
3. **Plan phased rollout** by dependency levels
4. **Maintain legacy compatibility** during transition

---
END OF DEPENDENCY ANALYSIS REPORT
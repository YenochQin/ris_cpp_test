/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:62:70:

   62 |       if (ierr.ne.0) call mem_fail(6,NS*(NS + 1)/2,'iniest::ap',ierr);
      |                                                                      1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:65:45:

   65 |       CALL DINIT (NS*(NS + 1)/2, 0.D0, AP, 1)
      |                                             1
Error: Procedure 'dinit' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:69:63:

   69 |       if (ierr.ne.0) call mem_fail(6,ns,'iniest::eigval',ierr);
      |                                                               1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:74:64:

   74 |       if (ierr.ne.0) call mem_fail(6,ns*niv,'iniest::vec',ierr);
      |                                                                1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:79:63:

   79 |       if (ierr.ne.0) call mem_fail(6,8*NS,'iniest::work',ierr);
      |                                                               1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:84:64:

   84 |       if (ierr.ne.0) call mem_fail(6,5*NS,'iniest::iwork',ierr);
      |                                                                1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:89:62:

   89 |       if (ierr.ne.0) call mem_fail(6,ns,'iniest::IFAIL',ierr);
      |                                                              1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:94:60:

   94 |       if (ierr.ne.0) call mem_fail(6,ns,'iniest::jsh',ierr);
      |                                                            1
Error: Procedure 'mem_fail' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:121:14:

  121 |          INFO)
      |              1
Error: Procedure 'dspevx' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:132:40:

  132 |       CALL DINIT (N*NIV, 0.D0, BASIS, 1)
      |                                        1
Error: Procedure 'dinit' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/libdvd90/iniest.f90:142:52:

  142 |       CALL DCOPY (NIV, EIGVAL, 1, BASIS(NIV*N+1), 1)
      |                                                    1
Error: Procedure 'dcopy' called with an implicit interface at (1) [-Werror=implicit-interface]
f951: some warnings being treated as errors
make[2]: *** [src/lib/libdvd90/CMakeFiles/dvd90.dir/build.make:234: src/lib/libdvd90/CMakeFiles/dvd90.dir/iniest.f90.o] Error 1
make[2]: *** Waiting for unfinished jobs....
make[1]: *** [CMakeFiles/Makefile2:285: src/lib/libdvd90/CMakeFiles/dvd90.dir/all] Error 2
make[1]: *** Waiting for unfinished jobs....


/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:46:47:

   46 |       CALL DINIT ((NS*(NS + 1))/2, 0.D0, AP, 1)
      |                                               1
Error: Procedure 'dinit' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:69:14:

   69 |          INFO)
      |              1
Error: Procedure 'dspevx' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:74:42:

   74 |       CALL DINIT (NCF*NIV, 0.D0, BASIS, 1)
      |                                          1
Error: Procedure 'dinit' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:79:67:

   79 |          CALL DCOPY (NS, VEC(NS*(J-1)+1), 1, BASIS(NCF*(J-1)+1), 1)
      |                                                                   1
Error: Procedure 'dcopy' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:82:54:

   82 |       CALL DCOPY (NIV, EIGVAL, 1, BASIS(NIV*NCF+1), 1)
      |                                                      1
Error: Procedure 'dcopy' called with an implicit interface at (1) [-Werror=implicit-interface]
/home/qqqyy/AppFiles/ris_test/src/lib/lib9290/iniest2.f90:82:23:

   79 |          CALL DCOPY (NS, VEC(NS*(J-1)+1), 1, BASIS(NCF*(J-1)+1), 1)
      |                         2
......
   82 |       CALL DCOPY (NIV, EIGVAL, 1, BASIS(NIV*NCF+1), 1)
      |                       1
Warning: Rank mismatch between actual argument at (1) and actual argument at (2) (scalar and rank-1)
f951: some warnings being treated as errors


[ 45%] Building Fortran object src/lib/lib9290/CMakeFiles/9290.dir/itrig.f90.o
make[2]: *** [src/lib/lib9290/CMakeFiles/9290.dir/build.make:936: src/lib/lib9290/CMakeFiles/9290.dir/iniest2.f90.o] Error 1
make[2]: *** Waiting for unfinished jobs....

[ 46%] Building Fortran object src/lib/lib9290/CMakeFiles/9290.dir/pack.f90.o
make[1]: *** [CMakeFiles/Makefile2:253: src/lib/lib9290/CMakeFiles/9290.dir/all] Error 2
make: *** [Makefile:146: all] Error 2

---

# ANALYSIS AND SOLUTIONS

## 🎯 **Summary of Errors Found**

The `-Werror=implicit-interface` flag has successfully identified **explicit interface issues** that need to be resolved for Fortran 2023 modernization. The build failed with multiple implicit interface errors.

### **Error Categories:**
1. **BLAS/LAPACK Procedures** - Missing explicit interfaces for external library calls
2. **GRASP Utility Procedures** - Missing interfaces for GRASP utility functions  
3. **Rank Mismatch Issues** - Parameter types don't match interface declarations

---

## 📋 **Detailed Error Breakdown**

### 🔴 **Priority 1: BLAS/LAPACK Missing Interfaces**

**Procedures needing interfaces:**
```fortran
DINIT    - Initialize array with constant value
DCOPY    - Copy vector x to vector y  
DSPEVX   - Symmetric packed eigenvalue solver (LAPACK)
```

**Files affected:**
- `src/lib/libdvd90/iniest.f90` (lines 65, 132, 121, 142)
- `src/lib/lib9290/iniest2.f90` (lines 46, 74, 69, 79, 82)

### 🟡 **Priority 2: GRASP Utility Missing Interfaces**

**Procedures needing interfaces:**
```fortran
mem_fail - Memory allocation failure handler
```

**Files affected:**
- `src/lib/libdvd90/iniest.f90` (lines 62, 69, 74, 79, 84, 89, 94)

### 🟠 **Priority 3: Rank Mismatch Issues**

**Issue:**
```
Warning: Rank mismatch between actual argument at (1) and actual argument at (2) (scalar and rank-1)
```

**Location:**
- `iniest2.f90:82` - DCOPY call with EIGVAL parameter

---

## 🚀 **SOLUTION PLAN**

### **Step 1: Create BLAS/LAPACK Interface Module**

Create `src/lib/libmod/blas_lapack_interfaces.f90`:

```fortran
module blas_lapack_interfaces
   implicit none
   
   interface
      ! Initialize array with scalar value
      subroutine dinit(n, alpha, x, incx)
         integer, intent(in) :: n, incx
         real(kind=8), intent(in) :: alpha
         real(kind=8), intent(out) :: x(*)
      end subroutine dinit
      
      ! Copy vector
      subroutine dcopy(n, dx, incx, dy, incy)
         integer, intent(in) :: n, incx, incy
         real(kind=8), intent(in) :: dx(*)
         real(kind=8), intent(out) :: dy(*)
      end subroutine dcopy
      
      ! LAPACK eigenvalue solver
      subroutine dspevx(jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, &
                        m, w, z, ldz, work, iwork, ifail, info)
         character(len=1), intent(in) :: jobz, range, uplo
         integer, intent(in) :: n, il, iu, ldz
         real(kind=8), intent(in) :: vl, vu, abstol
         real(kind=8), intent(inout) :: ap(*)
         integer, intent(out) :: m, iwork(*), ifail(*), info
         real(kind=8), intent(out) :: w(*), z(ldz, *), work(*)
      end subroutine dspevx
   end interface
   
end module blas_lapack_interfaces
```

### **Step 2: Create GRASP Utilities Interface Module**

Create `src/lib/libmod/grasp_utilities_interfaces.f90`:

```fortran
module grasp_utilities_interfaces
   implicit none
   
   interface
      subroutine mem_fail(unit, size, routine, error_code)
         integer, intent(in) :: unit, size, error_code
         character(len=*), intent(in) :: routine
      end subroutine mem_fail
   end interface
   
end module grasp_utilities_interfaces
```

### **Step 3: Update CMakeLists.txt**

Add to `src/lib/libmod/CMakeLists.txt`:
```cmake
add_library(mod STATIC
    # ... existing files ...
    blas_lapack_interfaces.f90
    grasp_utilities_interfaces.f90
)
```

### **Step 4: Update Problem Files**

Add USE statements to affected files:

**For `src/lib/libdvd90/iniest.f90`:**
```fortran
! Add after existing USE statements:
use blas_lapack_interfaces
use grasp_utilities_interfaces
```

**For `src/lib/lib9290/iniest2.f90`:**
```fortran
! Add after existing USE statements:
use blas_lapack_interfaces
```

### **Step 5: Fix Rank Mismatch**

In `iniest2.f90`, the EIGVAL parameter needs to be declared as an array:
```fortran
! Current (problematic):
real(kind=8) :: EIGVAL

! Should be:
real(kind=8) :: EIGVAL(*)
! OR
real(kind=8), dimension(*) :: EIGVAL
```

---

## ⚡ **Quick Fix Commands**

### **1. Create interface modules:**
```bash
# Will be done in next steps
```

### **2. Test incremental progress:**
```bash
cd build
make -j$(nproc) 2>&1 | grep -c "implicit interface"
```

### **3. After fixes, clean rebuild:**
```bash
make clean && make -j$(nproc)
```

---

## 🎯 **Expected Results After Fixes**

✅ **Clean compilation** with `-Werror=implicit-interface`  
✅ **No more implicit interface errors**  
✅ **No rank mismatch warnings**  
✅ **Working BLAS/LAPACK operations**  
✅ **Proper error handling with mem_fail**

---

## 📈 **Progress Tracking**

- **Errors identified**: ~15+ implicit interface calls
- **Files needing updates**: 2 main files + 2 new interface modules
- **Estimated fix time**: 2-3 hours
- **Current status**: ❌ Errors documented, fixes ready to implement

**Next action**: Create the interface modules and start fixing! 🚀
      MODULE damp_C
!     Arrays associated with damping solutions in the SCF process
!     ... ODAMP:  array for damping radial functions
!     ... CDAMP:  array for damping expansion coefficients
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNW
      real(real64), DIMENSION(NNNW) :: ODAMP
      real(real64), DIMENSION(:), pointer :: cdamp
      END MODULE damp_C

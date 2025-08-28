      MODULE A1JJ_I
      INTERFACE
!
      SUBROUTINE A1JJ (IK, BK, ID, BD, QM1, A)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK, ID
      real(real64), INTENT(IN), DIMENSION(3) :: BK, BD
      real(real64), INTENT(IN)               :: QM1
      real(real64), INTENT(OUT)              :: A
      END SUBROUTINE
      END INTERFACE
      END MODULE

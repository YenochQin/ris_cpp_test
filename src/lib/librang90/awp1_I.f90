      MODULE AWP1_I
      INTERFACE
!
      SUBROUTINE AWP1( IK, BK, ID, BD, K1, BK2, QM1, QM2, QM3, AW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)               :: K1
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK, ID
      real(real64), INTENT(IN)               :: BK2, QM1, QM2, QM3
      real(real64), INTENT(IN), DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT)              :: AW
      END SUBROUTINE
      END INTERFACE
      END MODULE

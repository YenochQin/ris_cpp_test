      MODULE W1JJG_I
      INTERFACE
!
      SUBROUTINE W1JJG(K1,QM1,QM2,IK,BK,ID,BD,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)               :: K1
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK, ID
      real(real64), INTENT(IN)               :: QM1, QM2
      real(real64), INTENT(IN), DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT)              :: WW
      END SUBROUTINE
      END INTERFACE
      END MODULE

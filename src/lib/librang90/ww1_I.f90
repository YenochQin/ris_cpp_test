      MODULE WW1_I
      INTERFACE
!
      SUBROUTINE WW1(IK,BK,ID,BD,K2,QM1,QM2,QM3,QM4,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)               :: K2
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK, ID
      real(real64), INTENT(IN)               :: QM1, QM2, QM3, QM4
      real(real64), INTENT(IN), DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT)              :: WW
      END SUBROUTINE
      END INTERFACE
      END MODULE

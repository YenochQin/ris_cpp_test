      MODULE GG12_I
      INTERFACE
!
      SUBROUTINE GG12(IK1,IK2,BK1,BK2,ID1,ID2,BD1,BD2,QM1,QM2,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK1, IK2, ID1, ID2
      real(real64), INTENT(IN)               :: QM1, QM2
      real(real64), INTENT(IN), DIMENSION(3) :: BK1, BK2, BD1, BD2
      real(real64), INTENT(OUT)              :: WW
      END SUBROUTINE
      END INTERFACE
      END MODULE

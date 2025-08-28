      MODULE GG1234_I
      INTERFACE
!
      SUBROUTINE GG1234(IK1,IK2,IK3,IK4,BK1,BK2,BK3,BK4,ID1,ID2,  &
      ID3,ID4,BD1,BD2,BD3,BD4,QM1,QM2,QM3,QM4,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK1,IK2,IK3,IK4,ID1,ID2,ID3,ID4
      real(real64), INTENT(IN)               :: QM1,QM2,QM3,QM4
      real(real64), INTENT(IN), DIMENSION(3) :: BK1,BK2,BK3,BK4,BD1,BD2,BD3,BD4
      real(real64), INTENT(OUT)              :: WW
      END SUBROUTINE
      END INTERFACE
      END MODULE

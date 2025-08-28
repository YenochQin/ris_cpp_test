      MODULE WAP1JJG_I
      INTERFACE
!
      SUBROUTINE WAP1JJG(K1,BK2,QM1,QM2,QM3,IK,BK,ID,BD,WA)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)               :: K1
      INTEGER,      INTENT(IN), DIMENSION(7) :: IK, ID
      real(real64), INTENT(IN)               :: BK2, QM1, QM2, QM3
      real(real64), INTENT(IN), DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT)              :: WA
      END SUBROUTINE
      END INTERFACE
      END MODULE

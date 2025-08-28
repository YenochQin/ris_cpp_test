      MODULE perko1_I
      INTERFACE
!
      SUBROUTINE PERKO1(JA,BK,IK,BD,ID)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)                :: JA
      INTEGER,      INTENT(OUT), DIMENSION(7) :: IK, ID
      real(real64), INTENT(OUT), DIMENSION(3) :: BK, BD
      END SUBROUTINE
      END INTERFACE
      END MODULE

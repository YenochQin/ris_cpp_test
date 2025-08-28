      MODULE onescalar2_I
      INTERFACE
!
      SUBROUTINE ONESCALAR2(JJA,JJB,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: JJA,JJB,JA,JB
      real(real64), INTENT(OUT) :: COEFF
      END SUBROUTINE
      END INTERFACE
      END MODULE

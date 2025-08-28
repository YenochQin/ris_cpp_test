      MODULE onescalar1_I
      INTERFACE
!
      SUBROUTINE ONESCALAR1(NS,JJA,JJB,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: NS,JJA,JJB,JA,JB
      real(real64), INTENT(OUT) :: COEFF
      END SUBROUTINE
      END INTERFACE
      END MODULE

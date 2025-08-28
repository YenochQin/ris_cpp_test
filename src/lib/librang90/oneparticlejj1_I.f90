      MODULE oneparticlejj1_I
      INTERFACE
!
      SUBROUTINE ONEPARTICLEJJ1(NS,KA,JJA,JJB,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: NS,KA,JJA,JJB,JA,JB
      real(real64), INTENT(OUT) :: COEFF
      END SUBROUTINE
      END INTERFACE
      END MODULE

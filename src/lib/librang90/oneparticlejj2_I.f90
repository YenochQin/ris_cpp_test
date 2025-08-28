      MODULE oneparticlejj2_I
      INTERFACE
!
      SUBROUTINE ONEPARTICLEJJ2(NS,KA,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: NS,KA,JA,JB
      real(real64), INTENT(OUT) :: COEFF
      END SUBROUTINE
      END INTERFACE
      END MODULE

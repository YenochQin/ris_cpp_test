      MODULE rwjj_I
      INTERFACE
!
      SUBROUTINE RWJJ(J,J1,J2,K1,K2,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)  :: J, J1, J2, K1, K2
      real(real64), INTENT(OUT) :: COEF
      END SUBROUTINE
      END INTERFACE
      END MODULE

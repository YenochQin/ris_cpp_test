      MODULE rmeajj9_I
      INTERFACE
!
      SUBROUTINE RMEAJJ9(IT,LQ,J,ITS,LQS,J1S,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)  :: IT, LQ, J, ITS, LQS, J1S
      real(real64), INTENT(OUT) :: COEF
      END SUBROUTINE
      END INTERFACE
      END MODULE

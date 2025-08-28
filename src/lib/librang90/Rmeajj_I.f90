      MODULE rmeajj_I
      INTERFACE
!
      SUBROUTINE RMEAJJ(LL,IT,LQ,J,ITS,LQS,J1S,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)  :: LL, IT, LQ, J, ITS, LQS, J1S
      real(real64), INTENT(OUT) :: COEF
      END SUBROUTINE
      END INTERFACE
      END MODULE

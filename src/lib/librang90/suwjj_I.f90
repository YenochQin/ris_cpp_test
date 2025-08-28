      MODULE SUWJJ_I
      INTERFACE
!
      SUBROUTINE SUWJJ(K1,K2,LL,J1,J2,SUW)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)  :: K1, K2, J1, J2
      INTEGER,      INTENT(OUT) :: LL
      real(real64), INTENT(OUT) :: SUW
      END SUBROUTINE
      END INTERFACE
      END MODULE

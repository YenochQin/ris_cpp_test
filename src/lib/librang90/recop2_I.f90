      MODULE recop2_I
      INTERFACE
!
      SUBROUTINE RECOP2(NS,JA1,JA2,K1,K2,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)       :: NS, JA1, JA2, K1, K2, KA, IRE
      INTEGER, INTENT(OUT)      :: IAT
      real(real64), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE

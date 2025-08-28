      MODULE recop1_I
      INTERFACE
!
      SUBROUTINE RECOP1(NS,JA1,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)       :: NS, JA1, KA, IRE
      INTEGER, INTENT(OUT)      :: IAT
      real(real64), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE

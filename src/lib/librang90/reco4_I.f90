      MODULE reco4_I
      INTERFACE
!
      SUBROUTINE RECO4(JA1,JA2,JA3,JA4,K1,K2,K3,K4,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)       :: JA1,JA2,JA3,JA4,K1,K2,K3,K4,KA,IRE
      INTEGER, INTENT(OUT)      :: IAT
      real(real64), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE

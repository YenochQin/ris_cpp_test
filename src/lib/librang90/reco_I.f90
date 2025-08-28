      MODULE reco_I
      INTERFACE
!
      SUBROUTINE RECO(JA1,JA2,JA3,JA4,KA,IAT)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)  :: JA1, JA2, JA3, JA4, KA
      INTEGER, INTENT(OUT) :: IAT
      END SUBROUTINE
      END INTERFACE
      END MODULE

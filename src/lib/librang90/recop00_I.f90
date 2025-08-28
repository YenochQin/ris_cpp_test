      MODULE recop00_I
      INTERFACE
!
      SUBROUTINE RECOP00(NS,JA1,JA2,KA,IAT)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)  :: NS, JA1, JA2, KA
      INTEGER, INTENT(OUT) :: IAT
      END SUBROUTINE
      END INTERFACE
      END MODULE

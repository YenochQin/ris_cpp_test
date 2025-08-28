      MODULE rec3_I
      INTERFACE
!
      SUBROUTINE REC3(JA1,JA2,JA3,K1,K2,K3,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)       :: JA1,JA2,JA3,K1,K2,K3,IRE
      INTEGER, INTENT(OUT)      :: IAT
      real(real64), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE

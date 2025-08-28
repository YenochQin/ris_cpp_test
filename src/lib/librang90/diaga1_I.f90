      MODULE diaga1_I
      INTERFACE
!
      SUBROUTINE DIAGA1(JA1,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN)       :: JA1,KA,IRE
!      INTEGER, INTENT(OUT)      :: IAT
      INTEGER, INTENT(INOUT)      :: IAT
      real(real64), INTENT(OUT) :: RECC
      END SUBROUTINE
      END INTERFACE
      END MODULE

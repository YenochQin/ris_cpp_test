      MODULE perko2_I
      INTERFACE
      SUBROUTINE PERKO2(JA1,JA2,JA3,JA4,I)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE trk_C
      INTEGER, INTENT(IN) :: JA1, JA2, JA3, JA4, I
      END SUBROUTINE
      END INTERFACE
      END MODULE

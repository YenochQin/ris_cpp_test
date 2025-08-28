      MODULE densread_I
      INTERFACE
!...Translated by Gediminas Gaigalas 11/18/19
      SUBROUTINE densread (DINT1,DINT2,DINT3,                          &
                           DINT4,DINT5,DINT6,                          &
                           DINT7)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNW, NNNP
      USE prnt_C,           ONLY: NVEC
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT1, DINT2,  &
                                          DINT3, DINT4, DINT5, DINT6,  &
                                          DINT7
      END SUBROUTINE
      END INTERFACE
      END MODULE

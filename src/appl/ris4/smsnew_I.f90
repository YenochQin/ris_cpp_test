      MODULE smsnew_I
      INTERFACE
!...Translated by Gediminas Gaigalas 11/18/19
      SUBROUTINE smsnew (DOIT,VINT,VINT2)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY: NNNW
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: VINT, VINT2
      INTEGER, INTENT(IN) :: DOIT
      END SUBROUTINE
      END INTERFACE
      END MODULE

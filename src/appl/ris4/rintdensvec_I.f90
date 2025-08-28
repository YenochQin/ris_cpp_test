      MODULE rintdensvec_I
      INTERFACE
!...Translated by Gediminas Gaigalas 11/18/19
      SUBROUTINE rintdensvec (I, J, DINT1VEC, NRNUC)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNW
      USE grid_C,           ONLY: N
      real(real64), DIMENSION(NNNW,NNNW,N), INTENT(OUT) :: DINT1VEC
      INTEGER, INTENT(IN) :: I
      INTEGER, INTENT(IN) :: J
      INTEGER, INTENT(IN) :: NRNUC
      END SUBROUTINE
      END INTERFACE
      END MODULE

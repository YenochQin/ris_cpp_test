      MODULE densnew_seltz_I
      INTERFACE
!...Translated by Gediminas Gaigalas 11/18/19
      SUBROUTINE densnew_seltz (DOIT,DINT1,DINT2,DINT3,DINT4,DINT5,DINT6,    &
                          DINT7,DINT1VEC,DENS1VEC,NRNUC)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY: NNNW, NNNP
      USE prnt_C,          ONLY : NVEC
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT1
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT2
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT3
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT4
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT5
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT6
      real(real64), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT7
      INTEGER, INTENT(IN) :: NRNUC
      real(real64), DIMENSION(NVEC,NRNUC), INTENT(OUT) :: DENS1VEC
      real(real64), DIMENSION(NNNW,NNNW,NRNUC), INTENT(IN) :: DINT1VEC
      INTEGER, INTENT(IN) :: DOIT
      END SUBROUTINE
      END INTERFACE
      END MODULE

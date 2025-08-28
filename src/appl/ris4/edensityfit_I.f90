      MODULE edensityfit_I
      INTERFACE
!...Translated by Gediminas Gaigalas 11/18/19
      SUBROUTINE edensityfit (XVEC,YVEC,Z,PAR,NRNUC,F,RHO,RES)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNP, NNN1
      real(real64), DIMENSION(NNN1) :: XVEC
      real(real64), DIMENSION(NNNP) :: YVEC
!GG      real(real64), DIMENSION(NNNP) :: XVEC, YVEC
      real(real64), DIMENSION(5) :: P, F
      real(real64), DIMENSION(2) :: PAR
      real(real64) :: Z, DRMS, RHO, RES
      INTEGER      :: NRNUC, NPARFIT
      END SUBROUTINE
      END INTERFACE
      END MODULE

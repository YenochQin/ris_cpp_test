      module edensityfit_I
      use iso_fortran_env, only: real64
      interface
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine edensityfit (XVEC,YVEC,Z,PAR,NRNUC,F,RHO,RES)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNP, NNN1
      real(kind=real64), dimension(NNN1) :: XVEC
      real(kind=real64), dimension(NNNP) :: YVEC
!GG      real(kind=real64), dimension(NNNP) :: XVEC, YVEC
      real(kind=real64), dimension(5) :: P, F
      real(kind=real64), dimension(2) :: PAR
      real(kind=real64) :: Z, DRMS, RHO, RES
      integer      :: NRNUC, NPARFIT
      end subroutine
      end interface
      end module

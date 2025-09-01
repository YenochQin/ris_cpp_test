      module onescalar_I
      interface
!
      subroutine ONESCALAR(JA,JB,IA1,IA2,VSHELL)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  NNNW
!GG      integer NNNW
!GG      parameter (NNNW = 214)
      integer, intent(in)  :: JA,JB
      integer, intent(out) :: IA1,IA2
      real(real64), dimension(NNNW), intent(out) :: VSHELL
      end subroutine
      end interface
      end module

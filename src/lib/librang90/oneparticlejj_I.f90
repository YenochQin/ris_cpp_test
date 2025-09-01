      module oneparticlejj_I
      interface
!
      subroutine ONEPARTICLEJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  NNNW
!GG      integer NNNW
!GG      parameter (NNNW = 214)
      integer, intent(in)  :: KA,IOPAR,JA,JB
      integer, intent(out) :: IA1,IA2
      real(kind=real64), dimension(NNNW), intent(out) :: VSHELL
      end subroutine
      end interface
      end module

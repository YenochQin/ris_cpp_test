      module WW1_I
      interface
!
      subroutine WW1(IK,BK,ID,BD,K2,QM1,QM2,QM3,QM4,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)               :: K2
      integer,      intent(in), dimension(7) :: IK, ID
      real(kind=real64), intent(in)               :: QM1, QM2, QM3, QM4
      real(kind=real64), intent(in), dimension(3) :: BK, BD
      real(kind=real64), intent(out)              :: WW
      end subroutine
      end interface
      end module

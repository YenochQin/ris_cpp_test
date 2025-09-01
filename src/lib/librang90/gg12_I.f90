      module GG12_I
      interface
!
      subroutine GG12(IK1,IK2,BK1,BK2,ID1,ID2,BD1,BD2,QM1,QM2,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in), dimension(7) :: IK1, IK2, ID1, ID2
      real(kind=real64), intent(in)               :: QM1, QM2
      real(kind=real64), intent(in), dimension(3) :: BK1, BK2, BD1, BD2
      real(kind=real64), intent(out)              :: WW
      end subroutine
      end interface
      end module

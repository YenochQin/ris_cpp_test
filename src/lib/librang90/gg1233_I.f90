      module GG1233_I
      interface
!
      subroutine GG1233(IK1,IK2,IK3,BK1,BK2,BK3,ID1,ID2,ID3,BD1,  &
      BD2,BD3,K1,QM1,QM2,QM3,QM4,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK1,IK2,IK3,ID1,ID2,ID3
      real(real64), intent(in)               :: QM1,QM2,QM3,QM4
      real(real64), intent(in), dimension(3) :: BK1,BK2,BK3,BD1,BD2,BD3
      real(real64), intent(out)              :: WW
      end subroutine
      end interface
      end module

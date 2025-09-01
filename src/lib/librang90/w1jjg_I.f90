      module W1JJG_I
      interface
!
      subroutine W1JJG(K1,QM1,QM2,IK,BK,ID,BD,WW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK, ID
      real(kind=real64), intent(in)               :: QM1, QM2
      real(kind=real64), intent(in), dimension(3) :: BK, BD
      real(kind=real64), intent(out)              :: WW
      end subroutine
      end interface
      end module

      module WJ1_I
      interface
!
      subroutine WJ1(IK,BK,ID,BD,K2,QM1,QM2,WJ)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)               :: K2
      integer,      intent(in), dimension(7) :: IK, ID
      real(kind=real64), intent(in)               :: QM1, QM2
      real(kind=real64), intent(in), dimension(3) :: BK, BD
      real(kind=real64), intent(out)              :: WJ
      end subroutine
      end interface
      end module

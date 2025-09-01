      module AWP1_I
      interface
!
      subroutine AWP1( IK, BK, ID, BD, K1, BK2, QM1, QM2, QM3, AW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK, ID
      real(kind=real64), intent(in)               :: BK2, QM1, QM2, QM3
      real(kind=real64), intent(in), dimension(3) :: BK, BD
      real(kind=real64), intent(out)              :: AW
      end subroutine
      end interface
      end module

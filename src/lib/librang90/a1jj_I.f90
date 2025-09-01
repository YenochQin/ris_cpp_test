      module A1JJ_I
      interface
!
      subroutine A1JJ (IK, BK, ID, BD, QM1, A)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in), dimension(7) :: IK, ID
      real(real64), intent(in), dimension(3) :: BK, BD
      real(real64), intent(in)               :: QM1
      real(real64), intent(out)              :: A
      end subroutine
      end interface
      end module

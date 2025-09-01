      module rmeajj11_I
      interface
!
      subroutine RMEAJJ11(J1,J2,LL,S)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)  :: J1, J2, LL
      real(real64), intent(out) :: S
      end subroutine
      end interface
      end module

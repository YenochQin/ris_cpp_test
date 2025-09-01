      module SUWJJ_I
      interface
!
      subroutine SUWJJ(K1,K2,LL,J1,J2,SUW)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)  :: K1, K2, J1, J2
      integer,      intent(out) :: LL
      real(kind=real64), intent(out) :: SUW
      end subroutine
      end interface
      end module

      module rmew3jj_I
      interface
!
      subroutine RMEW3JJ(J1,J2,K1,K2,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)  :: J1, J2, K1, K2
      real(real64), intent(out) :: COEF
      end subroutine
      end interface
      end module

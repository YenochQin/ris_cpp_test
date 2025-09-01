      module rmeajj_I
      interface
!
      subroutine RMEAJJ(LL,IT,LQ,J,ITS,LQS,J1S,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)  :: LL, IT, LQ, J, ITS, LQS, J1S
      real(real64), intent(out) :: COEF
      end subroutine
      end interface
      end module

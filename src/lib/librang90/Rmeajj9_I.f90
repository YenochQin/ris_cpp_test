      module rmeajj9_I
      interface
!
      subroutine RMEAJJ9(IT,LQ,J,ITS,LQS,J1S,COEF)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)  :: IT, LQ, J, ITS, LQS, J1S
      real(kind=real64), intent(out) :: COEF
      end subroutine
      end interface
      end module

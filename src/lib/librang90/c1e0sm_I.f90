      module c1e0sm_I
      interface
!
      subroutine C1E0SM(Q,QM,C,CM,A)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(real64), intent(in)  :: Q, QM, C, CM
      real(real64), intent(out) :: A
      end subroutine
      end interface
      end module

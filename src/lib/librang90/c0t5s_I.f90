      module c0t5s_I
      interface
!
      subroutine C0T5S(Q, QM, SM, C, CM, A)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(real64), intent(in)  :: Q, QM, SM, C, CM
      real(real64), intent(out) :: A
      end subroutine
      end interface
      end module

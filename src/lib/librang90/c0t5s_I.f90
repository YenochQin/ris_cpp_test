      MODULE c0t5s_I
      INTERFACE
!
      SUBROUTINE C0T5S(Q, QM, SM, C, CM, A)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(real64), INTENT(IN)  :: Q, QM, SM, C, CM
      real(real64), INTENT(OUT) :: A
      END SUBROUTINE
      END INTERFACE
      END MODULE

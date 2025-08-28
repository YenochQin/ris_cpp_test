      MODULE c1e0sm_I
      INTERFACE
!
      SUBROUTINE C1E0SM(Q,QM,C,CM,A)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(real64), INTENT(IN)  :: Q, QM, C, CM
      real(real64), INTENT(OUT) :: A
      END SUBROUTINE
      END INTERFACE
      END MODULE

      MODULE cle0sm_I
      INTERFACE
!
      SUBROUTINE CLE0SM(Q,QM,S,C,CM,A)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(real64), INTENT(IN)  :: Q, QM, S, C, CM
      real(real64), INTENT(OUT) :: A
      END SUBROUTINE
      END INTERFACE
      END MODULE

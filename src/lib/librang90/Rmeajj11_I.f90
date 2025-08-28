      MODULE rmeajj11_I
      INTERFACE
!
      SUBROUTINE RMEAJJ11(J1,J2,LL,S)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)  :: J1, J2, LL
      real(real64), INTENT(OUT) :: S
      END SUBROUTINE
      END INTERFACE
      END MODULE

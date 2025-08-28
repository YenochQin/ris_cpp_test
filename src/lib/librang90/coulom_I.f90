      MODULE coulom_I
      INTERFACE
!
      SUBROUTINE COULOM(J1,J2,J3,J4,L1,L2,L3,L4,K,AA)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: J1, J2, J3, J4, L1, L2, L3, L4, K
      real(real64),INTENT(OUT)  :: AA
      END SUBROUTINE
      END INTERFACE
      END MODULE

      MODULE gracah1_I
      INTERFACE
!
      SUBROUTINE GRACAH1(I,J,K,L,M,N,RAC)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER MFACT
      PARAMETER (MFACT = 500)
      INTEGER , INTENT(IN) :: I
      INTEGER , INTENT(IN) :: J
      INTEGER , INTENT(IN) :: K
      INTEGER , INTENT(IN) :: L
      INTEGER , INTENT(IN) :: M
      INTEGER , INTENT(IN) :: N
      real(real64) , INTENT(OUT) :: RAC
      END SUBROUTINE
      END INTERFACE
      END MODULE

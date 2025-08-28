      MODULE onescalar_I
      INTERFACE
!
      SUBROUTINE ONESCALAR(JA,JB,IA1,IA2,VSHELL)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY:  NNNW
!GG      INTEGER NNNW
!GG      PARAMETER (NNNW = 214)
      INTEGER, INTENT(IN)  :: JA,JB
      INTEGER, INTENT(OUT) :: IA1,IA2
      real(real64), DIMENSION(NNNW), INTENT(OUT) :: VSHELL
      END SUBROUTINE
      END INTERFACE
      END MODULE

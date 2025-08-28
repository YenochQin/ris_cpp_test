      MODULE oneparticlejj_I
      INTERFACE
!
      SUBROUTINE ONEPARTICLEJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL)
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY:  NNNW
!GG      INTEGER NNNW
!GG      PARAMETER (NNNW = 214)
      INTEGER, INTENT(IN)  :: KA,IOPAR,JA,JB
      INTEGER, INTENT(OUT) :: IA1,IA2
      real(real64), DIMENSION(NNNW), INTENT(OUT) :: VSHELL
      END SUBROUTINE
      END INTERFACE
      END MODULE

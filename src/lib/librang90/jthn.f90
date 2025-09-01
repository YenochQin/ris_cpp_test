!*******************************************************************
!                                                                  *
      integer FUNCTION JTHN(K,N,I)
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vanderbilt University,  Nashville               October  1996  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      IMPLICIT NONE
      integer, intent(in) :: I, N, K
!-----------------------------------------------
      if(N == 1) then
        JTHN=MOD(K,I)
      elseif(N == 2) then
        JTHN=MOD(K/I,I)
      elseif(N == 3) then
        JTHN=MOD(K/(I*I),I)
      elseif(N == 4) then
        JTHN=MOD(K/(I*I*I),I)
      elseif(N == 5) then
        JTHN=MOD(K/(I*I*I*I),I)
      else
        WRITE(6,'(A)') ' ERROR IN JTHN '
        STOP
      endif
      return
      END FUNCTION JTHN

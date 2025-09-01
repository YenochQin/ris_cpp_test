!*******************************************************************
!                                                                  *
      integer FUNCTION IXJTIK (I, J, K, L, M, N)
!                                                                  *
!     CHESKED TRIANGULAR CONDITIONS FOR 6j COEFFICIENT             *
!                                                                  *
!     | I/2  J/2  K/2 |            IXJTIK=1 - if NOT SATISFY       *
!     | L/2  M/2  N/2 |            IXJTIK=0 - IN OVER CASES        *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             December 1993  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: I, J, K, L, M, N
!-----------------------------------------------
      IXJTIK = 0
      if (ITTK(I,J,K) == 0) return
      if (ITTK(I,M,N) == 0) return
      if (ITTK(L,J,N) == 0) return
      if (ITTK(L,M,K) == 0) return
      IXJTIK = 1
      return
      END FUNCTION IXJTIK

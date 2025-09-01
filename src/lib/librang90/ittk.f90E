!*******************************************************************
!                                                                  *
      integer FUNCTION ITTK (I, J, K)
!                                                                  *
!     CHESKED TRIANGULAR CONDITIONS FOR   I/2, J/2, K/2.           *
!     I+J>=K, I+K>=J, J+K>=I,                                      *
!     I/2+J/2+K/2 - WHOLE NUMBER                                   *
!     ITTK=1 -   if NOT SATISFY                                    *
!     ITTK=0 -   IN OVER CASES                                     *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             December 1993  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: I, J, K
!-----------------------------------------------
      ITTK = 0
      if (IABS(I - J) > K) return
      if (I + J < K) return
      if (MOD(I + J + K,2) /= 0) return
      ITTK = 1
      return
      END FUNCTION ITTK

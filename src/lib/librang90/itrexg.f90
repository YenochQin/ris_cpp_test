!*******************************************************************
!                                                                  *
      integer FUNCTION ITREXG(I1,I2,I3,I4,K)
!
!   Written by G. Gaigalas,                                        *
!   Vanderbilt University,  Nashville               October  1996  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: I1, I2, I3, I4
      integer, intent(out) :: K
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: J
!-----------------------------------------------
      J=MAX0(IABS(I1-I2),IABS(I3-I4))
      K=MIN0(IABS(I1+I2),IABS(I3+I4))-J+1
      ITREXG=J
      return
      END FUNCTION ITREXG

!***********************************************************************
!                                                                      *
      integer FUNCTION ITRIG (I1, I2, I3)
!                                                                      *
!   The  triangular delta. Input: Values of 2*J+1; Output: 1, if J'S   *
!   form a triangle; 0, otherwise.                                     *
!                                           Last update: 09 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:47   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: I1
      integer, intent(in) :: I2
      integer, intent(in) :: I3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I4
!-----------------------------------------------
!
      I4 = I2 - I3
      if (I1>=ABS(I4) + 1 .AND. I1<=I2+I3-1) then
         ITRIG = 1
      else
         ITRIG = 0
      endif
!
      return
      END FUNCTION ITRIG

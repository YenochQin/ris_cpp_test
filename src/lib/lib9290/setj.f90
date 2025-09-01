!***********************************************************************
!                                                                      *
      subroutine SETJ(IS, JS, KS, NS, KJ23)
!                                                                      *
!   Sets the tables required by  the recoupling  coefficient package   *
!
!
!-----------------------------------------------
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:36   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use L1_C
      use L2_C
      use M_C,      only: JJC1, JJC2, JJQ1, JJQ2, JLIST
      use COUPLE_C, only: MJA, NJA, J1, J2, J3, MANGM
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: NS
      integer, INTENT(INOUT) :: KJ23
      integer, dimension(2,2), intent(in) :: IS
      integer, dimension(2,2), intent(in) :: JS
      integer, dimension(2,2), intent(in) :: KS
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: II, IJ, I, NS1, NS2, NS3, IJ1, IJ2, J
!-----------------------------------------------
!
!
!   1.0  Set J1 array
!
      II = 0
      if (NS > 0) then
         J1(:NS) = JBQ1(3,JLIST(:NS))
         II = NS
      endif
      if (NS /= 1) then
         NS1 = NS - 1
         if (NS1 > 0) then
            J1(II+1:NS1+II) = JJC1(:NS1)
            II = NS1 + II
         endif
         if (NS1 > 0) then
            J1(II+1:NS1+II) = JJC2(:NS1)
            II = NS1 + II
         endif
      endif
      DO I = 1, 2
         II = II + 1
         IJ = IS(I,1)
         J1(II) = JJQ1(3,IJ)
         if (I==1 .AND. IS(1,1)==IS(2,1)) J1(II) = JTQ1(3)
         J1(II+4) = KS(I,1)
      END DO
      DO I = 1, 2
         II = II + 1
         IJ = IS(I,2)
         J1(II) = JJQ2(3,IJ)
         if (I==1 .AND. IS(1,2)==IS(2,2)) J1(II) = JTQ2(3)
         J1(II+4) = KS(I,2)
      END DO
!
!   2.0  Set J2, J3 arrays if not already available
!
      NS2 = MAX(4,NS + 2)
      if (KJ23 <= 0) then
!
         DO I = 4, NS2
            J2(I,1) = NS + I - 4
            J2(I,2) = I - 2
            J2(I,3) = NS + I - 3
            J3(I,1) = J2(I,1) + NS - 1
            J3(I,2) = I - 2
            J3(I,3) = J2(I,3) + NS - 1
         END DO
         J2(4,1) = 1
         J3(4,1) = 1
!
!   At this stage, the entries in rows corresponding to active
!   shells are set incorrectly.
!
!   3.0  Set rows 1 through 3
!
         NS3 = 3*NS
         J2(1,1) = NS3 + 5
         J2(1,2) = NS3 + 7
         J2(1,3) = NS3 + 3
         J2(2,1) = JS(1,1)
         J2(2,2) = NS3 + 3
         J2(2,3) = NS3 - 1
         J2(3,1) = JS(2,1)
         J2(3,2) = NS3 + 4
         J2(3,3) = NS3
!
         J3(1,1) = NS3 + 7
         J3(1,2) = NS3 + 4
         J3(1,3) = NS3 + 6
         J3(2,1) = JS(1,2)
         J3(2,2) = NS3 + 5
         J3(2,3) = NS3 + 1
         J3(3,1) = JS(2,2)
         J3(3,2) = NS3 + 6
         J3(3,3) = NS3 + 2
!
!   4.0  Set remaining resultants
!
         IJ1 = JS(1,1)
         IJ2 = JS(2,1)
         if (IJ2 > 1) J2(IJ2+2,2) = J2(3,3)
         if (IJ2 == 1) J2(4,1) = J2(3,3)
         if (IJ1 == IJ2) then
            J2(3,1) = J2(2,3)
         else
!
            if (IJ1 > 1) J2(IJ1+2,2) = J2(2,3)
            if (IJ1 == 1) J2(4,1) = J2(2,3)
         endif
!
         IJ1 = JS(1,2)
         IJ2 = JS(2,2)
         if (IJ2 > 1) J3(IJ2+2,2) = J3(3,3)
         if (IJ2 == 1) J3(4,1) = J3(3,3)
         if (IJ1 == IJ2) then
            J3(3,1) = J3(2,3)
         else
!
            if (IJ1 > 1) J3(IJ1+2,2) = J3(2,3)
            if (IJ1 == 1) J3(4,1) = J3(2,3)
         endif
!
!   All arrays now set. Put up flag KJ23.
!
         KJ23 = 1
         MJA = NS3 + 7
         NJA = NS + 3
!
!   5.0  Save J2, J3 and return
!
         J2S(:NS2,:) = J2(:NS2,:)
         J3S(:NS2,:) = J3(:NS2,:)
         return
      endif
!
!   6.0  Reset J2, J3 from buffers if KJ23 has been set
!
      J2(:NS2,:) = J2S(:NS2,:)
      J3(:NS2,:) = J3S(:NS2,:)
      return
!
      end subroutine SETJ

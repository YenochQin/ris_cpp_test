!*******************************************************************
!                                                                  *
      subroutine RECOONESCALAR(NS,JA1,JA2,JA3,JA4,KA,IAT)
!                                                                  *
!     -------------  SECTION REC    SUBPROGRAM 05  --------------  *
!                                                                  *
!     NO subroutine CALLED                                         *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use m_C,          only: NPEEL, JLIST, JJC1, JJC2, JJQ1, JJQ2
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: NS, JA1, JA2, JA3, JA4, KA
      integer, intent(out) :: IAT
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I, IA1, IA2, IJ, IJ1, IJ2, J, NPEELGG
!-----------------------------------------------
      IAT=1
      if(NPEEL == 1 .AND. NS == -1)return
      if(NS == -1) then
         NPEELGG = NPEEL
      else
         NPEELGG = NS
      END if
      IJ1=JLIST(JA1)
      IJ2=JLIST(JA2)
      if(JA1 == 1.AND.JA2 == 2) GO TO 1
      if(KA /= 0)GO TO 5
!
!  CASES WHEN :          KA = 0
!                  OR    JA1 = JA2
!                  OR    JA1 = 1    JA2 = 2
!
    1 DO I=1,NPEELGG
        IJ=JLIST(I)
        if(I < NPEELGG-1) then
         if(JJC1(I) /= JJC2(I))IAT=0
        END if
        if(KA /= 0) then
          if(I == JA1) CYCLE
          if(I == JA2) CYCLE
        END if
        DO J=1,3
          if(JJQ1(J,IJ) /= JJQ2(J,IJ))IAT=0
        END DO
      END DO
      return
!
!  OTHER CASES
!
    5 CONTINUE
      DO I=1,NPEELGG
        IJ=JLIST(I)
        if(I < NPEELGG-1) then
          IA1=JA1-1
          IA2=JA2-1
          if(JA1 == 1)IA1=JA1
          if(I >= IA1.AND.I < IA2)GO TO 7
          if(JJC1(I) /= JJC2(I))IAT=0
        END if
    7   if(I == JA1) CYCLE
        if(I == JA2) CYCLE
        if((KA == 2).AND.(I == JA3)) CYCLE
        if((KA == 3).AND.(I == JA3)) CYCLE
        if((KA == 3).AND.(I == JA4)) CYCLE
        DO J=1,3
          if(JJQ1(J,IJ) /= JJQ2(J,IJ))IAT=0
        END DO
      END DO
      return
      end subroutine RECOONESCALAR

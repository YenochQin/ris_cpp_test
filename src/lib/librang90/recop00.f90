!*******************************************************************
!                                                                  *
      subroutine RECOP00(NS,JA1,JA2,KA,IAT)
!                                                                  *
!   ---------------  SECTION REC    SUBPROGRAM 06  --------------  *
!                                                                  *
!     subroutine CALLED:  DIAGA1,DIAGA2,DIAGA3                     *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use m_C,             only: NPEEL, JJC1, JJC2, JLIST, JJQ1, JJQ2
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: NS, JA1, JA2, KA
      integer, intent(out) :: IAT
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I, ISKR, IJ1, J, JI, JJ, NPEELGG, KK, LK, LD
!-----------------------------------------------
      IAT=1
      if(NPEEL == 1 .AND. NS == -1)return
      IAT=0
      if(NS == -1) then
         NPEELGG = NPEEL
      else
         NPEELGG = NS
      END if
      if(NPEELGG > 1) then
        LK=JJC1(NPEELGG-1)-1
        LD=JJC2(NPEELGG-1)-1
      else if(NPEELGG == 1) then
        LK=JJC1(NPEELGG)-1
        LD=JJC2(NPEELGG)-1
      else
        PRINT*, "ERROR in RECOP00"
        STOP
      END if
      if(ITTK(LK,LD,2*KA) == 0)return
      IAT=1
      if(NPEEL == 1)return
      DO I=1,NPEEL
        if(JA1 /= I) then
          if(JA2 /= I) then
            IJ1=JLIST(I)
            if(JJQ1(1,IJ1) /= JJQ2(1,IJ1))IAT=0
            if(JJQ1(2,IJ1) /= JJQ2(2,IJ1))IAT=0
            if(JJQ1(3,IJ1) /= JJQ2(3,IJ1))IAT=0
          endif
        endif
      END DO
      if(IAT == 0)return
      if(NPEELGG <= 2)return
      if(JA1 <= 2)return
      DO J=3,JA1
        JJ=J-2
        if(JJC1(JJ) /= JJC2(JJ))IAT=0
        if(IAT == 0)return
      END DO
      ISKR=NPEELGG-JA2
      if(ISKR > 0) then
        DO JI=1,ISKR
          KK=JA2-2+JI
          LK=JJC1(KK)-1
          LD=JJC2(KK)-1
          if(ITTK(LK,LD,2*KA) == 0)IAT=0
          if(IAT == 0)return
        END DO
      endif
      return
      end subroutine RECOP00

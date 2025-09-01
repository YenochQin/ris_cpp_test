!*******************************************************************
!                                                                  *
      subroutine RECOP2(NS,JA1,JA2,K1,K2,KA,IRE,IAT,RECC)
!                                                                  *
!   ---------------  SECTION REC    SUBPROGRAM 06  --------------  *
!                                                                  *
!     subroutine CALLED:  DIAGA1,DIAGA2,DIAGA3                     *
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
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ONE
      use m_C,             only: JLIST, JJQ1, JJQ2, NPEEL
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use diaga1_I
      use diaga3_I
      use diaga4_I
      use diaga5_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: NS, JA1, JA2, K1, K2, KA, IRE
      integer, intent(out)      :: IAT
      real(real64), intent(out) :: RECC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IJ, ISKR, NPEELGG
      real(real64) :: S, SS, RE
!-----------------------------------------------
      IAT=1
      IJ=JLIST(JA1)
      S=DBLE(JJQ1(3,IJ))
      IJ=JLIST(JA2)
      SS=DBLE(JJQ1(3,IJ))
      SS=S*SS
      RECC=ONE/DSQRT(SS)
      if(NPEEL == 1 .AND. NS == -1)return
      if(NS == -1) then
         NPEELGG = NPEEL
      else
         NPEELGG = NS
      END if
      IAT=0
      ISKR=NPEELGG-JA2
      if(ISKR > 1) then
        CALL DIAGA3(JA2,NPEELGG,2*KA,IRE,IAT,RE)
        if(IAT == 0)return
        RECC=RE*RECC
        IAT=0
      END if
      if(JA2 /= NPEELGG) then
        CALL DIAGA5(NPEELGG,JA2,2*KA,IRE,IAT,RE)
        if(IAT == 0)return
        RECC=RE*RECC
        IAT=0
      endif
      CALL DIAGA4(JA1,JA2,K1,K2,2*KA,IRE,IAT,RE)
      if(IAT == 0)return
      RECC=RE*RECC
      if(JA1 == 1.AND.JA2 == 2)return
      IAT=0
      CALL DIAGA1(JA1,K1,IRE,IAT,RE)
      if(IAT == 0)return
      RECC=RE*RECC
      ISKR=JA2-JA1
      if(JA1 == 1)ISKR=JA2-1-JA1
      if(ISKR <= 1)return
      IAT=0
      CALL DIAGA3(JA1,JA2,K1,IRE,IAT,RE)
      RECC=RE*RECC
      return
      end subroutine RECOP2

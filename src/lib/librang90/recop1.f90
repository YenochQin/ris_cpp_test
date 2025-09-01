!*******************************************************************
!                                                                  *
      subroutine RECOP1(NS,JA1,KA,IRE,IAT,RECC)
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
      use m_C,             only: JLIST, JJQ1, NPEEL
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use diaga1_I
      use diaga3_I
      use diaga5_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: NS, JA1, KA, IRE
      integer, intent(out)      :: IAT
      real(real64), intent(out) :: RECC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: IJ1, ISKR, NPEELGG
      real(real64) :: S, RE
!-----------------------------------------------
      IAT=1
      IJ1=JLIST(JA1)
      S=DBLE(JJQ1(3,IJ1))
      RECC=ONE/DSQRT(S)
      if(NPEEL == 1 .AND. NS == -1)return
      if(NS == -1) then
         NPEELGG = NPEEL
      else
         NPEELGG = NS
      END if
      IAT=0
      if(IRE /= 0) then
        if(KA == 0) then
          IAT=1
          return
        END if
      END if
      IAT=1
      if(NPEELGG == 1) return
      IAT=0
      if(NPEELGG /= 2) then
        CALL DIAGA5(NPEELGG,JA1,2*KA,IRE,IAT,RE)
        RECC=RE*RECC
        if(IAT == 0) return
        if(JA1 == NPEELGG) return
        IAT=0
      END if
      CALL DIAGA1(JA1,2*KA,IRE,IAT,RE)
      if(IAT == 0)return
      RECC=RE*RECC
      if(NPEELGG == 2) return
      ISKR=NPEELGG-JA1
      if(JA1 == 1)ISKR=NPEELGG-1-JA1
      if(ISKR <= 1)return
      IAT=0
      CALL DIAGA3(JA1,NPEELGG,2*KA,IRE,IAT,RE)
      RECC=RE*RECC
      return
      end subroutine RECOP1

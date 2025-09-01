!*******************************************************************
!                                                                  *
      subroutine RECO2(JA1,JA2,KA,IRE,IAT,RECC)
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
      use m_C,             only: JLIST, JJQ1, JJQ2
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use diaga1_I
      use diaga2_I
      use diaga3_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: JA1,JA2,KA,IRE
      integer, intent(out)      :: IAT
      real(real64), intent(out) :: RECC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: IA1,IA2,IB1,IB2,IJ1,IJ2,ISKR
      real(real64) :: S, SS, RE
!-----------------------------------------------
      IAT=0
      IJ1=JLIST(JA1)
      IJ2=JLIST(JA2)
      S=DBLE(JJQ1(3,IJ1))
      SS=DBLE(JJQ1(3,IJ2))
      SS=S*SS
      RECC=ONE/DSQRT(SS)
      if(IRE == 0) then
        IAT=0
      else if(KA /= 0) then
        IAT=0
      else
        IAT=1
        return
      END if
      IA1=JJQ1(3,IJ1)-1
      IA2=JJQ1(3,IJ2)-1
      IB1=JJQ2(3,IJ1)-1
      IB2=JJQ2(3,IJ2)-1
!
      CALL DIAGA2(JA1,JA2,KA,IRE,IAT,RE)
      if(IAT == 0)return
      RECC=RE*RECC*DSQRT(DBLE(IA2+1))/DSQRT(DBLE((KA+1)*(IB2+1)))
      if(JA1 == 1.AND.JA2 == 2)return
!
      IAT=0
      CALL DIAGA1(JA1,KA,IRE,IAT,RE)
      if(IAT == 0)return
      RECC=RE*RECC
      ISKR=JA2-JA1
      if(JA1 == 1)ISKR=JA2-1-JA1
      if(ISKR <= 1)return
!
      IAT=0
      CALL DIAGA3(JA1,JA2,KA,IRE,IAT,RE)
      RECC=RE*RECC
      return
      end subroutine RECO2

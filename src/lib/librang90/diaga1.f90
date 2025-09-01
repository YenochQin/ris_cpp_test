!*******************************************************************
!                                                                  *
      subroutine DIAGA1(JA1,KA,IRE,IAT,RECC)
!                                                                  *
!   ---------------  SECTION REC    SUBPROGRAM 01  --------------  *
!                                                                  *
!     subroutine CALLED:  IXJTIK, SIXJ                             *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO
      use m_C,             only: JLIST, JJQ1, JJQ2, JJC1, JJC2
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ixjtik_I
      use sixj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: JA1, KA, IRE
!      integer, intent(out)      :: IAT
      integer, INTENT(INOUT)      :: IAT
      real(kind=real64), intent(out) :: RECC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IJ1,IA1,IB1,K1,J1,IT1,IT1S,ifAZ,LL1
      real(kind=real64) :: A1
!-----------------------------------------------
      RECC = ZERO
      IJ1=JLIST(JA1)
      IA1=JJQ1(3,IJ1)-1
      IB1=JJQ2(3,IJ1)-1
      if(JA1 <= 2) then
        LL1=JLIST(1)
        if(JA1 == 1)LL1=JLIST(2)
        J1=JJQ1(3,LL1)-1
        IT1=JJC1(1)-1
        IT1S=JJC2(1)-1
      else
        K1=JA1-2
        J1=JJC1(K1)-1
        K1=JA1-1
        IT1=JJC1(K1)-1
        IT1S=JJC2(K1)-1
      END if
      if(IRE /= 0) then
        CALL SIXJ(KA,IB1,IA1,J1,IT1,IT1S,0,A1)
        A1=A1*DSQRT(DBLE((IA1+1)*(IT1S+1)))
        ifAZ=J1+IT1+IB1+KA
        if((ifAZ/4)*4 /= ifAZ)A1=-A1
        RECC=A1
        IAT=1
        if(JA1 /= 1)return
        ifAZ=IA1+IB1+2*J1-IT1-IT1S
        if((ifAZ/4)*4 /= ifAZ)RECC=-RECC
      else
        if(IXJTIK(KA,IB1,IA1,J1,IT1,IT1S) == 0)return
        IAT=1
      END if
      return
      end subroutine DIAGA1

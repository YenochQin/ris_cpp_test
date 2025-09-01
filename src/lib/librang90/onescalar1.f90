!*******************************************************************
!                                                                  *
      subroutine ONESCALAR1(NS,JJA,JJB,JA,JB,COEFF)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 03  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF ONE PARTICLE OPERATOR IN CASE :           N'1 = N1        *
!                                                  N'2 = N2        *
!                                                                  *
!      subroutine CALLED:                                          *
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
      use CONS_C,          only: ZERO, HALF, ONE, EPS
      use trk_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use recoonescalar_I
      use perko2_I
      use wj1_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: NS,JJA,JJB,JA,JB
      real(kind=real64), intent(out) :: COEFF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: IAT
      real(kind=real64) :: WJ,QM1,QM2,RECOUPL
!-----------------------------------------------
!
!     THE CASE 1111   + + - -
!
      COEFF=ZERO
      if(JA == JB) then
        if(JJA /= JJB) then
          CALL RECOONESCALAR(NS,JA,JA,JA,JA,0,IAT)
          if(IAT == 0)return
        END if
        CALL PERKO2(JA,JA,JA,JA,1)
        QM1=HALF
        QM2=-HALF
        CALL WJ1(IK1,BK1,ID1,BD1,0,QM1,QM2,WJ)
        if(DABS(WJ) > EPS) then
           RECOUPL=ONE/DSQRT(DBLE(IK1(6)+1))
           COEFF=WJ*RECOUPL*DSQRT(DBLE(ID1(3)+1))
           COEFF=-COEFF
        END if
      END if
      return
      end subroutine ONESCALAR1

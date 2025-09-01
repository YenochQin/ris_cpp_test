!*******************************************************************
!                                                                  *
      subroutine ONEPARTICLEJJ1(NS,KA,JJA,JJB,JA,JB,COEFF)
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
      use CONS_C,          only: ZERO, HALF, EPS
      use trk_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use recop00_I
      use recop1_I
      use recop2_I
      use wj1_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: NS,KA,JJA,JJB,JA,JB
      real(kind=real64), intent(out) :: COEFF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: IAT
      real(kind=real64) :: REC,WJ,QM1,QM2
!-----------------------------------------------
!
!     THE CASE 11   + -
!
      COEFF=ZERO
      if(JA == JB) then
        if(JJA /= JJB) then
          CALL RECOP00(NS,JA,JA,KA,IAT)
          if(IAT == 0)return
        END if
        CALL RECOP1(NS,JA,KA,0,IAT,REC)
        if(IAT == 0)return
        CALL PERKO2(JA,JA,JA,JA,1)
        QM1=HALF
        QM2=-HALF
        CALL WJ1(IK1,BK1,ID1,BD1,KA,QM1,QM2,WJ)
        if(DABS(WJ) > EPS) then
           CALL RECOP1(NS,JA,KA,1,IAT,REC)
           COEFF=WJ*REC*DSQRT(DBLE(ID1(3)+1))/DSQRT(DBLE(2*KA+1))
           COEFF=-COEFF
        END if
      END if
      return
      end subroutine ONEPARTICLEJJ1

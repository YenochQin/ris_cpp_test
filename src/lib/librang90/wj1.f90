!*******************************************************************
!                                                                  *
      subroutine WJ1(IK,BK,ID,BD,K2,QM1,QM2,WJ)
!                                                                  *
!   ---------------  SECTION SQ    SUBPROGRAM 24  --------------   *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING MATRIX       *
!                                                                  *
!                      N       (k2)  N'     +-                     *
!     ELEMENT:       (j  QJ:: W   ::j  QJ)  -+                     *
!                                           ++                     *
!                                           -- S5(1.47),(1.48),    *
!                                                (1.49),(1.50).    *
!                                                                  *
!     subroutine CALLED: CLE0SM,C1E1SM,RWJJ                        *
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
      use CONS_C,          only: ZERO, TENTH, ONE, TWO, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mes_I
      use w1jjg_I
      use cle0sm_I
      use c1e1sm_I
      use rwjj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)               :: K2
      integer,      intent(in), dimension(7) :: IK, ID
      real(real64), intent(in)               :: QM1, QM2
      real(real64), intent(in), dimension(3) :: BK, BD
      real(real64), intent(out)              :: WJ
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: K1, IQ, IQM2
      real(real64) :: A, QQ, W, WK1
!-----------------------------------------------
      WJ=ZERO
      if(ID(3) == 9) then
        if(MAX0(IK(4),ID(4)) < 3) then
          if(IK(1) < 300) CALL MES(56)
          if(ID(1) < 300) CALL MES(56)
          IQM2=QM2+QM2+QM2*EPS
          if((ID(4)+IQM2) > 2) CALL MES(2)
          CALL W1JJG(K2,QM1,QM2,IK,BK,ID,BD,WJ)
          return
        else
          PRINT*, "ERROR in  WJ1"
          STOP
        endif
      elseif(ID(3) > 9) then
        IQM2=QM2+QM2+QM2*EPS
        if((ID(4)+IQM2) > 2) CALL MES(2)
        CALL W1JJG(K2,QM1,QM2,IK,BK,ID,BD,WJ)
        return
      endif
      QQ=QM1+QM2
      if(DABS(QQ) >= EPS) then
        if(((K2/2)*2) /= K2)return
        IQ=QQ+QQ*TENTH
        if((IK(4)-ID(4)-2*IQ) /= 0)return
        CALL C1E1SM(BD(1),BD(3),QQ,BK(1),BK(3),A)
        if(DABS(A) < EPS)return
        CALL RWJJ(IK(3),IK(1),ID(1),1,K2,W)
        WJ=A*W/DSQRT(TWO*BK(1)+ONE)
      else if(IK(4) /= ID(4))then
        return
      else if(K2 == 0) then
        if(ID(1) /= IK(1))return
        if(QM1 < EPS) then
          A=DBLE(ID(3)+1-ID(4))
        else
          A=-DBLE(ID(4))
        END if
        WJ=A*DSQRT(DBLE(IK(6)+1)/DBLE(IK(3)+1))
      else
        K1=1
        if(((K2/2)*2) /= K2)K1=0
        WK1=DBLE(K1)
        CALL CLE0SM(BD(1),BD(3),WK1,BK(1),BK(3),A)
        if(DABS(A) < EPS)return
        CALL RWJJ(IK(3),IK(1),ID(1),K1,K2,W)
        A=A*W
        WJ=A/DSQRT(TWO*TWO*BK(1)+TWO)
        if(QM1 >= EPS)return
        if(((K2/2)*2) /= K2)WJ=-WJ
      END if
      return
      end subroutine WJ1

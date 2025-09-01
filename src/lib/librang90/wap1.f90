!*******************************************************************
!                                                                  *
      subroutine WAP1(IK,BK,ID,BD,K1,BK2,QM1,QM2,QM3,WA)
!                                                                  *
!   ---------------  SECTION SQ    SUBPROGRAM 23  --------------   *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING MATRIX       *
!                                                                  *
!                    N       (k1)   (j) (k2)   N'    +-            *
!     ELEMENT:     (j QJ::[ W    * A   ]    ::j QJ)  -+            *
!                                                    ++            *
!                                                    -- B17 (2.3)  *
!                                                                  *
!     subroutine CALLED: C0T5S,ITJJ3,IXJTIK,IZAS1,RUMTJJ,SIXJ,     *
!                        RMEAJJ,WJ1                                *
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
      use CONS_C,          only: ZERO, TENTH, TWO, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mes_I
      use wap1jjg_I
      use izas1_I
      use itjj3_I
      use rumtjj_I
      use ixjtik_I
      use c0T5S_I
      use rmeajj_I
      use wj1_I
      use sixj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK, ID
      real(real64), intent(in)               :: BK2, QM1, QM2, QM3
      real(real64), intent(in), dimension(3) :: BK, BD
      real(real64), intent(out)              :: WA
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KK1, KK2, IE, IQ3, IQM, IT, ITP, ITG, IBTT
      integer,      dimension(7) :: IBT
      real(real64)               :: ENQP, D1, S, SI, W
      real(real64), dimension(3) :: BT
!-----------------------------------------------
      WA=ZERO
      if(ID(3) == 9) then
        if(MAX0(IK(4),ID(4)) < 3) then
          if(IK(1) < 300) CALL MES(55)
          if(ID(1) < 300) CALL MES(55)
          CALL WAP1JJG(K1,BK2,QM1,QM2,QM3,IK,BK,ID,BD,WA)
          return
        endif
      elseif(ID(3) > 9) then
        CALL WAP1JJG(K1,BK2,QM1,QM2,QM3,IK,BK,ID,BD,WA)
        return
      endif
      if(IZAS1(ID(7),BD(3),IK(7),BK(3)) == 0)return
      ENQP=ZERO
      KK1=K1*2
      IQ3=QM3*TWO
      KK2=BK2+BK2+TENTH*BK2
      if(ITJJ3(IK,ID,KK2,BK,BD,IBT,BT,ITP,ITG,IQ3) == 0)return
      IQM=TWO*DABS(BT(3))+TENTH
      DO IT=ITP,ITG
        CALL RUMTJJ(IT,IBT(3),IBT(7),IBTT,IBT(6))
        if(IQM > IBT(7)) CYCLE
        if(IXJTIK(KK1,IK(3),KK2,ID(6),IK(6),IBT(6)) == 0) CYCLE
        IBT(1)=IT
        BT(2)=DBLE(IBT(6))/TWO
        BT(1)=DBLE(IBT(7))/TWO
        CALL C0T5S(BD(1),BD(3),QM3,BT(1),BT(3),D1)
        if(DABS(D1) < EPS) CYCLE
        CALL RMEAJJ(ID(3),IBT(1),IBT(7),IBT(6),ID(1),ID(7),ID(6),S)
        if(DABS(S) < EPS) CYCLE
        D1=D1*S
        CALL WJ1(IK,BK,IBT,BT,K1,QM1,QM2,W)
        if(DABS(W) < EPS) CYCLE
        D1=D1*W
        CALL SIXJ(KK1,IK(3),KK2,ID(6),IK(6),IBT(6),0,SI)
        D1=D1*SI/DSQRT(DBLE(IBT(7)+1))
        ENQP=ENQP+D1
      END DO
      WA=ENQP*DSQRT(DBLE(KK2+1))
      IE=KK2+IK(6)+ID(6)+2
      if(((IE/4)*4) /= IE)WA=-WA
      return
      end subroutine WAP1

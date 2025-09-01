!*******************************************************************
!                                                                  *
      subroutine AWP1(IK,BK,ID,BD,K1,BK2,QM1,QM2,QM3,AW)
!                                                                  *
!   ---------------  SECTION SQ    SUBPROGRAM 01  --------------   *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING MATRIX       *
!                                                                  *
!                    N      (j)  (k1) (k2)   N'     +-             *
!     ELEMENT:     (j QJ ::[A  * W    ]   ::j  QJ)  -+             *
!                                                   ++             *
!                                                   --  B17 (2.3)  *
!                                                                  *
!     subroutine CALLED: C0T5S,ITJJ2,IXJTIK,IZAS1,RUMTJJ,SIXJ,     *
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
      use CONS_C,          only: ZERO, TWO, TENTH, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use itjj2_I
      use ixjtik_I
      use izas1_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK, ID
      real(kind=real64), intent(in)               :: BK2, QM1, QM2, QM3
      real(kind=real64), intent(in), dimension(3) :: BK, BD
      real(kind=real64), intent(out)              :: AW
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KK1, KK2, IE, IQ, IQ2, IQ3, IQM, IT, ITP, ITG, IBTT
      integer,      dimension(7) :: IBT
      real(kind=real64)               :: ENQP, D1, S, SI, W
      real(kind=real64), dimension(3) :: BT
!-----------------------------------------------
      AW=ZERO
      if(ID(3) == 9) then
        if(MAX0(IK(4),ID(4)) < 3) then
          if(IK(1) < 300) CALL MES(54)
          if(ID(1) < 300) CALL MES(54)
          CALL AWP1JJG(K1,BK2,QM1,QM2,QM3,IK,BK,ID,BD,AW)
          return
        else
          PRINT*, "ERROR in AWP1"
          STOP
        endif
      elseif(ID(3) > 9) then
        CALL AWP1JJG(K1,BK2,QM1,QM2,QM3,IK,BK,ID,BD,AW)
        return
      endif
      if(IZAS1(ID(7),BD(3),IK(7),BK(3)) == 0)return
      ENQP=ZERO
      IQ2=QM2*TWO+QM2*TENTH
      IQ3=QM3*TWO+QM3*TENTH
      IQ=IQ2+IQ3
      KK1=K1*2
      KK2=BK2+BK2+TENTH*BK2
      if(ITJJ2(IK,ID,KK2,BK,BD,IBT,BT,ITP,ITG,IQ) == 0)return
      IQM=TWO*DABS(BT(3))+TENTH
      DO IT=ITP,ITG
        CALL RUMTJJ(IT,IBT(3),IBT(7),IBTT,IBT(6))
        if(IQM > IBT(7)) CYCLE
        if(IXJTIK(IK(3),KK1,KK2,ID(6),IK(6),IBT(6)) == 0) CYCLE
        IBT(1)=IT
        BT(2)=DBLE(IBT(6))/TWO
        BT(1)=DBLE(IBT(7))/TWO
        CALL C0T5S(BT(1),BT(3),QM1,BK(1),BK(3),D1)
        if(DABS(D1) < EPS) CYCLE
        CALL RMEAJJ(IK(3),IK(1),IK(7),IK(6),IBT(1),IBT(7),IBT(6),S)
        if(DABS(S) < EPS) CYCLE
        CALL WJ1(IBT,BT,ID,BD,K1,QM2,QM3,W)
        D1=D1*W*S
        if(DABS(D1) < EPS) CYCLE
        CALL SIXJ(IK(3),KK1,KK2,ID(6),IK(6),IBT(6),0,SI)
        D1=D1*SI/DSQRT(DBLE(IK(7)+1))
        ENQP=ENQP+D1
      END DO
      AW=ENQP*DSQRT(DBLE(KK2+1))
      IE=KK2+IK(6)+ID(6)+2
      if(((IE/4)*4) /= IE)AW=-AW
      return
      end subroutine AWP1

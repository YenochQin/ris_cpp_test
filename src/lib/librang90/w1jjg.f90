!*******************************************************************
!                                                                  *
      subroutine W1JJG(K1,QM1,QM2,IK,BK,ID,BD,WW)
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
      use CONS_C,          only: ZERO, TENTH, HALF, TWO, EPS
      use ribojj_C
      use ribojj9_C
      use ribojj11_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mes_I
      use rumtjj_I
      use ixjtik_I
      use ittk_I
      use a1jj_I
      use sixj_I
      use izas1_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)               :: K1
      integer,      intent(in), dimension(7) :: IK, ID
      real(real64), intent(in)               :: QM1, QM2
      real(real64), intent(in), dimension(3) :: BK, BD
      real(real64), intent(out)              :: WW
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KK1,IQ,IQM,IE,IT,ITK,ITD,ITP,ITP1,ITG,ITG1,IBTT
      integer,      dimension(7) :: IBT
      real(real64)               :: ENQP, D1, SI1, W
      real(real64), dimension(3) :: BT
!-----------------------------------------------
      WW=ZERO
      if(IZAS1(ID(7),BD(3),IK(7),BK(3)) == 0)return
      ENQP=ZERO
      KK1=K1*2
      IQ=QM2*TWO+QM2*TENTH
      if(ID(3) > 37) return
      if(ITTK(ID(6),IK(6),KK1) == 0)return
      ITK=IK(1)
      ITD=ID(1)
      if(ID(3) == 9) then
        if(ID(4) > 2) CALL MES(1)
        if(IK(4) > 2) CALL MES(1)
        ITK=ITK-300
        ITD=ITD-300
        ITP1=IMPNJJ9(ITK)
        ITP=IMPNJJ9(ITD)
        if(ITP1 /= ITP)return
        ITG1=IMGNJJ9(ITK)
        ITG=IMGNJJ9(ITD)
       else
        if(ID(4) > 2) CALL MES(1)
        if(IK(4) > 2) CALL MES(1)
        ITP1=IMPNJJ11(ITK)
        ITP=IMPNJJ11(ITD)
        if(ITP1 /= ITP)return
        ITG1=IMGNJJ11(ITK)
        ITG=IMGNJJ11(ITD)
      endif
      if(ITG1 /= ITG)return
      IBT(2)=ID(2)
      IBT(3)=ID(3)
      IBT(4)=ID(4)+IQ
      BT(3)=BD(3)+HALF*DBLE(IQ)
      IQM=TWO*DABS(BT(3))+TENTH
      DO IT=ITP,ITG
        CALL RUMTJJ(IT,IBT(3),IBT(7),IBTT,IBT(6))
        if(IQM <= IBT(7)) then
          if(IXJTIK(IK(3),IK(3),KK1,ID(6),IK(6),IBT(6)) /= 0) then
            IBT(1)=IT
            BT(2)=DBLE(IBT(6))/TWO
            BT(1)=DBLE(IBT(7))/TWO
            CALL A1JJ(IK,BK,IBT,BT,QM1,D1)
            if(DABS(D1) > EPS) then
              CALL A1JJ(IBT,BT,ID,BD,QM2,W)
              if(DABS(W) > EPS) then
                D1=D1*W
                CALL SIXJ(IK(3),IK(3),KK1,ID(6),IK(6),IBT(6),0,SI1)
                ENQP=ENQP+D1*SI1
              endif
            endif
          endif
        endif
      END DO
      WW=ENQP*DSQRT(DBLE((KK1+1)))
      IE=KK1+IK(6)+ID(6)
      if(((IE/4)*4) /= IE)WW=-WW
      return
      end subroutine W1JJG

!*******************************************************************
!                                                                  *
      subroutine EL2(JJA,JJB,JA,JB,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 04  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF TWO PARTICLE OPERATOR IN CASE :       N'1 = N1 - 2        *
!                                              N'2 = N2 + 2        *
!                                                                  *
!     subroutine CALLED: COULOM,GG1122,ITREXG,IXJTIK,PERKO2,       *
!                        RECO,RECO2,SIXJ,SPEAK                     *
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
      use m_C,             only: JLIST, NPEEL
      use orb_C,           only: NAK
      use trk_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reco_I
      use reco2_I
      use itrexg_I
      use itrig_I
      use ixjtik_I
      use snrc_I
      use coulom_I
      use gg1122_I
      use sixj_I
      use cxk_I
      use talk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJA,JJB,JA,JB,ICOLBREI
!      dimension J(2)
!      dimension COND(12,20),S(12),IS(4),KAPS(4),KS(4)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IAT,IA,IB,IBRD,IBRE,IP1,IP2,IG1,IG2,IKK,II,I2,I3, &
                 ifAZ,ifAZ1,ifAZFRCS,J12,JAA,JBB, &
                 KRA,L1,L2,N,ND1,ND2,NE1,NE2,NUP1,NU,MU
      integer, dimension(2) :: J
      integer, dimension(4) :: IS,KAPS,KS
      real(kind=real64)          :: AA,A1,AB,BB,QM1,QM2,QM3,QM4,RECC,SI
      real(kind=real64), dimension(12)    :: S
      real(kind=real64), dimension(12,20) :: COND
!-----------------------------------------------
      if(NPEEL <= 1)return
      if(JA > JB) then
        JAA=JB
        JBB=JA
      else
        JAA=JA
        JBB=JB
      END if
      CALL RECO(JAA,JBB,JBB,JBB,1,IAT)
      if(IAT == 0)return
      IA=JLIST(JA)
      IB=JLIST(JB)
      QM1=HALF
      QM2=HALF
      QM3=-HALF
      QM4=-HALF
      CALL PERKO2(JA,JB,JA,JA,2)
      J(1)=ID1(3)
      J(2)=ID2(3)
      L1=(J(1)+1)/2
      L2=(J(2)+1)/2
      IP1=ITREXG(J(1),J(1),J(2),J(2),IKK)+1
      if(IKK <= 0)return
      IG1=IP1+IKK-1
! * * *                      * * *                      * * *
!     THE CASE 1122   + + - -
!
      IP2=ITREXG(J(1),J(2),J(1),J(2),IKK)+1
      if(IKK <= 0) return
      IG2=IP2+IKK-1
      if (ICOLBREI == 2) then
        IS(1)=IA
        IS(2)=IA
        IS(3)=IB
        IS(4)=IB
        KAPS(1)=2*NAK(IS(1))
        KAPS(2)=2*NAK(IS(2))
        KAPS(3)=2*NAK(IS(3))
        KAPS(4)=2*NAK(IS(4))
        KS(1)=IABS(KAPS(1))
        KS(2)=IABS(KAPS(2))
        KS(3)=IABS(KAPS(3))
        KS(4)=IABS(KAPS(4))
        CALL SNRC(IS,KAPS,KS,ND1,ND2,NE1,NE2,IBRD,IBRE)
        if(IBRD <= 0)return
        DO II=1,20
          COND(1,II) =ZERO
          COND(2,II) =ZERO
          COND(3,II) =ZERO
          COND(4,II) =ZERO
          COND(5,II) =ZERO
          COND(6,II) =ZERO
          COND(7,II) =ZERO
          COND(8,II) =ZERO
          COND(9,II) =ZERO
          COND(10,II)=ZERO
          COND(11,II)=ZERO
          COND(12,II)=ZERO
        END DO
      END if
      DO I2=IP2,IG2,2
        KRA=(I2-1)/2
        if (ICOLBREI == 1) then
          CALL COULOM(L1,L1,L2,L2,ID1(5),ID1(5),ID2(5),ID2(5),KRA,A1)
          if(DABS(A1) < EPS) CYCLE
          A1=-HALF*A1
        END if
        AB=ZERO
          DO I3=IP1,IG1,2
            J12=(I3-1)/2
            CALL RECO2(JAA,JBB,J12*2,0,IAT,RECC)
            if(IAT /= 0) then
              if(IXJTIK(J(1),J(2),KRA*2,J(2),J(1),J12*2) /= 0) then
                CALL GG1122(J12,J12,QM1,QM2,QM3,QM4,AA)
                if(DABS(AA) > EPS) then
                  CALL RECO2(JAA,JBB,J12*2,1,IAT,RECC)
                  AA=AA*RECC
                  CALL SIXJ(J(1),J(2),KRA*2,J(2),J(1),J12*2,0,SI)
                  AA=AA*SI*DSQRT(DBLE(I3))
                  ifAZ=IK1(3)+IK2(3)+KRA*2+J12*2
                  if((ifAZ/4)*4 /= ifAZ)AA=-AA
                  AB=AB+AA
                END if
              END if
            END if
          END DO
!
!       TRANSFORM FANO & RACAH PHASE CONVENTION
!       TO CONDON & SHORTLEY PHASE CONVENTION
!
        ifAZFRCS=1
        ifAZ1=IK1(5)*IK1(4)+IK2(5)*IK2(4)-ID1(5)*ID1(4)-ID2(5)*ID2(4)
        if((ifAZ1/4)*4 /= ifAZ1)ifAZFRCS=-ifAZFRCS
!
        if (ICOLBREI == 1) then
          BB=A1*AB*DBLE(ifAZFRCS)
          if(DABS(BB) > EPS)CALL SPEAK(JJA,JJB,IA,IA,IB,IB,KRA,BB)
        else if (ICOLBREI == 2) then
          NU=KRA
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU+1) /= 0) .AND.   &
               (ITRIG(KS(2),KS(4),NU+NU+1) /= 0)) then
              N=(NU-ND1)/2+1
              if(NU > 0) then
                CALL CXK(S,IS,KAPS,NU,KRA,1,1)
                DO MU = 1,4
                  COND(MU,N)=COND(MU,N)-HALF*AB*S(MU)
                END DO
              END if
            END if
          END if
          NU=KRA+1
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU-1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(4),NU+NU-1) /= 0)) then
              N=(NU-ND1)/2+1
              if(N <= ND2) then
                CALL CXK(S,IS,KAPS,NU,KRA,1,1)
                DO MU = 1,4
                  COND(MU,N)=COND(MU,N)-HALF*AB*S(MU)
                END DO
              END if
            END if
          END if
          NU=KRA-1
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU+3) /= 0) .AND.   &
               (ITRIG(KS(2),KS(4),NU+NU+3) /= 0)) then
              if(NU >= 0) then
                N=(NU-ND1)/2+1
                if(N < ND2) then
                  CALL CXK(S,IS,KAPS,NU,KRA,1,1)
                  DO MU = 1,12
                    COND(MU,N)=COND(MU,N)-HALF*AB*S(MU)
                  END DO
                END if
              END if
            END if
          END if
        END if
      END DO
      if (ICOLBREI .EQ. 2) then
        DO N = 1,ND2
          NU=ND1+2*(N-1)
          CALL TALK(JJA,JJB,NU,IS(1),IS(3),IS(2),IS(4),1,COND(1,N))
          CALL TALK(JJA,JJB,NU,IS(3),IS(1),IS(4),IS(2),1,COND(2,N))
          CALL TALK(JJA,JJB,NU,IS(1),IS(3),IS(4),IS(2),1,COND(3,N))
          CALL TALK(JJA,JJB,NU,IS(3),IS(1),IS(2),IS(4),1,COND(4,N))
          if(N == ND2) CYCLE
          NUP1=NU+1
          CALL TALK(JJA,JJB,NUP1,IS(1),IS(3),IS(2),IS(4),2,COND(5,N))
          CALL TALK(JJA,JJB,NUP1,IS(2),IS(4),IS(1),IS(3),2,COND(6,N))
          CALL TALK(JJA,JJB,NUP1,IS(3),IS(1),IS(4),IS(2),2,COND(7,N))
          CALL TALK(JJA,JJB,NUP1,IS(4),IS(2),IS(3),IS(1),2,COND(8,N))
          CALL TALK(JJA,JJB,NUP1,IS(1),IS(3),IS(4),IS(2),2,COND(9,N))
          CALL TALK(JJA,JJB,NUP1,IS(4),IS(2),IS(1),IS(3),2,COND(10,N))
          CALL TALK(JJA,JJB,NUP1,IS(3),IS(1),IS(2),IS(4),2,COND(11,N))
          CALL TALK(JJA,JJB,NUP1,IS(2),IS(4),IS(3),IS(1),2,COND(12,N))
        END DO
      END if
      return
      end subroutine EL2

!*******************************************************************
!                                                                  *
      subroutine EL41(JJJA,JJJB,JA,JB,JC,IREZ,JJA,JJB,JJC,JJD,    &
                                                          ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 10  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF TWO PARTICLE OPERATOR IN CASE :       N'1 = N1 + 1        *
!                                              N'2 = N2 + 1        *
!                                              N'3 = N3 - 2,       *
!     WHEN IREZ = 1   . . . . . . . . . . . . . . . . . . .        *
!                                              N'1 = N1 - 1        *
!                                              N'2 = N2 - 1        *
!                                              N'3 = N3 + 2,       *
!     WHEN IREZ = 2   . . . . . . . . . . . . . . . . . . .        *
!                                                                  *
!     subroutine CALLED: COULOM,EILE,GG1233,ITREXG,IXJTIK,         *
!                        JFAZE,PERKO2,RECO,REC3,SIXJ,SPEAK         *
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
      use m_C,             only: NQ1, JLIST, NPEEL
      use orb_C,           only: NAK
      use trk_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use eile_I
      use reco_I
      use rec3_I
      use perko2_I
      use snrc_I
      use jfaze_I
      use ixjtik_I
      use itrexg_I
      use itrig_I
      use coulom_I
      use gg1233_I
      use sixj_I
      use speak_I
      use cxk_I
      use talk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJJA,JJJB,JA,JB,JC,IREZ,JJA,JJB, &
                             JJC,JJD,ICOLBREI
!      dimension J(3)
!      dimension COND(12,20),S(12),IS(4),KAPS(4),KS(4)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  IA,IB,IC,II,IIA,IIB,IIC,IBRD,IBRE,IP1,IG1,IP2,IG2, &
                  IAT,IID,IKK,ifAZ,ifAZP,ifAZFRCS,INN,I2,I3,JB1,JAA, &
                  JBB,JCC,J12,KRA,L1,L2,L3,ND1,ND2,NE1,NE2,N,NN,NU,  &
                  NUP1,MU
      integer, dimension(3) :: J
      integer, dimension(4) :: IS,KAPS,KS
      real(real64)          :: AA,AB,A1,BB,QM1,QM2,QM3,QM4,SI,RECC
      real(real64), dimension(12) :: S
      real(real64), dimension(12,20) :: COND
!-----------------------------------------------
      CALL EILE(JA,JB,JC,JAA,JBB,JCC)
      if(NPEEL <= 1)return
      CALL RECO(JAA,JCC,JBB,JBB,2,IAT)
      if(IAT == 0)return
      IA=JLIST(JA)
      IB=JLIST(JB)
      IC=JLIST(JC)
      IIA=JLIST(JJA)
      IIB=JLIST(JJB)
      IIC=JLIST(JJC)
      IID=JLIST(JJD)
      if(IREZ == 1) then
        QM1=-HALF
        QM2=-HALF
        QM3=HALF
        QM4=HALF
      else
        QM1=HALF
        QM2=HALF
        QM3=-HALF
        QM4=-HALF
      END if
      CALL PERKO2(JA,JB,JC,JA,3)
      J(1)=ID1(3)
      J(2)=ID2(3)
      J(3)=ID3(3)
      L1=(J(1)+1)/2
      L2=(J(2)+1)/2
      L3=(J(3)+1)/2
      if (ICOLBREI == 2) then
        IS(1)=IIA
        IS(2)=IIB
        IS(3)=IIC
        IS(4)=IID
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
      ifAZP=JFAZE(JC,JA,JB,JC)
      ifAZFRCS = 1
!
!     TRANSFORM FANO & RACAH PHASE CONVENTION
!     TO CONDON & SHORTLEY PHASE CONVENTION
!
      ifAZ=IK1(5)*IK1(4)+IK2(5)*IK2(4)-ID1(5)*ID1(4)-ID2(5)*ID2(4)+ &
      IK3(5)*IK3(4)-ID3(5)*ID3(4)
      if((ifAZ/4)*4 /= ifAZ)ifAZFRCS=-ifAZFRCS
!
      if(JA > JB) then
        JAA=JB
        JBB=JA
      else
        JAA=JA
        JBB=JB
      END if
      NN=0
      JB1=JBB-1
      DO II=JAA,JB1
        INN=JLIST(II)
        NN=NQ1(INN)+NN
      END DO
      if((NN/2)*2 == NN)ifAZP=-ifAZP
! * * *                      * * *                      * * *
!     CASES 3312   + + - -        TRANSFORM TO  1233   - - + +
!           3321                                1233
!                                                    (IREZ = 1)
!     OR
!     CASES 1233   + + - -        TRANSFORM TO  1233   + + - -
!           2133                                1233
!                                                    (IREZ = 2)
      IP1=ITREXG(J(2),J(1),J(3),J(3),IKK)+1
      if(IKK <= 0)return
      IG1=IP1+IKK-1
      IP2=ITREXG(J(3),J(1),J(2),J(3),IKK)+1
      if(IKK <= 0) return
      IG2=IP2+IKK-1
      DO I2=IP2,IG2,2
        KRA=(I2-1)/2
!
        if (ICOLBREI == 1) then
          if(IREZ == 2) then
            CALL COULOM(L1,L2,L3,L3,ID1(5),ID2(5),ID3(5),ID3(5),KRA,A1)
          else
            CALL COULOM(L3,L3,L1,L2,ID3(5),ID3(5),ID1(5),ID2(5),KRA,A1)
          END if
          if(DABS(A1) < EPS) CYCLE
        END if
!
        AB=ZERO
        DO I3=IP1,IG1,2
          J12=(I3-1)/2
          ifAZ=J(2)-J12+1
          if(IREZ == 2)ifAZ=J(1)-J12+1
          if((ifAZ/2)*2 /= ifAZ) CYCLE
          CALL REC3(JA,JB,JC,J(1),J(2),J12*2,0,IAT,AA)
          if(IAT == 0) CYCLE
          if(IXJTIK(J(3),J(1),KRA*2,J(2),J(3),J12*2) == 0) CYCLE
          CALL GG1233(IK1,IK2,IK3,BK1,BK2,BK3,ID1,ID2,ID3,BD1,    &
                    BD2,BD3,J12,QM1,QM2,QM3,QM4,AA)
          if(DABS(AA) < EPS) CYCLE
          CALL REC3(JA,JB,JC,J(1),J(2),J12*2,1,IAT,RECC)
          AA=AA*RECC
          CALL SIXJ(J(3),J(1),KRA*2,J(2),J(3),J12*2,0,SI)
          AA=AA*SI*DSQRT(DBLE(I3))
          ifAZ=J(3)+J(1)+2*J12+2*KRA
          if(IREZ == 2)ifAZ=J(2)+J(3)+2*J12+2*KRA
          if((ifAZ/4)*4 /= ifAZ)AA=-AA
          AB=AB+AA
        END DO
        if(DABS(AB) < EPS) CYCLE
        AB=-AB*DBLE(ifAZP)
        if (ICOLBREI == 1) then
          BB=A1*AB*DBLE(ifAZFRCS)
          CALL SPEAK(JJJA,JJJB,IIA,IIB,IIC,IID,KRA,BB)
        else if (ICOLBREI == 2) then
          NU=KRA
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU+1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(4),NU+NU+1) /= 0)) then
              N=(NU-ND1)/2+1
              if(NU > 0) then
                CALL CXK(S,IS,KAPS,NU,KRA,1,1)
                DO MU = 1,4
                  COND(MU,N)=COND(MU,N)+AB*S(MU)
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
                  COND(MU,N)=COND(MU,N)+AB*S(MU)
                END DO
              END if
            END if
          END if
          NU=KRA-1
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU+3) /= 0) .AND.  &
               (ITRIG(KS(2),KS(4),NU+NU+3) /= 0)) then
              if(NU >= 0) then
                N=(NU-ND1)/2+1
                if(N < ND2) then
                  CALL CXK(S,IS,KAPS,NU,KRA,1,1)
                  DO MU = 1,12
                    COND(MU,N)=COND(MU,N)+AB*S(MU)
                  END DO
                END if
              END if
            END if
          END if
        END if
      END DO
      if (ICOLBREI == 2) then
        DO N = 1,ND2
          NU=ND1+2*(N-1)
          CALL TALK(JJJA,JJJB,NU,IS(1),IS(3),IS(2),IS(4),1,COND(1,N))
          CALL TALK(JJJA,JJJB,NU,IS(3),IS(1),IS(4),IS(2),1,COND(2,N))
          CALL TALK(JJJA,JJJB,NU,IS(1),IS(3),IS(4),IS(2),1,COND(3,N))
          CALL TALK(JJJA,JJJB,NU,IS(3),IS(1),IS(2),IS(4),1,COND(4,N))
          if(N == ND2) CYCLE
          NUP1=NU+1
          CALL TALK(JJJA,JJJB,NUP1,IS(1),IS(3),IS(2),IS(4),2,COND(5,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(2),IS(4),IS(1),IS(3),2,COND(6,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(3),IS(1),IS(4),IS(2),2,COND(7,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(4),IS(2),IS(3),IS(1),2,COND(8,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(1),IS(3),IS(4),IS(2),2,COND(9,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(4),IS(2),IS(1),IS(3),2,COND(10,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(3),IS(1),IS(2),IS(4),2,COND(11,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(2),IS(4),IS(3),IS(1),2,COND(12,N))
        END DO
      END if
      return
   10 WRITE(99,100)
  100 FORMAT(5X,'ERRO IN EL41  PMGG RAGG')
      STOP
      end subroutine EL41

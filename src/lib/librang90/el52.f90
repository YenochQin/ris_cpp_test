!*******************************************************************
!                                                                  *
      subroutine EL52(JJA,JJB,JA,JB,JC,JD,IREZ,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 13  -------------  *
!                                                                  *
!     THIS PACKAGE EVALUATED THE CASES - 1324, 3142, 1342, 3124    *
!                                                   ( IREZ = 1),   *
!                                                   ( + + - - ),   *
!     WHICH APPEARS IN CALCULATION MATRIX ELEMENTS BETWEEN         *
!     CONFIGURATIONS:                               N'1 = N1 - 1   *
!                                                   N'2 = N2 + 1   *
!                                                   N'3 = N3 - 1   *
!                                                   N'4 = N4 + 1   *
!     AND    2413, 4231, 2431, 4213                 ( IREZ = 2),   *
!                                                   ( + + - - ),   *
!     WHICH APPEARS IN CALCULATION MATRIX ELEMENTS BETWEEN         *
!     CONFIGURATIONS:                               N'1 = N1 + 1   *
!                                                   N'2 = N2 - 1   *
!                                                   N'3 = N3 + 1   *
!                                                   N'4 = N4 - 1   *
!                                                                  *
!     subroutine CALLED: COULOM,GG1234,ITREXG,IXJTIK,PERKO2,       *
!                        RECO,REC4,SIXJ,SPEAK                      *
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
      use reco_I
      use reco4_I
      use perko2_I
      use snrc_I
      use gg1234_I
      use itrexg_I
      use ixjtik_I
      use coulom_I
      use sixj_I
      use speak_I
      use itrig_I
      use cxk_I
      use talk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJA,JJB,JA,JB,JC,JD,IREZ,ICOLBREI
!      dimension PMGG(30),J(4)
!     dimension COND(12,20),CONE(12,20),S(12),IS(4),KAPS(4),KS(4)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IA,IB,IC,ID,IBRD,IBRE,II,IP1,IP2,IG1,IG2,IKK,I2,I3,  &
                 I4,ifAZ,ifAZP,ifAZFRCS,INN,IAT,KRA,KRA1,L1,L2,L3,L4, &
                 J12,JB1,JD1,ND1,ND2,NE1,NE2,N,NN,NU,NUP1,MU
      integer, dimension(4) :: J
      integer, dimension(4) :: IS,KAPS,KS
      real(kind=real64)          :: AA,AB,A1,BB,QM1,QM2,QM3,QM4,RAG,RECC,SI
      real(kind=real64), dimension(12) :: S
      real(kind=real64), dimension(30) :: PMGG
      real(kind=real64), dimension(12,20) :: COND,CONE
!-----------------------------------------------
      if(NPEEL <= 3)return
      CALL RECO(JA,JD,JC,JB,3,IAT)
      if(IAT == 0)return
      IA=JLIST(JA)
      IB=JLIST(JB)
      IC=JLIST(JC)
      ID=JLIST(JD)
      if(IREZ == 2) then
        QM1=-HALF
        QM2=HALF
        QM3=-HALF
        QM4=HALF
      else
        QM1=HALF
        QM2=-HALF
        QM3=HALF
        QM4=-HALF
      END if
      CALL PERKO2(JA,JB,JC,JD,4)
      J(1)=ID1(3)
      J(2)=ID2(3)
      J(3)=ID3(3)
      J(4)=ID4(3)
      L1=(J(1)+1)/2
      L2=(J(2)+1)/2
      L3=(J(3)+1)/2
      L4=(J(4)+1)/2
      if (ICOLBREI == 2) then
        if(IREZ == 1) then
          IS(1)=IA
          IS(2)=IC
          IS(3)=IB
          IS(4)=ID
        else if(IREZ == 2) then
          IS(1)=IB
          IS(2)=ID
          IS(3)=IA
          IS(4)=IC
        END if
        KAPS(1)=2*NAK(IS(1))
        KAPS(2)=2*NAK(IS(2))
        KAPS(3)=2*NAK(IS(3))
        KAPS(4)=2*NAK(IS(4))
        KS(1)=IABS(KAPS(1))
        KS(2)=IABS(KAPS(2))
        KS(3)=IABS(KAPS(3))
        KS(4)=IABS(KAPS(4))
        CALL SNRC(IS,KAPS,KS,ND1,ND2,NE1,NE2,IBRD,IBRE)
        if(IBRD <= 0 .AND. IBRE <= 0)return
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
          CONE(1,II) =ZERO
          CONE(2,II) =ZERO
          CONE(3,II) =ZERO
          CONE(4,II) =ZERO
          CONE(5,II) =ZERO
          CONE(6,II) =ZERO
          CONE(7,II) =ZERO
          CONE(8,II) =ZERO
          CONE(9,II) =ZERO
          CONE(10,II)=ZERO
          CONE(11,II)=ZERO
          CONE(12,II)=ZERO
        END DO
      END if
      CALL GG1234(IK1,IK2,IK3,IK4,BK1,BK2,BK3,BK4,ID1,ID2,  &
      ID3,ID4,BD1,BD2,BD3,BD4,QM1,QM2,QM3,QM4,RAG)
      if(DABS(RAG) < EPS) return
      IP1=ITREXG(J(1),J(2),J(3),J(4),IKK)+1
      if(IKK <= 0)return
      IG1=IP1+IKK-1
      DO I4=IP1,IG1,2
        KRA=(I4-1)/2
        KRA1=KRA+1
        if(KRA1 > 30)GO TO 10
        PMGG(KRA1)=ZERO
        CALL RECO4(JA,JB,JC,JD,J(1),J(2),J(3),J(4),KRA*2,0,IAT,RECC)
        if(IAT == 0) CYCLE
        CALL RECO4(JA,JB,JC,JD,J(1),J(2),J(3),J(4),KRA*2,1,IAT,RECC)
        PMGG(KRA1)=RECC
      END DO
!
!     TRANSFORM FANO & RACAH PHASE CONVENTION
!     TO CONDON & SHORTLEY PHASE CONVENTION
!
      ifAZFRCS=1
      ifAZ=IK1(5)*IK1(4)+IK2(5)*IK2(4)-ID1(5)*ID1(4)-ID2(5)*ID2(4)  &
      +IK3(5)*IK3(4)-ID3(5)*ID3(4)+IK4(5)*IK4(4)-ID4(5)*ID4(4)
      if((ifAZ/4)*4 /= ifAZ)ifAZFRCS=-ifAZFRCS
!
      NN=0
      JB1=JB-1
      ifAZP=1
      DO II=JA,JB1
        INN=JLIST(II)
        NN=NQ1(INN)+NN
      END DO
      if((NN/2)*2 == NN)ifAZP=-ifAZP
      NN=0
      JD1=JD-1
      DO II=JC,JD1
        INN=JLIST(II)
        NN=NQ1(INN)+NN
      END DO
      if((NN/2)*2 == NN)ifAZP=-ifAZP
! * * *                      * * *                      * * *
!     CASES 1324   + + - -        TRANSFORM TO  1234   + - + -
!           1342                                1234
!                                                    (IREZ = 1)
!     OR
!     CASES 2413   + + - -        TRANSFORM TO  1234   - + - +
!           4231                                1234
!                                                    (IREZ = 2)
      DO I3=IP1,IG1,2
        KRA=(I3-1)/2
        if (ICOLBREI == 1) then
          if(IREZ == 2) then
            CALL COULOM(L2,L4,L1,L3,ID2(5),ID4(5),ID1(5),ID3(5),KRA,A1)
          else
            CALL COULOM(L1,L3,L2,L4,ID1(5),ID3(5),ID2(5),ID4(5),KRA,A1)
          END if
          if(DABS(A1) < EPS) CYCLE
        END if
        KRA1=KRA+1
        if(KRA1 > 30)GO TO 10
        AA=PMGG(KRA1)
        if(DABS(AA) < EPS) CYCLE
        AA=AA*RAG
        if(DABS(AA) < EPS) CYCLE
        AA=AA/DSQRT(DBLE(I3))
        AB=AA*DBLE(ifAZP)
        if(IREZ == 2) then
          ifAZ=J(1)+J(2)+J(4)+J(3)+4*KRA
          if((ifAZ/4)*4 /= ifAZ)AB=-AB
        END if
        if (ICOLBREI == 1) then
          BB=A1*AB*DBLE(ifAZFRCS)
          if(IREZ == 1)CALL SPEAK(JJA,JJB,IA,IC,IB,ID,KRA,BB)
          if(IREZ == 2)CALL SPEAK(JJA,JJB,IB,ID,IA,IC,KRA,BB)
        else if (ICOLBREI == 2) then
          NU=KRA
          if(((NU-ND1)/2)*2 == (NU-ND1)) then
            if((ITRIG(KS(1),KS(3),NU+NU+1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(4),NU+NU+1) /= 0)) then
              N=(NU-ND1)/2+1
              if(NU >  0) then
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
            if((ITRIG(KS(1),KS(3),NU+NU+3) /= 0) .AND.   &
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
! * * *                      * * *                      * * *
!     CASES 1342   + + - -        TRANSFORM TO  1234   + - + -
!           3124                                1234
!                                                    (IREZ = 1)
!     OR
!     CASES 2431   + + - -        TRANSFORM TO  1234   - + - +
!           4213                                1234
!                                                    (IREZ = 2)
      IP2=ITREXG(J(1),J(4),J(2),J(3),IKK)+1
      if(IKK <= 0) return
      IG2=IP2+IKK-1
      DO I2=IP2,IG2,2
        KRA=(I2-1)/2
!
        if (ICOLBREI == 1) then
          if(IREZ == 2) then
            CALL COULOM(L2,L4,L3,L1,ID2(5),ID4(5),ID3(5),ID1(5),KRA,A1)
          else
            CALL COULOM(L1,L3,L4,L2,ID1(5),ID3(5),ID4(5),ID2(5),KRA,A1)
          END if
          if(DABS(A1) < EPS) CYCLE
        END if
!
        AB=ZERO
        DO I3=IP1,IG1,2
          J12=(I3-1)/2
          ifAZ=J(1)-J(3)+2*J12
          if(IREZ == 2)ifAZ=J(4)-J(2)-2*J12
          if((ifAZ/2)*2 /= ifAZ) CYCLE
          KRA1=J12+1
          if(KRA1 > 30)GO TO 10
          AA=PMGG(KRA1)
          if(DABS(AA) < EPS) CYCLE
          AA=AA*RAG
          if(DABS(AA) < EPS) CYCLE
          if(IXJTIK(J(1),J(4),KRA*2,J(3),J(2),J12*2) == 0) CYCLE
          CALL SIXJ(J(1),J(4),KRA*2,J(3),J(2),J12*2,0,SI)
          AA=AA*SI*DSQRT(DBLE(I3))
          ifAZ=2*J(3)-4*KRA-4*J12
          if(IREZ == 2)ifAZ=J(1)+J(2)+J(3)-J(4)-4*KRA
          if((ifAZ/4)*4 /= ifAZ)AA=-AA
          AB=AB+AA
        END DO
        if(DABS(AB) < EPS) CYCLE
        AB=-AB*DBLE(ifAZP)
        if (ICOLBREI == 1) then
          BB=A1*AB*DBLE(ifAZFRCS)
          if(IREZ == 1)CALL SPEAK(JJA,JJB,IA,IC,ID,IB,KRA,BB)
          if(IREZ == 2)CALL SPEAK(JJA,JJB,IB,ID,IC,IA,KRA,BB)
        else if (ICOLBREI == 2) then
          NU=KRA
          if(((NU-NE1)/2)*2 == (NU-NE1)) then
            if((ITRIG(KS(1),KS(4),NU+NU+1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(3),NU+NU+1) /= 0)) then
              N=(NU-NE1)/2+1
              if(NU > 0) then
                CALL CXK(S,IS,KAPS,NU,KRA,1,2)
                DO MU = 1,4
                  CONE(MU,N)=CONE(MU,N)+AB*S(MU)
                END DO
              END if
            END if
          END if
          NU=KRA+1
          if(((NU-NE1)/2)*2 == (NU-NE1)) then
            if((ITRIG(KS(1),KS(4),NU+NU-1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(3),NU+NU-1) /= 0)) then
              N=(NU-NE1)/2+1
              if(N <= NE2) then
                CALL CXK(S,IS,KAPS,NU,KRA,1,2)
                DO MU = 1,4
                  CONE(MU,N)=CONE(MU,N)+AB*S(MU)
                END DO
              END if
            END if
          END if
          NU=KRA-1
          if(((NU-NE1)/2)*2 == (NU-NE1)) then
            if((ITRIG(KS(1),KS(4),NU+NU+3) /= 0) .AND.  &
               (ITRIG(KS(2),KS(3),NU+NU+3) /= 0)) then
              if(NU >= 0) then
                N=(NU-NE1)/2+1
                if(N < NE2) then
                  CALL CXK(S,IS,KAPS,NU,KRA,1,2)
                  DO MU = 1,12
                    CONE(MU,N)=CONE(MU,N)+AB*S(MU)
                  END DO
                END if
              END if
            END if
          END if
        END if
      END DO
      if (ICOLBREI == 2) then
        DO N = 1,NE2
          NU=NE1+2*(N-1)
          CALL TALK(JJA,JJB,NU,IS(1),IS(4),IS(2),IS(3),1,CONE(1,N))
          CALL TALK(JJA,JJB,NU,IS(4),IS(1),IS(3),IS(2),1,CONE(2,N))
          CALL TALK(JJA,JJB,NU,IS(1),IS(4),IS(3),IS(2),1,CONE(3,N))
          CALL TALK(JJA,JJB,NU,IS(4),IS(1),IS(2),IS(3),1,CONE(4,N))
          if(N == NE2) CYCLE
          NUP1=NU+1
          CALL TALK(JJA,JJB,NUP1,IS(1),IS(4),IS(2),IS(3),2,CONE(5,N))
          CALL TALK(JJA,JJB,NUP1,IS(2),IS(3),IS(1),IS(4),2,CONE(6,N))
          CALL TALK(JJA,JJB,NUP1,IS(4),IS(1),IS(3),IS(2),2,CONE(7,N))
          CALL TALK(JJA,JJB,NUP1,IS(3),IS(2),IS(4),IS(1),2,CONE(8,N))
          CALL TALK(JJA,JJB,NUP1,IS(1),IS(4),IS(3),IS(2),2,CONE(9,N))
          CALL TALK(JJA,JJB,NUP1,IS(3),IS(2),IS(1),IS(4),2,CONE(10,N))
          CALL TALK(JJA,JJB,NUP1,IS(4),IS(1),IS(2),IS(3),2,CONE(11,N))
          CALL TALK(JJA,JJB,NUP1,IS(2),IS(3),IS(4),IS(1),2,CONE(12,N))
        END DO
      END if
      return
   10 WRITE(99,100)
  100 FORMAT(5X,'ERRO IN EL52  PMGG RAGG')
      STOP
      end subroutine EL52

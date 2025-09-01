!*******************************************************************
!                                                                  *
      subroutine EL33(JJJA,JJJB,JA,JB,JC,IREZ,JJA,JJB,JJC,JJD   &
                                                        ,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 08  -------------  *
!                                                                  *
!     THIS PACKAGE EVALUATED THE CASES - 2313, 3231, 3213, 2331    *
!                                                   ( + + - - ),   *
!     WHICH APPEARS IN CALCULATION MATRIX ELEMENTS BETWEEN         *
!     CONFIGURATIONS:                               N'1 = N1 - 1   *
!                                                   N'2 = N2 + 1   *
!                                                                  *
!     subroutine CALLED: COULOM,EILE,GG1233,ITREXG,IXJTIK,         *
!                      JFAZE,PERKO2,RECO,REC3,SIXJ,SPEAK           *
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
      use itrexg_I
      use itrig_I
      use ixjtik_I
      use jfaze_I
      use snrc_I
      use gg1233_I
      use coulom_I
      use speak_I
      use cxk_I
      use talk_I
      use sixj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJJA,JJJB,JA,JB,JC,IREZ,JJA,JJB,JJC,JJD &
                                                        ,ICOLBREI
!      dimension PMGG(30),RAGG(30),J(3)
!      dimension CONE(12,20),S(12),IS(4),KAPS(4),KS(4)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IA,IB,IC,IAT,II,IIA,IIB,IIC,IID,IP1,IP2,IG1,IG2,   &
                 IKK,INN,I1,I2,I3,I4,IBRD,IBRE,ifAZ,ifAZP,ifAZFRCS, &
                 JAA,JBB,JCC,JB1,J12,KRA,KRA1,L1,L2,L3,ND1,ND2,NE1, &
                 NE2,N,NN,NU,NUP1,MU
      integer, dimension(3) :: J
      integer, dimension(4) :: IS,KAPS,KS
      real(kind=real64)          :: A1,AA,AB,BB,QM1,QM2,QM3,QM4,RAG,RECC,SI
      real(kind=real64), dimension(12) :: S
      real(kind=real64), dimension(30) :: PMGG,RAGG
      real(kind=real64), dimension(12,20) :: CONE
!-----------------------------------------------
      if(NPEEL <= 1)return
      CALL EILE(JA,JB,JC,JAA,JBB,JCC)
      CALL RECO(JAA,JCC,JBB,JBB,2,IAT)
      if(IAT == 0)return
      IA=JLIST(JA)
      IB=JLIST(JB)
      IC=JLIST(JC)
      IIA=JLIST(JJA)
      IIB=JLIST(JJB)
      IIC=JLIST(JJC)
      IID=JLIST(JJD)
      QM1=HALF
      QM2=-HALF
      QM3=HALF
      QM4=-HALF
      CALL PERKO2(JA,JB,JC,JA,3)
      J(1)=ID1(3)
      J(2)=ID2(3)
      J(3)=ID3(3)
      L1=(J(1)+1)/2
      L2=(J(2)+1)/2
      L3=(J(3)+1)/2
      IP1=ITREXG(J(2),J(1),J(3),J(3),IKK)+1
      if(IKK <= 0)return
      IG1=IP1+IKK-1
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
        if(IBRD <= 0 .AND. IBRE <= 0)return
        DO II=1,20
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
      DO I4=IP1,IG1,2
        KRA=(I4-1)/2
        KRA1=KRA+1
        if(KRA1 > 30)GO TO 10
        RAGG(KRA1)=ZERO
        PMGG(KRA1)=ZERO
        CALL REC3(JB,JA,JC,J(2),J(1),KRA*2,0,IAT,RECC)
        if(IAT == 0) CYCLE
        CALL GG1233(IK2,IK1,IK3,BK2,BK1,BK3,ID2,ID1,ID3,BD2,  &
                    BD1,BD3,KRA,QM1,QM2,QM3,QM4,RAG)
        if(DABS(RAG) < EPS) CYCLE
        RAGG(KRA1)=RAG
        CALL REC3(JB,JA,JC,J(2),J(1),KRA*2,1,IAT,RECC)
        PMGG(KRA1)=RECC
      END DO
      ifAZP=JFAZE(JB,JA,JC,JC)
!
!     TRANSFORM FANO & RACAH PHASE CONVENTION
!     TO CONDON & SHORTLEY PHASE CONVENTION
!
      ifAZFRCS=1
      ifAZ=IK1(5)*IK1(4)+IK2(5)*IK2(4)-ID1(5)*ID1(4)-ID2(5)*ID2(4)
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
      if(IREZ == 2)GO TO 5
! * * *                      * * *                      * * *
!     CASES 2313   + + - -        TRANSFORM TO  2133   + - + -
!           3231                                2133
!
    6 CONTINUE
      DO I1=IP1,IG1,2
        KRA=(I1-1)/2
        KRA1=KRA+1
        if(KRA1 > 30)GO TO 10
        if (ICOLBREI == 1) then
          CALL COULOM(L2,L3,L1,L3,ID2(5),ID3(5),ID1(5),ID3(5),KRA,A1)
          if(DABS(A1) < EPS) CYCLE
        END if
        AA=PMGG(KRA1)
        if(DABS(AA) < EPS) CYCLE
        AA=AA*RAGG(KRA1)
        if(DABS(AA) < EPS) CYCLE
        AA=AA/DSQRT(DBLE(I1))
        AA=AA*DBLE(ifAZP)
        if (ICOLBREI == 1) then
          BB=A1*AA*DBLE(ifAZFRCS)
          CALL SPEAK(JJJA,JJJB,IIA,IIB,IIC,IID,KRA,BB)
        else if (ICOLBREI == 2) then
          N=(KRA-ND1)/2+1
          CALL CXK(S,IS,KAPS,KRA,KRA,2,1)
          if(DABS(S(1)) > EPS) then
            BB=S(1)*AA
            if(DABS(BB) > EPS)                    &
          CALL TALK(JJJA,JJJB,KRA,IS(1),IS(3),IS(2),IS(4),3,BB)
          END if
        END if
      END DO
      if(IREZ == 2) GO TO 7
! * * *                      * * *                      * * *
!     CASES 3213   + + - -        TRANSFORM TO  2133   + - + -
!           2331                                2133
!
    5 IP2=ITREXG(J(3),J(1),J(2),J(3),IKK)+1
      if(IKK <= 0) return
      IG2=IP2+IKK-1
      DO I2=IP2,IG2,2
        KRA=(I2-1)/2
        if(KRA > 30)GO TO 10
        if (ICOLBREI == 1) then
          CALL COULOM(L3,L2,L1,L3,ID3(5),ID2(5),ID1(5),ID3(5),KRA,A1)
          if(DABS(A1) < EPS) CYCLE
        END if
        AB=ZERO
        DO I3=IP1,IG1,2
          J12=(I3-1)/2
          KRA1=J12+1
          if(KRA1 > 30)GO TO 10
          AA=PMGG(KRA1)
          if(DABS(AA) < EPS) CYCLE
          AA=AA*RAGG(KRA1)
          if(DABS(AA) < EPS) CYCLE
          if(IXJTIK(J(1),J(3),KRA*2,J(3),J(2),J12*2) == 0) CYCLE
          CALL SIXJ(J(1),J(3),KRA*2,J(3),J(2),J12*2,0,SI)
          AA=AA*SI*DSQRT(DBLE(I3))
          AB=AB+AA
        END DO
        if(DABS(AB) < EPS) CYCLE
        AB=AB*DBLE(ifAZP)
        if (ICOLBREI == 1) then
          BB=A1*AB*DBLE(ifAZFRCS)
          CALL SPEAK(JJJA,JJJB,IIA,IIB,IID,IIC,KRA,BB)
        else if (ICOLBREI == 2) then
          NU=KRA
          if(((NU-NE1)/2)*2 == (NU-NE1)) then
            if((ITRIG(KS(1),KS(4),NU+NU+1) /= 0) .AND.  &
               (ITRIG(KS(2),KS(3),NU+NU+1) /= 0)) then
              if(NU > 0) then
                N=(NU-NE1)/2+1
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
              if(NU >= 0) then
                N=(NU-NE1)/2+1
                if(N <= NE2) then
                  CALL CXK(S,IS,KAPS,NU,KRA,1,2)
                  DO MU = 1,4
                    CONE(MU,N)=CONE(MU,N)+AB*S(MU)
                  END DO
                END if
              END if
            END if
          END if
          NU=KRA-1
          if(((NU-NE1)/2)*2 == (NU-NE1)) then
            if((ITRIG(KS(1),KS(4),NU+NU+3) /= 0) .AND.  &
               (ITRIG(KS(2),KS(3),NU+NU+3) /= 0)) then
              if(NU >=  0) then
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
          CALL TALK(JJJA,JJJB,NU,IS(1),IS(4),IS(2),IS(3),1,CONE(1,N))
          CALL TALK(JJJA,JJJB,NU,IS(4),IS(1),IS(3),IS(2),1,CONE(2,N))
          CALL TALK(JJJA,JJJB,NU,IS(1),IS(4),IS(3),IS(2),1,CONE(3,N))
          CALL TALK(JJJA,JJJB,NU,IS(4),IS(1),IS(2),IS(3),1,CONE(4,N))
          if(N == NE2) CYCLE
          NUP1=NU+1
          CALL TALK(JJJA,JJJB,NUP1,IS(1),IS(4),IS(2),IS(3),2,CONE(5,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(2),IS(3),IS(1),IS(4),2,CONE(6,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(4),IS(1),IS(3),IS(2),2,CONE(7,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(3),IS(2),IS(4),IS(1),2,CONE(8,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(1),IS(4),IS(3),IS(2),2,CONE(9,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(3),IS(2),IS(1),IS(4),2,CONE(10,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(4),IS(1),IS(2),IS(3),2,CONE(11,N))
          CALL TALK(JJJA,JJJB,NUP1,IS(2),IS(3),IS(4),IS(1),2,CONE(12,N))
        END DO
      END if
      if(IREZ == 2)GO TO 6
    7 CONTINUE
      return
   10 WRITE(99,100)
  100 FORMAT(5X,'ERRO IN EL33  PMGG RAGG')
      STOP
      end subroutine EL33

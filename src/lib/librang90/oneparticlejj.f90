!*******************************************************************
!                                                                  *
      subroutine ONEPARTICLEJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL)
!                                                                  *
!   The main program for evaluating the reduced matrix elements of *
!   a one particle operator for configurations in jj-coupling.     *
!                                                                  *
!   Call(s) to: [LIB92]: CFP, FIXJ, GENSUM, ICHOP, IROW1, ISPAR,   *
!                        ITJPO, ITRIG, SETQNA, VIJOUT.             *
!               [NJGRAF]: NJGRAF.                                  *
!                                                                  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  NNNW
      use CONS_C
      use m_C
      use orb_C,   only: NW, NAK
      use dumx_C,  only: JLIS, JC1S, JC2S
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use itrig_I
      use ichkq1_I
      use ichop_I
      use ispar_I
      use itjpo_I
      use setqna_I
      use oneparticlejj1_I
      use oneparticlejj2_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: KA,IOPAR,JA,JB
      integer, intent(out) :: IA1,IA2
      real(kind=real64), intent(out) :: VSHELL(NNNW)
!      dimension VSHELL(NNNW)
!      dimension IS(2),KS(2)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KK,IJ,IDQ,JA1,JA2,JW,NDQ,NS,ISH,I,II,I1,IM,  &
                 KS1,KS2,NX,NPEELM
      integer, dimension(2) :: IS,KS
      real(kind=real64) :: TCOEFF
!-----------------------------------------------
      IA1 = 0
      KK = KA+KA+1
      if(ITRIG(ITJPO(JA),ITJPO(JB),KK).EQ.0)return
      if((IOPAR.NE.0).AND.(ISPAR(JA)*ISPAR(JB)*IOPAR.NE.1))return
      if(ICHKQ1(JA,JB).EQ.0)return
!
      CALL SETQNA (JA,JB)
!
      DO IJ = 1,NW
         VSHELL(IJ) = ZERO
      END DO
!
!   Analyse peel shell interactions
      IDQ = 0
      JA1 = 0
      JA2 = 0
      if (NPEEL .NE. 0) then
        DO JW = 1,NPEEL
          IJ = JLIST(JW)
          NDQ = NQ1(IJ)-NQ2(IJ)
          if (ABS (NDQ) .GT. 1) return
          if (NDQ .GT. 0) then
            JA1 = JW
            IDQ = IDQ+1
          elseif (NDQ .LT. 0) then
            JA2 = JW
            IDQ = IDQ+1
          endif
        END DO
        if (IDQ .GT. 2) return
!
!   Evaluate the array VSHELL
!
!   Then there are two possibilities IDQ = 0 or IDQ = 2
!   if IDQ = 0, then loop over all shells by index ISH
!   if IDQ = 2, then one orbital fixed on each side
        NS = NPEEL
      endif
      if (IDQ .EQ. 2) GOTO 19
!
!   Loop over shells when IDQ = 0
      ISH = 0
      if (NPEEL .EQ. 0) GOTO 9
      DO I = 1,NPEEL
         JLIS(I) = JLIST(I)
      END DO
      if (NPEEL .EQ. 1) GOTO 9
      NPEELM = NPEEL-1
      DO I = 1,NPEELM
         JC1S(I) = JJC1(I)
         JC2S(I) = JJC2(I)
      END DO
!
!   If ISH .GT. NW, then loop is over and return
    9 ISH = ISH+1
      if (ISH .GT. NW) return
      if (ICHOP (ISH,JA) .EQ. -1) GOTO 9
      if (ICHOP (ISH,JA) .EQ. 0) GOTO 16
!
!   Case one --- the ISH-th shell is in the core or in the peel and
!   closed for both sides
      I = 1
      if (NPEEL.EQ.0) GOTO 15
      DO I = 1,NPEEL
        IJ = JLIST(I)
        if (ISH .LT. IJ) GOTO 11
      END DO
      I = NPEEL+1
      GOTO 13
   11 IM = NPEEL-I+1
      DO II = 1,IM
         JLIST(NPEEL+2-II) = JLIST(NPEEL+1-II)
         if (NPEEL.EQ.II) GOTO 13
         JJC1(NPEEL+1-II) = JJC1(NPEEL-II)
         JJC2(NPEEL+1-II) = JJC2(NPEEL-II)
      END DO
   13 CONTINUE
      if (I .LT. 3) GOTO 14
      JJC1(I-1) = JJC1(I-2)
      JJC2(I-1) = JJC2(I-2)
      GOTO 15
   14 I1 = JLIST(1)
      JJC1(1) = JJQ1(3,I1)
      JJC2(1) = JJQ2(3,I1)
   15 JLIST(I) = ISH
      JA1 = I
      JA2 = I
      NS = NPEEL+1
      GOTO 19
!
!   Case two --- the ISH-th shell is in the peel and open for either
!   side
   16 NS = NPEEL
      DO JW = 1,NPEEL
        NX = ISH-JLIST(JW)
        if (NX.EQ.0) GOTO 18
      END DO
   18 JA1 = JW
      JA2 = JW
!
!   Main computation
!
!     JA1, JA2 are the indices of interacting shells in JLIST
!     IA1, IA2 are the indices of interacting shells in NW
   19 IA1 = JLIST(JA1)
      IA2 = JLIST(JA2)
      KS1 = 2*ABS (NAK(IA1))
      KS2 = 2*ABS (NAK(IA2))
!
!   Check triangular condition for the active shells
      if (ITRIG (KS1,KS2,KK).EQ.1) GOTO 99
      if (IDQ .EQ. 2) return
      GOTO 100
!
!   Set tables of quantum numbers of non-interacting spectator shells
   99 CONTINUE
      if (IDQ .EQ. 0) then
!GG
!GG   Cia orginalioje programoje yra klaida
!GG
!GG        if(JA .EQ. JB) then
          CALL ONEPARTICLEJJ1(NS,KA,JA,JB,JA1,JA2,TCOEFF)
!GG          CALL ONESCALAR1(NS,JA,JB,JA1,JA2,TCOEFF)
!GG        else
!GG          TCOEFF = 0.0D 00
!GG        END if
      else if (IDQ .EQ. 2) then
!
!   IDQ = 2 Case
!
!       Permutation factor for IDQ = 2
        CALL ONEPARTICLEJJ2(NS,KA,JA1,JA2,TCOEFF)
!GG        CALL ONESCALAR2(JA,JB,JA1,JA2,TCOEFF)
        VSHELL(1) = TCOEFF
        return
      END if
!
!   End of loop over parent states
!
!
!   IDQ = 0 CASE
      VSHELL(ISH) = TCOEFF
!
!   Loop over all shells when IDQ = 0
100   CONTINUE
      if (NPEEL .EQ. 0) GOTO 9
      DO I = 1,NPEEL
         JLIST(I) = JLIS(I)
      END DO
      if (NPEEL .EQ. 1) GOTO 9
      NPEELM = NPEEL-1
      DO  I = 1,NPEELM
         JJC1(I)  = JC1S(I)
         JJC2(I)  = JC2S(I)
      END DO
      GOTO 9
      return
      end subroutine ONEPARTICLEJJ

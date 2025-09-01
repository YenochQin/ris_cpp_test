!***********************************************************************
!                                                                      *
      subroutine RKCO_GG (JA,JB,CORD,INCOR,ICOLBREI)
!                                                                      *
!   Configurations JA, JB. Analyse the tables of quantum numbers set   *
!   in the COMMON  blocks M0 , M1, M2, M3  to determine all possible   *
!   sets of interacting  orbitals which give a non-vanishing Coulomb   *
!   matrix element,  and  initiates the calculation of coefficients.   *
!   The following conventions are in force: (1) labels 1, 2 refer to   *
!   left, right sides of matrix element respectively;   (2) pointers   *
!   JA1, JB1, JA2, JB2 point to the JLIST array of active  orbitals;   *
!   IA1, IB1, IA2, IB2 point to the complete list of orbitals.         *
!                                                                      *
!   Call(s) to: [LIB92]: COR, CORD, ISPAR, ITJPO, SETQNA, VIJOUT.      *
!                                                                      *
!   Rewrite by G. Gaigalas                                             *
!   Transform to fortran 90/95 by G. Gaigalas           December 2012  *
!   The last modification made by G. Gaigalas           October  2020  *
!                                                                      *
!***********************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use m_C
      use orb_C
      use dumx_C
!
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
!      use cord_I
      use itjpo_I
      use ispar_I
      use ichkq2_I
      use setqna_I
      use el1_I
      use el2_I
      use el3_I
      use el4_I
      use el5_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!      EXTERNAL CORD
      integer, intent(in) :: JA,JB,INCOR,ICOLBREI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I,IB1,IB2,IDQ,IDQG,II,I1,IJ,IJW,IM,IPCA,IT1,IT2, &
                 JA1,JB1,JA2,JB2,J,JW,JT1,JT2,JT3,NDQ, &
                 KLAST,KW,KWA,K1,KW1,KW2,NPEELM
!-----------------------------------------------
!
!   The Hamiltonian is an even scalar operator
      if (ITJPO (JA) .NE. ITJPO (JB)) return
      if (ISPAR (JA) .NE. ISPAR (JB)) return
      if (ICHKQ2(JA,JB) .EQ. 0) return
      CALL SETQNA (JA,JB)
!
!   1.0 Analyse peel shell interactions
!
!   1.1 Analyse electron distribution in peel. (The full procedure is
!       needed only if the number of peel orbitals NPEEL .GE. 2)
      if (NW .LT. 1) then
         PRINT *, 'RKCO_GG: No subshells.'
         STOP
      endif
      if (NPEEL .EQ. 0) GOTO 48
      if (NPEEL .EQ. 1) GOTO 43
!
!   Find differences in occupations, NDQ, for each peel orbital in
!   turn and use to set up labels of active orbitals maintaining the
!   convention JA1 .LE. JB1, JA2 .LE. JB2.
      IDQ = 0
      JA1 = 0
      JB1 = 0
      JA2 = 0
      JB2 = 0
      IDQG = 0
      DO JW = 1,NPEEL
         J = JLIST(JW)
         NDQ = NQ1(J) - NQ2(J)
         if (IABS (NDQ) .GT. 2) return
         if (NDQ .LT. 0) then
           if (NDQ+1 .GT. 0) then
             CYCLE
           else if (NDQ+1 .EQ. 0) then
             if (JA2 .GT. 0) then
               JB2 = JW
             else
               JA2 = JW
             END if
             IDQ = IDQ+1
             IDQG=1+IDQG
           else if (NDQ+1 .LT. 0) then
             JA2 = JW
             IDQ = IDQ+2
             IDQG=20+IDQG
           END if
         else
           if (NDQ-1 .GT. 0) then
             JA1 = JW
             IDQ = IDQ+2
             IDQG=20+IDQG
             CYCLE
           else if (NDQ-1 .EQ. 0) then
             if (JA1 .GT. 0) then
               JB1 = JW
             else
               JA1 = JW
             END if
             IDQ = IDQ+1
             IDQG=1+IDQG
             CYCLE
           else
             CYCLE
           END if
         END if
      END DO
!
!   1.2 Calculate coefficients for all possible sets of active shells.
!
!   There are 4 cases, depending on the value of IDQ, the sum of the
!   absolute differences NDQ:
!
!   1.2.1 IDQ .GT. 4: matrix element null
      if (IDQ .GT. 4) return
      if (IDQ .EQ. 4) then
!
!   1.2.2 IDQ .EQ. 4: matrix element uniquely defined
        if (JB1 .EQ. 0) then
          JB1 = JA1
        END if
        if (JB2 .EQ. 0) then
          JB2 = JA2
        END if
        CONTINUE
        if(IDQG.NE.40) then
          if(IDQG.NE.22) then
!
!         TARP KONFIGURACIJU
!                        KAI      N'1=N1+-1
!                                 N'2=N2+-1
!                        KAI      N'3=N3+-1
!                                 N'4=N4+-1
            CALL EL5(JA,JB,JA1,JB1,JA2,JB2,ICOLBREI)
          else
            CALL EL4(JA,JB,JA1,JB1,JA2,JB2,ICOLBREI)
          END if
        else
!
!       TARP KONFIGURACIJU
!                        KAI      N'1=N1+1
!                                 N'2=N2-1
          CALL EL2(JA,JB,JA1,JA2,ICOLBREI)
        END if
        return
      END if
!
      if (IDQ .NE. 2) then
        if (IDQ .NE. 0) then
!
!         3.0 Diagnostic print - NW .LT. 1
          WRITE (*,300)
          STOP
        END if
      else
        KLAST = 1
        GOTO 16
      END if
!
      if (JA .EQ. JB) GOTO 43
      KLAST = NPEEL
!
!   1.2.3 IDQ .EQ. 2: One orbital fixed each side include all
!                     possible spectators.
!
!   Also IDQ .EQ. 0 for a matrix element off-diagonal in coupling
!   only. Must sum over all pairs of orbitals excluding core-core
!   terms
   16 DO KWA = 1,KLAST
         if (IDQ .NE. 2) then
           JA1 = KWA
           JA2 = KWA
         END if
         JT1 = JA1
         JT2 = JA2
         IT1 = JLIST(JA1)
         IT2 = JLIST(JA2)
         DO KW = KWA,NPEEL
            K1 = JLIST(KW)
            if (NQ1(K1)*NQ2(K1) .EQ. 0) CYCLE
            JB1 = KW
            JB2 = KW
            JA1 = JT1
            JA2 = JT2
!
!   Interchange JA1 and JB1 and/or JA2 and JB2 if necessary
            if (JA1-JB1 .GT. 0) then
              JT3 = JB1
              JB1 = JA1
              JA1 = JT3
            else if (JA1-JB1 .EQ. 0) then
              IB1 = JLIST(JB1)
              if (NQ1(IB1) .LE. 1) CYCLE
            END if
            if (JA2-JB2 .GT. 0) then
              JT3 = JB2
              JB2 = JA2
              JA2 = JT3
            else if (JA2-JB2 .EQ. 0) then
              IB2 = JLIST(JB2)
              if (NQ2(IB2) .LE. 1) CYCLE
            END if
            if(IDQ.NE.0) then
!
!     TARP KONFIGURACIJU
!                        KAI      N'1=N1+1
!                                 N'2=N2-1
!                                 N'3=N3
              CALL EL3(JA,JB,JA1,JB1,JA2,JB2,ICOLBREI)
            else
!
!     TARP TU PACIU KONFIGURACIJU
              CALL EL1(JA,JB,JA1,JB1,1,ICOLBREI)
            END if
         END DO
         if ((IDQ .EQ. 0) .AND. (NCORE .EQ. 0)) CYCLE
         if ((NCORE .EQ. 0) .OR. (NAK(IT1) .NE. NAK(IT2))) return
!
!   This section calculates the terms arising from active electrons
!   which are in closed shells
         NPEELM = NPEEL-1
         DO I = 1,NPEEL
            JLIS(I) = JLIST(I)
         END DO
         DO I = 1,NPEELM
            JC1S(I) = JJC1(I)
            JC2S(I) = JJC2(I)
         END DO
         DO KW = 1,NCORE
            IJW = KLIST(KW)
            DO I = 1,NPEEL
               IJ = JLIST(I)
               if (IJW .LT. IJ) GOTO 29
            END DO
            I = NPEEL+1
            GOTO 31
   29       IM = NPEEL-I+1
            DO II = 1,IM
               JLIST(NPEEL+2-II) = JLIST(NPEEL+1-II)
               if (NPEEL .EQ. II) GOTO 31
               JJC1(NPEEL+1-II) = JJC1(NPEEL-II)
               JJC2(NPEEL+1-II) = JJC2(NPEEL-II)
            END DO
   31       CONTINUE
            if (I .LT. 3) then
              I1 = JLIST(1)
              JJC1(1) = JJQ1(3,I1)
              JJC2(1) = JJQ2(3,I1)
            else
              JJC1(I-1) = JJC1(I-2)
              JJC2(I-1) = JJC2(I-2)
            END if
            JLIST(I) = IJW
            JA1 = JT1
            if (JT1 .GE. I) JA1 = JA1+1
            JB1 = I
            JA2 = JT2
            if (JT2 .GE. I) JA2 = JA2+1
            JB2 = I
            if (JA1-JB1 .GT. 0) then
              JT3 = JB1
              JB1 = JA1
              JA1 = JT3
            END if
            if (JA2-JB2 .GT. 0) then
              JT3 = JB2
              JB2 = JA2
              JA2 = JT3
            END if
            NPEEL = NPEEL+1
            if(IDQ.NE.0) then
              if(IDQG.NE.40) then
                if(IDQG.NE.2) then
                   WRITE(99,995)
                   return
                else
!
!     TARP KONFIGURACIJU
!                        KAI      N'1=N1+1
!                                 N'2=N2-1
!                                 N'3=N3
                  CALL EL3(JA,JB,JA1,JB1,JA2,JB2,ICOLBREI)
                END if
              else
                WRITE(99,994)
                return
              END if
            else
!
!     TARP TU PACIU KONFIGURACIJU
               CALL EL1(JA,JB,JA1,JB1,1,ICOLBREI)
            END if
            NPEEL = NPEEL-1
            NPEELM = NPEEL-1
            DO I = 1,NPEEL
               JLIST(I) = JLIS(I)
            END DO
            DO I = 1,NPEELM
               JJC1(I)  = JC1S(I)
               JJC2(I)  = JC2S(I)
            END DO
         END DO
      END DO
      return
!
!   1.2.4 IDQ .EQ. 0 - diagonal case. Include all pairs with
!         JA1 = JA2, JB1 = JB2.
   43 DO KW1 = 1,NPEEL
         K1 = JLIST(KW1)
         JB1 = KW1
         JB2 = KW1
         DO KW2 = 1,KW1
            JA1 = KW2
            if (JA1 .EQ. JB1) then
              if (NQ1(K1) .LE. 1) CYCLE
            END if
            JA2 = JA1
            if(JA /= JB) then
              if(NPEEL == 1 .AND. NQ1(K1) == NQ2(K1)) then
!
!               TARP TU PACIU BuseNU
                CALL EL1(JA,JB,JA1,JB1,1,ICOLBREI)
              else if(IDQG.NE.2) then
                WRITE(99,996)
                WRITE(99,*)"JA,JB,JA1,JB1",JA,JB,JA1,JB1
                WRITE(99,*)"IDQG IDQ",IDQG,IDQ
                return
              else
!
!                TARP KONFIGURACIJU
!                        KAI      N'1=N1+1
!                                 N'2=N2-1
!                                 N'3=N3
                 CALL EL3(JA,JB,JA1,JB1,JA2,JB2,ICOLBREI)
              END if
            else
!
!             TARP TU PACIU BuseNU
              CALL EL1(JA,JB,JA1,JB1,0,ICOLBREI)
            END if
         END DO
      END DO
   48 if (INCOR .LT. 1) return
      if (NCORE .EQ. 0) return
!
!   2.0 The diagonal case. deal with contributions from core orbitals
!       if INCOR .EQ. 1.
      if(JA.NE.JB) return
      DO KW1 = 1,NCORE
         JB1 = KW1
         JB2 = KW1
!
!   2.1 Calculate contribution from core/core terms
         IPCA = 2
         DO KW2 = 1,KW1
            JA1 = KW2
            JA2 = KW2
            CALL CORD (JA,JB,JA1,IPCA,JB1)
         END DO
!
!   2.2 Calculate contribution from peel/core terms
         if (NPEEL .EQ. 0) CYCLE
         IPCA = 1
         DO KW2 = 1,NPEEL
            JA1 = KW2
            JA2 = KW2
            CALL CORD (JA,JB,JA1,IPCA,JB1)
         END DO
      END DO
      return
  300 FORMAT ('RKCO_GG: Error.')
  994 FORMAT('   rie zymes 38?? atv N=N-N  !!!!!!')
  995 FORMAT('   rie zymes 38 atv N=N-N  !!!!!!')
  996 FORMAT('   rie zymes 45 atv N=N-N  !!!!!!')
      end subroutine RKCO_GG

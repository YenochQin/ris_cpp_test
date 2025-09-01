!***********************************************************************
!                                                                      *
 !                                                                      *
!   The value of this  function is the one-electron integral I (J,K)   *
!   for  orbitals  J, K. The analytical expression for this quantity   *
!   is given as  eq (9) in  I P Grant, B J McKenzie, P H Norrington,   *
!   D F Mayers, and N C Pyper,  Computer  Phys Commun 21 (1980) 211 .  *
!                                                                      *
!   Call(s) to: [LIB92]: DPBDT, QUAD.                                  *
!                                                                      *
!   Written by Farid A Parpia, at Oxford  Last revision: 06 Oct 1992   *
!                                                                      *
!   Modified by C. Naz\'e  Oct. 2011                                   *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNN1
      use def_C
      use grid_C
      use wave_C
      use orb_C
      use tatb_C
      use debug_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dpbdt_I
      use quad_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(kind=real64), intent(out) :: VALNMSK123, VALNMSK1
      integer, intent(in) :: J
      integer, intent(in) :: K
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(kind=real64), dimension(NNN1) :: PA, PB, QA, QB
      real(kind=real64) :: A1, A2, PIECE1, PIECE2, PIECE3, PIECE4
      integer :: L, KAP1, KAP2, I, L1, L2, J1, J2, L2_TILDA
!-----------------------------------------------
!
!
!   Set KAP1 and KAP2
!
!C*      RINTI_NMS = 0.0D 00
      VALNMSK1   = 0.0D00
      VALNMSK123 = 0.0D00
      KAP1 = NAK(J)
      KAP2 = NAK(K)
!
!   Stop if orbitals J and K have different kappa values
!
      if (KAP1 .NE. KAP2) then
         WRITE (*,300) NP(J),NH(J),NP(K),NH(K)
         STOP
      endif
!
!   Determine the l quantum numbers
!
      if (KAP1 .GT. 0) then
         L1 =  KAP1
      else
         L1 = -KAP1-1
      endif
!
      if (KAP2 .GT. 0) then
         L2 =  KAP2
      else
         L2 = -KAP2-1
      endif
      if (L1 .NE. L2) return
!
!   Determine the j quantum numbers and l_2 tilda
!
      J1 = 2*IABS(NAK(J))-1
      J2 = 2*IABS(NAK(K))-1
      L2_TILDA = J2 - L2
!
      MTP = MAX (MF(J),MF(K))
!
!   Kinetic energy contribution
!
!   Piece involving derivatives
!
      CALL DPBDT (J)
      PA(1) = 0.0D00
      QA(1) = 0.0D00
      DO 11 I = 2,MTP
        PA(I) = TA(I)
        QA(I) = TB(I)
   11 CONTINUE
      CALL DPBDT (K)
      PB(1) = 0.0D00
      QB(1) = 0.0D00
      DO 12 I = 2,MTP
        PB(I) = TA(I)
        QB(I) = TB(I)
   12 CONTINUE
      TA(1) = 0.0D00
      DO 1 I = 2,MTP
         TA(I) = (PA(I)*PB(I)+QA(I)*QB(I))/RP(I)
    1 CONTINUE
      CALL QUAD (PIECE1)
      PIECE1 = PIECE1/(H*H)
!
      A1 = DBLE(L2*(L2+1))
      A2 = DBLE(L2_TILDA*(L2_TILDA+1))
      TA(1) = 0.0D00
      DO 2 I = 2,MTP
        TA(I) = RP(I)                                                  &
              *(A1*PF(I,J)*PF(I,K)+A2*QF(I,J)*QF(I,K))                 &
              /(R(I)*R(I))
    2 CONTINUE
      CALL QUAD (PIECE2)
!
!   Pieces not involving derivatives
!
      TA(1) = 0.D0
      DO 3 I = 2,MTP
         TA(I) = (QF(I,J)*PB(I)+QF(I,K)*PA(I))/R(I)
    3 CONTINUE
      CALL QUAD (PIECE3)
      PIECE3 = -2.0*Z*PIECE3/(C*H)
!
      if (KAP2 .NE. 1) then
        TA(1) = 0.0D00
        DO 4 I = 2,MTP
           TA(I) = RP(I)                                               &
                 * (QF(I,J)*PF(I,K)+PF(I,J)*QF(I,K))                   &
                 /(R(I)*R(I))
    4   CONTINUE
        CALL QUAD (PIECE4)
        PIECE4 = -PIECE4*Z*(DBLE (KAP2)-1.0)/C
      else
        PIECE4 = 0.0D00
      END if
      VALNMSK1 = 0.5*(PIECE1+PIECE2)
      VALNMSK123 = 0.5*(PIECE1+PIECE2+PIECE3+PIECE4)
!
!   Debug printout
!
!C*      LDBPR(5)=.TRUE.
      if (LDBPR(5))                                                    &
         WRITE (99,302) NP(J),NH(J),NP(K),NH(K),VALNMSK123
!
!C*      return
!
  300 FORMAT ('RINTI_NMS: Attempt to calculate I(',                    &
               1I2,1A2,',',1I2,1A2,')')
  302 FORMAT (/' K (',1I2,1A2,',',1I2,1A2,') = ',1PD19.12)
!
      return
      end subroutine RINTI_NMS

!***********************************************************************
!                                                                      *
      subroutine START(IORB, ITYPE, P0, P, Q0, Q)
!                                                                      *
!   This subroutine sets up  P(1:6), Q(1:6),  required  to start the   *
!   integration for programs  OUT  and  SBSTEP .                       *
!                                                                      *
!   Arguments:                                                         *
!                                                                      *
!      IORB : (Input) Index of the orbital                             *
!      ITYPE: (Input) 1 = homogeneous equation; 2 = inhomogeneous      *
!             equation; 3 = variational equation                       *
!      P0   : (Input) Slope parameter                                  *
!      P    : (Output) P(1:6) are tabulated by this program            *
!      Q0   : (Output) First term in the series expansion of Q         *
!      Q    : (Output) Q(1:6) are tabulated by this program            *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 09 Dec 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:48   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only: NNNP
      use CNC_C
      use DEF_C,           only: C, Z, ACCY
      use GRID_C
      use NPAR_C
      use ORB_C
      use POTE_C,          only: YP, XP, XQ
      use SCF_C,           only: NDCOF, NDA, DA
      use WAVE_C,          only: PZ,PF,QF
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: IORB
      integer, intent(in) :: ITYPE
      real(kind=real64), intent(in) :: P0
      real(kind=real64), intent(out) :: Q0
      real(kind=real64), dimension(NNNP), INTENT(INOUT) :: P
      real(kind=real64), dimension(NNNP), INTENT(INOUT) :: Q
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: MXITER=36, KIORB, I, JORB, NITER, J
      real(kind=real64), dimension(6) :: RDP, RDQ, RSEP, RSEQ
      real(kind=real64), dimension(2:6) :: SPEST, SQEST
      real(kind=real64) :: OBC, ZONC, GIORB, FKIORB, OMGI, OMGMK, OMGPK, RSEP1, &
         RSEQ1, PZERO, P1, Q1, SUMP, SUMQ, FACTOR, CSQ, TWOCSQ, ENERGY, ENEFAC&
         , RI, RPI, RIRPI, YPIRPI, DifMAW, DifMAX, COEFIJ, PI, QI, RJ
!-----------------------------------------------
!
!
!   Initialization
!
      OBC = 1.0D00/C
      ZONC = Z*OBC
      GIORB = GAMA(IORB)
      KIORB = NAK(IORB)
      FKIORB = DBLE(KIORB)
      OMGI = 1.0D00 - GIORB
!
      OMGMK = OMGI - FKIORB
      OMGPK = OMGI + FKIORB
!
!   Determine P(1), Q(1): THESE STORE  R**(-GAMMA)*(P(1),Q(1));
!   set up  RSEP  and  RSEQ , the inhomogeneous terms
!
      if (ITYPE==1 .OR. ITYPE==2) then
         if (NPARM == 0) then
            P(1) = P0
            if (KIORB < 0) then
               Q(1) = -P0*ZONC/(GIORB - FKIORB)
            else
               Q(1) = P0*(GIORB + FKIORB)/ZONC
            endif
         else
            if (KIORB < 0) then
               P(1) = P0
               Q(1) = 0.0D00
            else
               P(1) = 0.0D00
               Q(1) = P0*(GIORB + FKIORB)/ZONC
            endif
         endif
         if (ITYPE == 1) then
            RSEP = 0.0D00
            RSEQ = 0.0D00
         else
            RSEP1 = 0.0D00
            RSEQ1 = 0.0D00
            DO I = 1, NDCOF
               JORB = NDA(I)
               PZERO = PZ(JORB)
               if (NPARM == 0) then
                  P1 = PZERO
                  if (KIORB < 0) then
                     Q1 = -PZERO*ZONC/(GIORB - FKIORB)
                  else
                     Q1 = PZERO*(GIORB + FKIORB)/ZONC
                  endif
                  SUMP = ZONC*Q1
                  SUMQ = -ZONC*P1
               else
                  if (KIORB < 0) then
                     P1 = PZERO
                     Q1 = 0.0D00
                  else
                     P1 = 0.0D00
                     Q1 = PZERO*(GIORB + FKIORB)/ZONC
                  endif
                  SUMP = 0.0D00
                  SUMQ = 0.0D00
               endif
               FACTOR = DA(I)
               RSEP1 = RSEP1 + FACTOR*(SUMP + OMGMK*P1)
               RSEQ1 = RSEQ1 + FACTOR*(SUMQ + OMGPK*Q1)
            END DO
            FACTOR = RP(1)
            RSEP(1) = FACTOR*RSEP1
            RSEQ(1) = FACTOR*RSEQ1
            DO I = 2, 6
               FACTOR = -RP(I)*R(I)**(-GIORB)
               RSEP(I) = FACTOR*XP(I)
               RSEQ(I) = FACTOR*XQ(I)
            END DO
         endif
      else if (ITYPE == 3) then
         P(1) = 0.0D00
         Q(1) = 0.0D00
         RSEP(1) = 0.0D00
         RSEQ(1) = 0.0D00
         DO I = 2, 6
            FACTOR = OBC*RP(I)*R(I)**OMGI
            RSEP(I) = -FACTOR*QF(I,IORB)
            RSEQ(I) = FACTOR*PF(I,IORB)
         END DO
      endif
      Q0 = Q(1)
!
!   Set up  RDP  and  RDQ
!
      CSQ = C*C
      TWOCSQ = CSQ + CSQ
      ENERGY = E(IORB)
      ENEFAC = TWOCSQ - ENERGY
      DO I = 1, 6
         RI = R(I)
         RPI = RP(I)
         RIRPI = RI*RPI
         YPIRPI = YP(I)*RPI
         RDP(I) = -OBC*(ENEFAC*RIRPI + YPIRPI)
         RDQ(I) = -OBC*(ENERGY*RIRPI - YPIRPI)
      END DO
!
!   Determine  P(2:6) , Q(2:6)
!
!   Initilizations for the iterations
!
      NITER = 0
      P1 = P(1)
      Q1 = Q(1)
      DifMAW = MAX(ABS(P1),ABS(Q1))
!
      P(2:6) = P1
      Q(2:6) = Q1
!
!   This is the largest factor by which any result will be
!   multiplied
!
      FACTOR = R(6)**GIORB
!
!   Now iterate
!
    7 CONTINUE
      NITER = NITER + 1
      DifMAX = 0.0D00
      DO J = 2, 6
         SUMP = SUM(CNC6C(:,J)*(OMGMK*RP(:6)*P(:6)-RDP*Q(:6)+RSEP))
         SUMQ = SUM(CNC6C(:,J)*(OMGPK*RP(:6)*Q(:6)-RDQ*P(:6)+RSEQ))
         RJ = R(J)
         SUMP = SUMP/RJ
         SUMQ = SUMQ/RJ
         SPEST(J) = SUMP
         SQEST(J) = SUMQ
         DifMAX = MAX(DifMAX,ABS(SUMP - P(J)))
         DifMAX = MAX(DifMAX,ABS(SUMQ - Q(J)))
      END DO
!zou  if (DifMAX .LT. DifMAW) then
      P(2:6) = SPEST(:6)
      Q(2:6) = SQEST(:6)
      DifMAW = DifMAX
      DifMAX = DifMAX*FACTOR
      if (DifMAX > ACCY) then
         if (NITER < MXITER) then
            GO TO 7
         endif
      endif
!     else
!        DifMAX = DifMAX*FACTOR
!        if (DifMAX .GT. ACCY) then
!           WRITE (*,300) NP(IORB),NH(IORB),DifMAX,NITER,ACCY
!        endif
!zou  endif
! not convergent, using the initial P,Q
      if (DifMAX > ACCY) then
         P(2:6) = P1
         Q(2:6) = Q1
      endif
!zou
!
!   All done
!
!   This is always true in GRASP2
!
      P(1) = 0.0D00
      Q(1) = 0.0D00
!
      DO I = 2, 6
         FACTOR = R(I)**GIORB
         P(I) = FACTOR*P(I)
         Q(I) = FACTOR*Q(I)
      END DO
!
      return
!
  300 FORMAT('START: ',1I2,1A2,' subshell: accuracy ',1P,1D8.1,/,&
         ' attained after ',1I2,' iterations; this fails the'/,&
         ' accuracy criterion ',D8.1,'.')
      return
!
      end subroutine START

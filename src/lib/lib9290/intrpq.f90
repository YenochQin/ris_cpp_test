!***********************************************************************
!                                                                      *
      subroutine INTRPQ(PA, QA, MA, RA, J, DNORM)
!                                                                      *
!   This  subprogram  interpolates  the  arrays  PA(1:MA), QA(1:MA),   *
!   tabulated on grid RA(1:MA) into the COMMON arrays PF(1:MF(J),J),   *
!   QF(1:MF(J),J). (Aitken's  algorithm is used. See F B Hildebrand,   *
!   Introduction  to  Numerical  Analysis, 2nd ed., McGraw-Hill, New   *
!   York, NY, 1974.) The orbital is renormalized.                      *
!                                                                      *
!   subroutines called: RINT.                                          *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 14 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  15:37:49   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only: NNNP
      use DEBUG_C
      use DEF_C,           only: ACCY
      use GRID_C
      use WAVE_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rint_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: MA
      integer  :: J
      real(real64), intent(out) :: DNORM
      real(real64), dimension(*), intent(in) :: PA, QA, RA
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: MXORD = 13
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I, MFJ, NRSTLO, KOUNT, IROW, K, NRSTHI, LLO, LHI, LOCNXT, &
         ILIROK, ILDIAG, ILOTHR, MFJP1
      real(real64) :: RAMA
      real(real64), dimension(MXORD) :: X, DX
      real(real64), dimension((MXORD*(MXORD + 1))/2) :: POLYP, POLYQ
      real(real64) :: RN, XBAR, PESTL, QESTL, DifF, DifFT, DXKMN1, DXIROW, &
         FACTOR, PESTT, QESTT, DPBP, DQBQ, DNFAC
      logical :: SET
      logical, dimension(NNNP) :: useD
!-----------------------------------------------
!
!
!   This function dispenses with the need for a two-dimensional
!   array
!
!
!   Initialization
!
      RAMA = RA(MA)
      RN = R(N)
!
!   This is always true in GRASP
!
      PF(1,J) = 0.0D00
      QF(1,J) = 0.0D00
!
!   Checks
!
      if (RAMA > RN) then
         WRITE (*, 300) RN, RAMA
         PRINT*, "N =",N,"MA =",MA
         STOP
      endif
!
!   Determine end of grid
!
      I = N
    1 CONTINUE
      I = I - 1
      if (R(I) <= RAMA) then
         MFJ = I
      else
         GO TO 1
      endif
      MF(J) = MFJ
!
!   Overall initialization for interpolation
!
      NRSTLO = 0
      KOUNT = 0
!
!   Perform interpolation
!
      DO I = 2, MFJ
!
!   Initialization for interpolation
!
         XBAR = R(I)
         IROW = 0
         PESTL = 0.0D00
         QESTL = 0.0D00
!
!   Determine the nearest two grid points bounding the present
!   grid point
!
    2    CONTINUE
         K = NRSTLO + 1
         if (RA(K) < XBAR) then
            NRSTLO = K
            GO TO 2
         else
            NRSTHI = K
         endif
!
!   Clear relevant piece of use-indicator array
!
         LLO = MAX(NRSTLO - MXORD,1)
         LHI = MIN(NRSTHI + MXORD,MA)
         useD(LLO:LHI) = .FALSE.
!
!   Determine next-nearest grid point
!
    4    CONTINUE
         IROW = IROW + 1
         LLO = MAX(NRSTLO - IROW + 1,1)
         LHI = MIN(NRSTHI + IROW - 1,MA)
         SET = .FALSE.
         DO K = LLO, LHI
            if (useD(K)) CYCLE
            if (.NOT.SET) then
               DifF = RA(K) - XBAR
               LOCNXT = K
               SET = .TRUE.
            else
               DifFT = RA(K) - XBAR
               if (ABS(DifFT) < ABS(DifF)) then
                  DifF = DifFT
                  LOCNXT = K
               endif
            endif
         END DO
         useD(LOCNXT) = .TRUE.
         X(IROW) = RA(LOCNXT)
         DX(IROW) = DifF
!
!   Fill table for this row
!
         DO K = 1, IROW
            ILIROK = ILOC(IROW,K)
            if (K == 1) then
               POLYP(ILIROK) = PA(LOCNXT)
               POLYQ(ILIROK) = QA(LOCNXT)
            else
               ILDIAG = ILOC(K - 1,K - 1)
               ILOTHR = ILOC(IROW,K - 1)
               DXKMN1 = DX(K-1)
               DXIROW = DX(IROW)
               FACTOR = 1.0D00/(X(IROW)-X(K-1))
               POLYP(ILIROK) = (POLYP(ILDIAG)*DXIROW-POLYP(ILOTHR)*DXKMN1)*&
                  FACTOR
               POLYQ(ILIROK) = (POLYQ(ILDIAG)*DXIROW-POLYQ(ILOTHR)*DXKMN1)*&
                  FACTOR
            endif
         END DO
!
!   Check for convergence
!
         ILDIAG = ILOC(IROW,IROW)
         PESTT = POLYP(ILDIAG)
         QESTT = POLYQ(ILDIAG)
         if (PESTT==0.0D00 .OR. QESTT==0.0D00) then
            if (IROW < MXORD) then
               GO TO 4
            else
               PF(I,J) = PESTT
               QF(I,J) = QESTT
            endif
         else
            DPBP = ABS((PESTT - PESTL)/PESTT)
            DQBQ = ABS((QESTT - QESTL)/QESTT)
            if (DQBQ<ACCY .AND. DPBP<ACCY) then
               PF(I,J) = PESTT
               QF(I,J) = QESTT
            else
               PESTL = PESTT
               QESTL = QESTT
               if (IROW < MXORD) then
                  GO TO 4
               else
                  PF(I,J) = PESTT
                  QF(I,J) = QESTT
                  KOUNT = KOUNT + 1
               endif
            endif
         endif
!
      END DO
!
!   Ensure that all points of the array are defined by setting the
!   tail to zero
!
      MFJP1 = MFJ + 1
      PF(MFJP1:N,J) = 0.0D00
      QF(MFJP1:N,J) = 0.0D00
!
      if (LDBPR(3) .AND. KOUNT>0) WRITE (99, 301) ACCY, KOUNT, MFJ
!
!   Normalization
!
      DNORM = RINT(J,J,0)
      DNFAC = 1.0D00/SQRT(DNORM)
      PF(:MFJ,J) = PF(:MFJ,J)*DNFAC
      QF(:MFJ,J) = QF(:MFJ,J)*DNFAC
!
      return
!
  300 FORMAT(/,'INTRPQ: Grid of insufficient extent:'/,&
         ' Present grid has R(N) = ',1P,1D19.12,' Bohr radii'/,&
         '          Require R(N) = ',1D19.12,' Bohr radii')
  301 FORMAT(/,'INTRPQ: Interpolation procedure not converged to',1P,1D19.12,&
         ' for ',1I3,' of ',1I3,' tabulation points')
      return
      CONTAINS


      integer FUNCTION ILOC (IND1, IND2)
      integer, intent(in) :: IND1
      integer, intent(in) :: IND2
      ILOC = (IND1*(IND1 - 1))/2 + IND2
      return
      END FUNCTION ILOC
!
      end subroutine INTRPQ

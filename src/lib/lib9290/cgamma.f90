!***********************************************************************
!                                                                      *
      subroutine CGAMMA(ARGR, ARGI, RESR, RESI)
!                                                                      *
!   This subroutine returns in RES the complex Gamma function of the   *
!   complex argument ARG.  The suffixes R and I respectively distin-   *
!   guish the real and imaginary parts OF both RES and ARG.            *
!                                                                      *
!   Only RESR is nonzero if ARGI is zero.                              *
!                                                                      *
!   The  ARCTAN function required must return angles (in radians) in   *
!   the range  [0,2*\pi).                                              *
!                                                                      *
!   Subprogram(S) required: ARCTAN.                                    *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 06 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:48   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use DEF_C
      use arctan_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(kind=real64), intent(in) :: ARGI, ARGR
      real(kind=real64), intent(out) :: RESR, RESI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I
      real(kind=real64), dimension(7) :: FN, FD
      real(kind=real64) :: HLNTPI, TWOI, DifF, ARGUM, CLNGI, FACNEG, ARGUR, OVLFAC, &
         CLNGR, FAC, OBASQ, ARGUI, ARGUI2, OVLFR, OVLFI, TERMR, TERMI, ARGUR2, &
         OBASQR, OBASQI, ZFACR, ZFACI
      logical,SAVE :: FIRST, NEGARG
!----------------------------------------------------------------------*
!                                                                      *
!   These are the Bernoulli numbers B02, B04, ..., B14, expressed as   *
!   rational numbers. From abramowitz and stegun, p. 810.              *
!
      DATA FN/ 1.0D00, -1.0D00, 1.0D00, -1.0D00, 5.0D00, -691.0D00, 7.0D00/
      DATA FD/ 6.0D00, 30.0D00, 42.0D00, 30.0D00, 66.0D00, 2730.0D00, 6.0D00/
!
!----------------------------------------------------------------------*
!
      DATA HLNTPI/ 1.0D00/
!
      DATA FIRST/ .TRUE./
!
!   On the first entry to this routine, set up the constants required
!   for the reflection formula (cf. Abramowitz and Stegun 6.1.17) and
!   Stirling's approximation (cf. Abramowitz and Stegun 6.1.40).
!
      if (FIRST) then
!
         HLNTPI = 0.5D00*LOG(PI + PI)
!
         DO I = 1, 7
            FN(I) = FN(I)/FD(I)
            TWOI = DBLE(I + I)
            FN(I) = FN(I)/(TWOI*(TWOI - 1.0D00))
         END DO
!
         FIRST = .FALSE.
!
      endif
!
!   Cases where the argument is real
!
      if (ARGI == 0.0D00) then
!
!   Cases where the argument is real and negative
!
         if (ARGR <= 0.0D00) then
!
!   Stop with an error message if the argument is too near a pole
!
            DifF = ABS(DBLE(NINT(ARGR)) - ARGR)
            if (DifF <= PRECIS + PRECIS) then
               WRITE (*, 300) ARGR, ARGI
               STOP
            else
!
!   Otherwise use the reflection formula (Abramowitz and Stegun 6.1.17)
!   to ensure that the argument is suitable for Stirling's formula
!
               ARGUM = PI/(-ARGR*SIN(PI*ARGR))
               if (ARGUM < 0.0D00) then
                  ARGUM = -ARGUM
                  CLNGI = PI
               else
                  CLNGI = 0.0D00
               endif
               FACNEG = LOG(ARGUM)
               ARGUR = -ARGR
               NEGARG = .TRUE.
!
            endif
!
!   Cases where the argument is real and positive
!
         else
!
            CLNGI = 0.0D00
            ARGUR = ARGR
            NEGARG = .FALSE.
!
         endif
!
!   Use Abramowitz and Stegun formula 6.1.15 to ensure that
!   the argument in Stirling's formula is greater than 10
!
         OVLFAC = 1.0D00
    2    CONTINUE
         if (ARGUR < 10.0D00) then
            OVLFAC = OVLFAC*ARGUR
            ARGUR = ARGUR + 1.0D00
            GO TO 2
         endif
!
!   Now use Stirling's formula to compute Log (Gamma (ARGUM))
!
         CLNGR = (ARGUR - 0.5D00)*LOG(ARGUR) - ARGUR + HLNTPI
         FAC = ARGUR
         OBASQ = 1.0D00/(ARGUR*ARGUR)
         DO I = 1, 7
            FAC = FAC*OBASQ
            CLNGR = CLNGR + FN(I)*FAC
         END DO
!
!   Include the contributions from the recurrence and reflection
!   formulae
!
         CLNGR = CLNGR - LOG(OVLFAC)
         if (NEGARG) CLNGR = FACNEG - CLNGR
!
      else
!
!   Cases where the argument is complex
!
         ARGUR = ARGR
         ARGUI = ARGI
         ARGUI2 = ARGUI*ARGUI
!
!   Use the recurrence formula (Abramowitz and Stegun 6.1.15)
!   to ensure that the magnitude of the argument in Stirling's
!   formula is greater than 10
!
         OVLFR = 1.0D00
         OVLFI = 0.0D00
    4    CONTINUE
         ARGUM = SQRT(ARGUR*ARGUR + ARGUI2)
         if (ARGUM < 10.0D00) then
            TERMR = OVLFR*ARGUR - OVLFI*ARGUI
            TERMI = OVLFR*ARGUI + OVLFI*ARGUR
            OVLFR = TERMR
            OVLFI = TERMI
            ARGUR = ARGUR + 1.0D00
            GO TO 4
         endif
!
!   Now use Stirling's formula to compute Log (Gamma (ARGUM))
!
         ARGUR2 = ARGUR*ARGUR
         TERMR = 0.5D00*LOG(ARGUR2 + ARGUI2)
         TERMI = ARCTAN(ARGUI,ARGUR)
         CLNGR = (ARGUR - 0.5D00)*TERMR - ARGUI*TERMI - ARGUR + HLNTPI
         CLNGI = (ARGUR - 0.5D00)*TERMI + ARGUI*TERMR - ARGUI
         FAC = (ARGUR2 + ARGUI2)**(-2)
         OBASQR = (ARGUR2 - ARGUI2)*FAC
         OBASQI = -2.0D00*ARGUR*ARGUI*FAC
         ZFACR = ARGUR
         ZFACI = ARGUI
         DO I = 1, 7
            TERMR = ZFACR*OBASQR - ZFACI*OBASQI
            TERMI = ZFACR*OBASQI + ZFACI*OBASQR
            FAC = FN(I)
            CLNGR = CLNGR + TERMR*FAC
            CLNGI = CLNGI + TERMI*FAC
            ZFACR = TERMR
            ZFACI = TERMI
         END DO
!
!   Add in the relevant pieces from the recurrence formula
!
         CLNGR = CLNGR - 0.5D00*LOG(OVLFR*OVLFR + OVLFI*OVLFI)
         CLNGI = CLNGI - ARCTAN(OVLFI,OVLFR)
!
      endif
!
!   Now exponentiate the complex Log Gamma function to get
!   the complex Gamma function
!
      if (CLNGR<=EXPMAX .AND. CLNGR>=EXPMIN) then
         FAC = EXP(CLNGR)
      else
         WRITE (*, 301) CLNGR
         STOP
      endif
      RESR = FAC*COS(CLNGI)
      RESI = FAC*SIN(CLNGI)
!
      return
!
  300 FORMAT('CGAMMA: Argument (',1P,1D19.12,',',1D19.12,')',&
         ' too close to a pole.')
  301 FORMAT('CGAMMA: Argument to exponential function',' (',1P,1D19.12,&
         ') out of range.')
      return
!
      end subroutine CGAMMA

!***********************************************************************
!                                                                      *
      subroutine INTERP(XARR, YARR, NARR, XVAL, YVAL, ACCY)
!                                                                      *
!   This routine returns  YVAL  given a value  XVAL by interpolating   *
!   using a pair of arrays XARR(1:NARR), YARR(1:NARR), that tabulate   *
!   a  function.  ACCY  is the  desired  accuracy of the estimate: a   *
!   warning message is issued if this is not achieved.  A warning is   *
!   also issued when the routine is extrapolating.  Aitken's algori-   *
!   thm is used. See, for  instance, F B Hildebrand, Introduction to   *
!   Numerical Analysis,  2nd ed., McGraw-Hill, New York, NY, 1974.     *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 06 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:32   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: NARR
      real(real64), intent(in) :: XVAL
      real(real64), intent(out) :: YVAL
      real(real64), intent(in) :: ACCY
      real(real64), dimension(NARR), intent(in) :: XARR
      real(real64), dimension(NARR), intent(in) :: YARR
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: MXORD = 11
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: NRSTLO, NRSTHI, K, LLO, LHI, LLR, IROW, LOCNXT, ILIROK, ILDIAG&
         , ILOTHR, IBEST
      real(real64), dimension(MXORD) :: DX, X, EST
      real(real64), dimension((MXORD*(MXORD + 1))/2) :: POLY
      real(real64) :: DifF, DifFT, DEBEB, DEBE
      logical :: SET
      logical, dimension(2*MXORD + 2) :: useD
!-----------------------------------------------
!
!
!   MXORD is the maximum order of the interpolation
!
!
!   This function dispenses with the need for a two-dimensional
!   array for the Aitken lower triangle matrix
!
!
!   Determine the nearest two XARR entries bounding XVAL
!
      if (XVAL < XARR(1)) then
         NRSTLO = 1
         NRSTHI = 1
         WRITE (*, 300)
      else if (XVAL > XARR(NARR)) then
         NRSTLO = NARR
         NRSTHI = NARR
         WRITE (*, 300)
      else
         K = 0
    1    CONTINUE
         K = K + 1
         if (XARR(K) < XVAL) then
            NRSTLO = K
            GO TO 1
         else
            NRSTHI = K
         endif
      endif
!
!   Clear relevant piece of use-indicator array
!
      LLO = MAX(NRSTLO - MXORD,1)
      LHI = MIN(NRSTHI + MXORD,NARR)
      LLR = LLO - 1
      useD(LLO-LLR:LHI-LLR) = .FALSE.
!
!   Determine next-nearest XARR entry
!
      DO IROW = 1, MXORD
         LLO = MAX(NRSTLO - IROW + 1,1)
         LHI = MIN(NRSTHI + IROW - 1,NARR)
         SET = .FALSE.
         DO K = LLO, LHI
            if (useD(K-LLR)) CYCLE
            if (.NOT.SET) then
               DifF = XARR(K) - XVAL
               LOCNXT = K
               SET = .TRUE.
            else
               DifFT = XARR(K) - XVAL
               if (ABS(DifFT) < ABS(DifF)) then
                  DifF = DifFT
                  LOCNXT = K
               endif
            endif
         END DO
         useD(LOCNXT-LLR) = .TRUE.
         X(IROW) = XARR(LOCNXT)
         DX(IROW) = DifF
!
!   Fill table for this row
!
         DO K = 1, IROW
            ILIROK = ILOC(IROW,K)
            if (K == 1) then
               POLY(ILIROK) = YARR(LOCNXT)
            else
               ILDIAG = ILOC(K - 1,K - 1)
               ILOTHR = ILOC(IROW,K - 1)
               POLY(ILIROK) = (POLY(ILDIAG)*DX(IROW)-POLY(ILOTHR)*DX(K-1))/(X(&
                  IROW)-X(K-1))
            endif
         END DO
!
!   Pick off the diagonal element
!
         ILDIAG = ILOC(IROW,IROW)
         EST(IROW) = POLY(ILDIAG)
!
      END DO
!
!   Now the estimate vector is filled in, so obtain the
!   best estimate
!
      DEBEB = ABS((EST(2)-EST(1))/EST(2))

      IBEST = 2
      DO IROW = 3, MXORD
         DEBE = ABS((EST(IROW)-EST(IROW-1))/EST(IROW))

!ps 13/12/2017
!ps If NARR is small, the EST array sometimes contains the NaN values.
!ps In a consequence, DEBE becomes NaN, and in that case the condition
!ps in the CYCLE statement (NaN >= number) is then evaluated to "false".
!ps So, the CYCLE is not performed, what is incorrect.
!ps To avoid this, the CYCLE was replaced with the if / then / END if
!ps (the condition was reversed).
        !ps if (DEBE >= DEBEB) CYCLE
        !ps DEBEB = DEBE
        !ps IBEST = IROW
        if (DEBE < DEBEB) then
            DEBEB = DEBE
            IBEST = IROW
        END if

      END DO
      YVAL = EST(IBEST)
!
      if (DEBEB > ACCY) WRITE (*, 301) DEBEB, ACCY
!
      return
!
  300 FORMAT('INTERP: Extrapolating, not interpolating.')
  301 FORMAT('INTERP: Accuracy of interpolation (',1P,1D10.3,') is',&
         ' below input criterion (',1D10.3,').')
      return
      CONTAINS


      integer FUNCTION ILOC (IND1, IND2)
      integer, intent(in) :: IND1
      integer, intent(in) :: IND2
      ILOC = (IND1*(IND1 - 1))/2 + IND2
      return
      END FUNCTION ILOC
!
      end subroutine INTERP

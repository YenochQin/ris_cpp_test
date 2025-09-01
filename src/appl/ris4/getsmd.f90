!***********************************************************************
!                                                                      *
      subroutine GETSMD(NAME)
!                                                                      *
!   Interactively determines the data governing the SMS problem.       *
!                                                                      *
!   Call(s) to: [LIB92]: NUCPOT, RADGRD, SETQIC.                       *
!               [RCI92]: SETISO, SETRWF.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 15 Dec 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:07:11   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  11/02/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNP
      use decide_C
      use def_C
      use default_C
      use foparm_C
      use grid_C
      use npar_C
      use npot_C
      use orb_C
      use wave_C
      use wfac_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use getyn_I
      use setiso_I
      use setqic_I
      use radgrd_I
      use nucpot_I
      use setrwfa_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character, intent(in) :: NAME*24
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      logical :: YES
!-----------------------------------------------
!
!
!   Open, check, load data from, and close the  .iso  file
!
      CALL SETISO ('isodata')
!
!   Determine the physical effects specifications
!

      if (NDEF /= 0) then
         WRITE (6, *) 'The physical speed of light in'
         WRITE (6, *) ' atomic units is', CVAC, ';'
         WRITE (6, *) ' revise this value?'
         YES = GETYN()
         if (YES) then
            WRITE (6, *) 'Enter the revised value:'
            READ (5, *) C
         else
            C = CVAC
         endif
      else
         C = CVAC
      endif
!
      if (NDEF /= 0) then
!
         WRITE (6, *) 'Treat contributions of some CSFs'
         WRITE (6, *) ' as first-order perturbations?'
         YES = GETYN()
         if (YES) then
            LFORDR = .TRUE.
            WRITE (6, *) 'The contribution of CSFs'
            WRITE (6, *) ' 1 -- ICCUT will be treated'
            WRITE (6, *) ' variationally; the remainder'
            WRITE (6, *) ' perturbatively; enter ICCUT:'
            READ (5, *) ICCUT
         else
            LFORDR = .FALSE.
            ICCUT = 0
         endif
      else
         LFORDR = .FALSE.
         ICCUT = 0
      endif
!
!   Determine the parameters controlling the radial grid
!
      if (NPARM == 0) then
         RNT = EXP((-65.0D00/16.0D00))/Z
         H = 0.5D00**4
         N = MIN(220,NNNP)
      else
!CFF     .. should be Z-dependent
         RNT = 2.0D-06/Z
         H = 5.0D-02
         N = NNNP
      endif
      HP = 0.0D00
      if (NDEF /= 0) then
         WRITE (6, *) 'The default radial grid parameters'
         WRITE (6, *) ' for this case are:'
         WRITE (6, *) ' RNT = ', RNT, ';'
         WRITE (6, *) ' H = ', H, ';'
         WRITE (6, *) ' HP = ', HP, ';'
         WRITE (6, *) ' N = ', N, ';'
         WRITE (6, *) ' revise these values?'
         YES = GETYN()
         if (YES) then
            WRITE (6, *) 'Enter RNT:'
            READ (5, *) RNT
            WRITE (6, *) 'Enter H:'
            READ (5, *) H
            WRITE (6, *) 'Enter HP:'
            READ (5, *) HP
            WRITE (6, *) 'Enter N:'
            READ (5, *) N
         endif
      endif
!
!   ACCY is an estimate of the accuracy of the numerical procedures
!
      ACCY = H**6
!
!   Set up the coefficients for the numerical procedures
!
      CALL SETQIC
!
!   Generate the radial grid and all associated arrays
!
      CALL RADGRD
!
!   Generate $- r \times V_nuc (r)$
!
      CALL NUCPOT
!
!   Load the radial wavefunctions
!
!      CALL SETRWFA(NAME)
      CALL SETRWFA (TRIM(NAME)//'.w')
!
      return
      end subroutine GETSMD

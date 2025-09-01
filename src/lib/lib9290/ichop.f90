!***********************************************************************
!                                                                      *
      integer FUNCTION ICHOP (ISUBSH, ICSF)
!                                                                      *
!   ICHOP is -1 if subshell ISUBSH is empty in CSF  ICSF,  +1 if the   *
!   subshell is full, and 0 if it is open.                             *
!                                                                      *
!   Call(s) to: [LIB92]: IQ.                                           *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 30 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:25   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use parameter_def, only: NNNW
      use ORB_C,         only: NKL, NKJ
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use iq_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer :: ISUBSH
      integer :: ICSF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IOCC, ifULL
!-----------------------------------------------
!
!
! cff  Since ICHOP is always called from within a do-loop over the
!      appropriate range, testing seems redundant
!     if ((ISUBSH .GE. 1) .AND. (ISUBSH .LE. NW)) then
!        if ((ICSF .GE. 1) .AND. (ICSF .LE. NCF)) then
      IOCC = IQ(ISUBSH,ICSF)
      ifULL = NKJ(ISUBSH) + 1
      if (IOCC == 0) then
         ICHOP = -1
      else if (IOCC == ifULL) then
         ICHOP = 1
      else
         ICHOP = 0
      endif
!        else
!           PRINT *, 'ICHOP: Argument ICSF is out of range.'
!           STOP
!        endif
!     else
!        PRINT *, 'ICHOP: Argument ISUBSH is out of range.'
!        STOP
!     endif
!
      return
      END FUNCTION ICHOP

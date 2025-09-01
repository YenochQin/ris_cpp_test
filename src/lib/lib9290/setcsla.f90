!***********************************************************************
!                                                                      *
      subroutine SETCSLA(NAME, NCORE)
!                                                                      *
!   Open, check, load data from and close the  .csl  file. This file   *
!   is always attached to stream 21.                                   *
!                                                                      *
!   Call(s) to: [RCI92]: LODCSL, OPENFL.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 23 Dec 1992   *
!   Modified by G. Gaigalas,                                May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:33   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use IOUNIT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use openfl_I
      use lodcsl_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: NCORE
      character (LEN = 24), intent(in) :: NAME
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer   :: K, IERR, IOS
      character(LEN=3)   :: STATUS
      character(LEN=9)   :: FORM
      character(LEN=15)  :: RECORD
      character(LEN=256) :: FILNAM
!-----------------------------------------------
!
!   The  .csl  file is FORMATTED; it must exist
!
      K = INDEX(NAME,' ')
      FILNAM = NAME(1:K-1)//'.c'
      FORM = 'FORMATTED'
      STATUS = 'OLD'

!
      CALL OPENFL (21, FILNAM, FORM, STATUS, IERR)
      if (IERR == 1) then
         WRITE (ISTDE, *) 'Error when opening', FILNAM
         STOP
      endif
!
!   Check the first record of the file; if not as expected, try again
!
      READ (21, '(1A15)', IOSTAT=IOS) RECORD
      if (IOS/=0 .OR. RECORD(1:15)/='Core subshells:') then
         WRITE (ISTDE, *) 'Not a Configuration Symmetry List File;'
         CLOSE(21)
      endif
!
!   Load data from the  .csl  file
!
      CALL LODCSL (NCORE)
!
!   Close the  .csl  file
!
      CLOSE(21)
!
      return
      end subroutine SETCSLA

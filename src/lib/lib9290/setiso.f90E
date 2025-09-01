!***********************************************************************
!                                                                      *
      subroutine SETISO(FNAME)
!                                                                      *
!   Open, check, load data from and close the  .iso  file. This file   *
!   is always attached to stream  22 in  RSCF92,  RCI92,  HFS92, and   *
!   OSCL92.                                                            *
!   Filename - fname is moved to the argument list. This subroutine
!   is ready to replace the one in lib/lib92, but need to check
!   other application programs before doing do.
!                                                                      *
!   Call(s) to: [LIB92]: LODISO, OPENFL.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 06 Oct 1992   *
!   Modified    by Xinghong He            Last revision:  1 Jun 1998   *
!   Modified by G. Gaigalas,                                May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:35   2/14/04
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
      use lodiso_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character (LEN = *), intent(in) :: FNAME
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      character(LEN=3), parameter  :: STATUS = 'OLD'
      character(LEN=6), parameter  :: MYNAME = 'SETISO'
      character(LEN=9), parameter  :: FORM = 'FORMATTED'
      character(LEN=14), parameter :: SIGNATURE = 'Atomic number:'
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer           :: LENF, IERR, IOS
      logical           :: FOUND
      character(LEN=14) :: STR
!-----------------------------------------------
!
      INQUIRE(FILE=FNAME, EXIST=FOUND)
      if (.NOT.FOUND) then
         LENF = LEN_TRIM(FNAME)
         WRITE (ISTDE,*)                                              &
                     MYNAME,'- file: ',FNAME(1:LENF),' does not exist'
         STOP
      endif
!
      CALL OPENFL (22, FNAME, FORM, STATUS, IERR)
      if (IERR /= 0) then
         WRITE (ISTDE, *) 'Error opening isodata file: ', FNAME(1:LENF)
         STOP
      endif
!
!   Check the first record of the file; if not as expected, try again
!
      READ (22, '(A)', IOSTAT=IOS) STR
      if (IOS/=0 .OR. STR/=SIGNATURE) then
         WRITE (ISTDE, *) 'Not an ISOtope Data File;'
         CLOSE(22)
         STOP
      endif
!
!   Load data from the .iso file and then close it.
!
      CALL LODISO
      CLOSE(22)

      return
      end subroutine SETISO

!***********************************************************************
!
      subroutine SETCSH(NFILE, NAME, NCORE)
!
!   Open, check the CSL file and load the load (via lodcsh) data from
!   the header lines. It is designed to replace all kinds of "setcsl"
!   routines within GRASP packages.
!
!   Routines called: lodcsh
!
!   ncore is output parameter
!
!   Written by Xinghone He                Last revision: 23 Dec 1997
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:30   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use IOUNIT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lodcsh_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: NFILE
      integer  :: NCORE
      character (LEN = *), intent(in) :: NAME
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: LENGTH, IOS
      logical :: FOUND
      character :: RECORD*15
!-----------------------------------------------
!
!     ...Locals

      LENGTH = LEN_TRIM(NAME)

      INQUIRE(FILE=NAME, EXIST=FOUND)
      if (.NOT.FOUND) then
         WRITE (ISTDE, *) NAME(1:LENGTH), ' does not exist'
         STOP
      endif

      INQUIRE(UNIT=NFILE, OPENED=FOUND)
      if (FOUND) then
         WRITE (ISTDE, *) 'Unit ', NFILE, ' has been used elsewhere'
         STOP
      endif

      OPEN(NFILE, FILE=NAME, STATUS='OLD', IOSTAT=IOS, POSITION='asis')
      if (IOS /= 0) then
         WRITE (ISTDE, *) 'Error when opening ', NAME
         STOP
      endif
!
!   Check the first record of the file; if not as expected, try again
!
      READ (NFILE, '(1A15)', IOSTAT=IOS) RECORD

      if (IOS/=0 .OR. RECORD(1:15)/='Core subshells:') then
         WRITE (ISTDE, *) 'Not a Configuration Symmetry List File;'
         CLOSE(NFILE)
         STOP
      endif
!
!   Load data from the  .csl  file
!
      CALL LODCSH (NFILE, NCORE)

      return
      end subroutine SETCSH

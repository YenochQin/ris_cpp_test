!***********************************************************************
      subroutine SETCSLL(NUNIT, NAME, NBLKIN, NBLOCK, NCFBLK, NCFTOT, IDBLK)
!
!  Open, read name file to get nblock, ncfblk(), idblk(), ncftot
!
! Xinghong He 98-06-29
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:34   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use openfl_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: NUNIT
      integer, intent(in) :: NBLKIN
      integer, intent(out) :: NBLOCK
      integer, intent(out) :: NCFTOT
      character (LEN = *), INTENT(INOUT) :: NAME
      integer, dimension(*), INTENT(INOUT) :: NCFBLK
      character (LEN = 8), dimension(*), intent(out) :: IDBLK
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I, NCSF, IOS, IERR
      logical :: FOUND
      character :: STR*15, CH*2, LINE3*200
!-----------------------------------------------
! Locals

! Look for  <name>

      INQUIRE(FILE=NAME, EXIST=FOUND)
      if (.NOT.FOUND) then
         WRITE (6, *) NAME(1:LEN_TRIM(NAME)), ' does not exist'
         STOP
      endif

! Open it

      CALL OPENFL (NUNIT, NAME, 'FORMATTED', 'OLD', IERR)
      if (IERR == 1) then
         WRITE (6, *) 'Error when opening ', NAME(1:LEN_TRIM(NAME))
         STOP
      endif

! Check the first record of the file; if not as expected, stop

      READ (NUNIT, '(1A15)', IOSTAT=IOS) STR
      if (IOS/=0 .OR. STR/='Core subshells:') then
         WRITE (6, *) 'Not a Configuration Symmetry List File;'
         CLOSE(NUNIT)
         STOP
      endif

! Skip next 4 records

      DO I = 1, 4
         READ (NUNIT, *)
      END DO

! Determine the number of blocks in this file

      NBLOCK = 0
      NCSF = 0

      IOS = 0
      DO WHILE(IOS == 0)
         READ (NUNIT, '(1A2)', IOSTAT=IOS) CH
         if (CH==' *' .OR. IOS/=0) then
            !.. a new block has been found
            NBLOCK = NBLOCK + 1
            WRITE (6, *) 'Block ', NBLOCK, ',  ncf = ', NCSF
            if (NBLOCK > NBLKIN) then
               WRITE (6, *) 'setcsll: Too many blocks(', NBLOCK, ')'
               WRITE (6, *) 'Maximum allowed is ', NBLKIN
               STOP
            endif
            I = LEN_TRIM(LINE3)
            IDBLK(NBLOCK) = LINE3(I-4:I)
            NCFBLK(NBLOCK) = NCSF
            NCSF = 0
            if (IOS == 0) CYCLE
         else
            READ (NUNIT, *)
            READ (NUNIT, '(A)') LINE3
            NCSF = NCSF + 1
         endif
      END DO

! Obtain ncftot

      NCFTOT = SUM(NCFBLK(:NBLOCK))

      return
      end subroutine SETCSLL

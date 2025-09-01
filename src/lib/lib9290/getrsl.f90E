!***********************************************************************
!                                                                      *
      subroutine GETRSL(INDX, NSUBS)
!                                                                      *
!   READs and parses a list of relativistic subshell labels delimit-   *
!   ed either by blanks or by commas. An asterisk may be used as the   *
!   `wildcard' character.                                              *
!
!   Output:
!     NSUBS - the # of orbitals parsed
!     indx(1:NSUBS) - indeces of these orbitals
!                                                                      *
!   Call(s) to: [LIB92]: LDIGIT.                                       *
!                                                                      *
!   Written by Farid A. Parpia             Last revised: 18 Dec 1992   *
!   Modified by Xinghong He                Last revised: 09 Jul 1998   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:14   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use ORB_C
      use IOUNIT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ldigit_I
      use convrt_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(out) :: NSUBS
      integer, dimension(*), INTENT(INOUT) :: INDX
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ISTART, I, IEND, NFORM, J, LENTH, IOS, NPQ, K
      logical :: FOUND, NLANY, NLOK, NPANY, NPOK, NSANY, NSOK
      character :: NLQ, NSQ, RECI, CNUM*2, FORM*5, RECORD*500
!-----------------------------------------------
!
!

      GO TO 2

    1 CONTINUE
      WRITE (ISTDE, *) ' redo ...'
!
!   Read a record
!
    2 CONTINUE
      NSUBS = 0
      READ (*, '(A)') RECORD

      WRITE(734,'(a)') trim(record)
!
!   Parse RECORD from left to right
!
      ISTART = 0
      I = 1
    3 CONTINUE
      RECI = RECORD(I:I)
      if (RECI/=' ' .AND. RECI/=',') then
         if (ISTART == 0) ISTART = I
      else
         if (ISTART /= 0) then
            IEND = I - 1
!
!   Parse the substring from left to right
!
!   (1) Determine the principal quantum number
!
            if (RECORD(ISTART:ISTART) == '*') then
               NPANY = .TRUE.
               ISTART = MIN(ISTART + 1,IEND)
            else
               NPANY = .FALSE.
               NFORM = 0
               DO J = ISTART, IEND
                  if (.NOT.LDIGIT(RECORD(J:J))) CYCLE
                  NFORM = NFORM + 1
               END DO
               if (NFORM == 0) then
                  WRITE (ISTDE, *) 'GETRSL: Unable to interpret ', &
                     'the principal quantum number;'
                  GO TO 1
               endif
               CALL CONVRT (NFORM, CNUM, LENTH)
               FORM = '(1I'//CNUM(1:LENTH)//')'
               READ (RECORD(ISTART:ISTART+NFORM-1), FORM, IOSTAT=IOS) NPQ
               if (IOS /= 0) then
                  WRITE (ISTDE, *) 'GETRSL: Unable to interpret ', 'string ', &
                     RECORD(ISTART:IEND-2), ' as a principal quantum number'
                  GO TO 1
               endif
               ISTART = ISTART + NFORM
            endif
!
!   (2) Determine the orbital angular momentum quantum number
!
            NLQ = RECORD(ISTART:ISTART)
            if (NLQ == '*') then
               NLANY = .TRUE.
            else
               NLANY = .FALSE.
            endif
!
!   (3) Determine the spin-orbit component
!
            if (IEND > ISTART) then
               NSQ = RECORD(IEND:IEND)
               if (NSQ == '*') then
                  NSANY = .TRUE.
               else if (NSQ == '-') then
                  NSANY = .FALSE.
               else
                  WRITE (ISTDE, *) 'GETRSL: Unable to interpret ', 'string ', &
                     NSQ, ' as a spin-orbit component indicator'
                  GO TO 1
               endif
            else
               if (NLANY) then
                  NSANY = .TRUE.
               else
                  NSANY = .FALSE.
                  NSQ = ' '
               endif
            endif
!
!
            FOUND = .FALSE.
            DO J = 1, NW
               NPOK = NPANY .OR. NP(J)==NPQ
               NLOK = NLANY .OR. NLQ==NH(J)(1:1)
               NSOK = NSANY .OR. NSQ==NH(J)(2:2)
               if (.NOT.(NPOK .AND. NLOK .AND. NSOK)) CYCLE
               DO K = 1, NSUBS
                  if (INDX(K) /= J) CYCLE
                  WRITE (ISTDE, *) 'GETRSL: ', 'Repeated subshell in list;'
                  GO TO 1
               END DO
               FOUND = .TRUE.
               NSUBS = NSUBS + 1
               INDX(NSUBS) = J
            END DO
!
            if (.NOT.FOUND) then
               WRITE (ISTDE, *) 'GETRSL: Subshell not occupied as ', &
                  ' according to CSL  File;'
               GO TO 1
            endif
!
            ISTART = 0
         endif
      endif
!
      if (I < 500) then
         I = I + 1
         GO TO 3
      endif
!
      return
      end subroutine GETRSL

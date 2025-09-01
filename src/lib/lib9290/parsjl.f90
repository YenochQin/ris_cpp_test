!***********************************************************************
!                                                                      *
      subroutine PARSJL(MODE, NCORE, RECORD, LOC, JX, NJX, IERR)
!                                                                      *
!   READs and  parses a string that specifies angular momentum quan-   *
!   tum numbers.                                                       *
!                                                                      *
!   Call(s) to: [LIB92] CONVRT.                                        *
!                                                                      *
!   Written by Farid A Parpia              Last revised: 21 Dec 1992   *
!   Modified by G. Gaigalas,                                May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:09   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use convrt_I
      use ORB_C, only: NCF, NW, IQA
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)           :: MODE, NCORE, LOC
      integer, intent(out)          :: NJX, IERR
      character(LEN=256), intent(in) :: RECORD
      integer, dimension(*), intent(out) :: JX
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: NJXMAX, ISTART, I, ILOOP, ifRAC, IEND, LTEGER, J
      character(LEN=1) :: RECI
      character(LEN=2) :: CTEGER
      character(LEN=6) :: FORM
!-----------------------------------------------
!
! This subroutine is here to replace the existing one which has been
! renamed as PARSJL_OLD. The purpose is to remove illegal GOTO into an
! if block. The strategy is simple: copy the kernl to the bottom
! and add a do-loop over I.
! To restore (re-use) this subroutine, give the existing PARSJL a new
! name and then change the name of this subroutine back to PARSJL, and
! compile.
! XHH 1997.01.28
!
!
!   There cannot be more than JXMAX angular momenta specified; if
!   MODE is 1, the subshell quantum numbers are being read; if MODE
!   is 2, the intermediate and final angular momenta are being read
!
      if (MODE == 1) then
         NJXMAX = 2*(NW - NCORE)
      else
         NJXMAX = NW - NCORE
      endif
!
!   Initialise NJX
!
      NJX = 0
!
!   Parse RECORD from left to right
!
      ISTART = 0
      I = 1

! The original algorithm goes through the whole subroutine at least
! once, whatever the value of LOC. Thus we define another integer
! iloop to achieve this.
! XHH 1997.01.28

      ILOOP = MAX(1,LOC)
      DO I = 1, ILOOP
         RECI = RECORD(I:I)
         if (RECI/=' ' .AND. RECI/=',' .AND. RECI/=';') then
            if (ISTART == 0) then
               ISTART = I
               ifRAC = 0
            else
               if (RECI == '/') then
                 ifRAC = I
               endif
            endif
         else
            if (ISTART /= 0) then
!XHH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               NJX = NJX + 1
               if (NJX > NJXMAX) then
                  WRITE (6, *) 'PARSJL: Too many angular momentum', &
                     ' quantum numbers specified;'
                  IERR = 1
                  GO TO 3
               endif
               IEND = I - 1
               if (ifRAC == 0) then
                  CALL CONVRT (IEND - ISTART + 1, CTEGER, LTEGER)
                  FORM = '(1I'//CTEGER(1:LTEGER)//')'
                  READ (RECORD(ISTART:IEND), FMT=FORM) J
                  if (J < 0) then
                     WRITE (6, *) 'PARSJL: Negative angular momentum', &
                        ' quantum number found;'
                     IERR = 2
                     GO TO 3
                  endif
                  JX(NJX) = 2*J
               else
                  CALL CONVRT (IEND - ifRAC, CTEGER, LTEGER)
                  FORM = '(1I'//CTEGER(1:LTEGER)//')'
                  READ (RECORD(ifRAC+1:IEND), FMT=FORM) J
                  if (J /= 2) then
                     WRITE (6, *) 'PARSJL: The denominator of a', &
                        ' fractional quantum number must be 2;'
                     IERR = 3
                     GO TO 3
                  endif
                  CALL CONVRT (ifRAC - ISTART, CTEGER, LTEGER)
                  FORM = '(1I'//CTEGER(1:LTEGER)//')'
                  READ (RECORD(ISTART:ifRAC-1), FMT=FORM) J
                  if (J < 0) then
                     WRITE (6, *) 'PARSJL: Negative angular momentum', &
                        ' quantum number found;'
                     IERR = 4
                     GO TO 3
                  endif
                  JX(NJX) = J
               endif
               ISTART = 0
!XHH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            endif
         endif
      END DO
!
      if (LOC >= 1) then

! The following was accessed one extra time only when 1 <= I = LOC+1 .
! After the do-loop above, the value of I would be either LOC+1 or
! 2, depending on if LOC is greater than 1 or not, respectively


!XHH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! The following is exactly the same as those above
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         NJX = NJX + 1
         if (NJX > NJXMAX) then
            WRITE (6, *) 'PARSJL: Too many angular momentum', &
               ' quantum numbers specified;'
            IERR = 1
            GO TO 3
         endif
         IEND = I - 1
         if (ifRAC == 0) then
            CALL CONVRT (IEND - ISTART + 1, CTEGER, LTEGER)
            FORM = '(1I'//CTEGER(1:LTEGER)//')'
            READ (RECORD(ISTART:IEND), FMT=FORM) J
            if (J < 0) then
               WRITE (6, *) 'PARSJL: Negative angular momentum', &
                  ' quantum number found;'
               IERR = 2
               GO TO 3
            endif
            JX(NJX) = 2*J
         else
            CALL CONVRT (IEND - ifRAC, CTEGER, LTEGER)
            FORM = '(1I'//CTEGER(1:LTEGER)//')'
            READ (RECORD(ifRAC+1:IEND), FMT=FORM) J
            if (J /= 2) then
               WRITE (6, *) 'PARSJL: The denominator of a', &
                  ' fractional quantum number must be 2;'
               IERR = 3
               GO TO 3
            endif
            CALL CONVRT (ifRAC - ISTART, CTEGER, LTEGER)
            FORM = '(1I'//CTEGER(1:LTEGER)//')'
            READ (RECORD(ISTART:ifRAC-1), FMT=FORM) J
            if (J < 0) then
               WRITE (6, *) 'PARSJL: Negative angular momentum', &
                  ' quantum number found;'
               IERR = 4
               GO TO 3
            endif
            JX(NJX) = J
         endif
         ISTART = 0
      endif
!XHH~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IERR = 0
!
    3 CONTINUE
      return
      end subroutine PARSJL

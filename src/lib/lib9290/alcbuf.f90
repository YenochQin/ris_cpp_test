!***********************************************************************
!                                                                      *
      subroutine ALCBUF(MODE)
!                                                                      *
!   The arrays in  module BUFFER_C are allocated (MODE = 1), realloca-   *
!   ted (MODE = 2), and deallocated (MODE = 3) in this routine.        *
!                                                                      *
!   Call(s) to: [LIB92]: ALLOC, DALLOC, RALLOC.                        *
!                                                                      *
!   Written by Farid A. Parpia.             Last update: 06 Oct 1992   *
!   Editted after translation by C. Froese Fischer       29 Apr 2007   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  16:02:30   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!====================================================================

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use memory_man
      use buffer_C,       only: LABEL, COEFF, NBDIM
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: MODE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: NEWSIZ, IERR
!
!
      SELECT CASE (MODE)
      CASE (1)
!
!   Allocation
!
         NBDIM = 10
         CALL ALLOC (LABEL, 6,NBDIM, 'LABEL', 'ALCBUF')
         CALL ALLOC (COEFF, NBDIM, 'COEFF', 'ALCBUF')
!
      CASE (2)
!
!   Reallocation to double storage
!
         NEWSIZ = NBDIM + NBDIM
         CALL RALLOC(LABEL,6,NEWSIZ, 'LABEL', 'ALCBUF')
         CALL RALLOC(COEFF,NEWSIZ, 'COEFF', 'ALCBUF')
         NBDIM = NEWSIZ
!
      CASE (3)
!
!   Deallocation
!
          CALL DALLOC(label, 'LABEL', 'ALCBUF')
          CALL DALLOC(coeff, 'COEFF', 'ALCBUF')
!
      CASE DEFAULT
!
!   Argument error
!
         WRITE (6, *) 'ALCBUF: Invalid argument MODE = ', MODE
         STOP
!
      END SELECT
!
      return
!
      end subroutine ALCBUF

!***********************************************************************
      subroutine POSFILE(MODE, NUNIT, NREC)
!    Position the file
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:12   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: MODE
      integer, intent(in) :: NUNIT
      integer, intent(in) :: NREC
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: FORWD = 0
      integer, parameter :: BACKWD = 1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I
!-----------------------------------------------
!

      SELECT CASE (MODE)

      CASE (FORWD)
         REWIND (NUNIT)
         DO I = 1, NREC
            READ (NUNIT)
         END DO

      CASE (BACKWD)
         DO I = 1, NREC
            BACKSPACE (NUNIT)
         END DO

      END SELECT

      return
      end subroutine POSFILE

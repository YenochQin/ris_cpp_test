      subroutine DINIT(N, A, X, INCX)
!     ==================================================================
!
!     PURPOSE ... INITIALIZES real(real64)           VECTOR TO
!                 A CONSTANT VALUE 'A'
!
!     CREATED ... APR. 14, 1987
!
!     ==================================================================
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:47:18   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: N
      integer, intent(in) :: INCX
      real(real64), intent(in) :: A
      real(real64), dimension(*), intent(out) :: X
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: XADDR, I
!-----------------------------------------------
!
      if (INCX == 1) then
!
!         ----------------------------------
!         ... UNIT INCREMENT (STANDARD CASE)
!         ----------------------------------
!
         X(:N) = A
!
      else
!
!         ----------------------
!         ... NON-UNIT INCREMENT
!         ----------------------
!
         XADDR = 1
         if (INCX < 0) XADDR = ((-N) + 1)*INCX + 1
!
         X(XADDR:(N-1)*INCX+XADDR:INCX) = A
!
      endif
!
      return
!
      end subroutine DINIT

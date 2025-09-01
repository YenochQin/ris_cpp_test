      module iniest_I
      interface
!
      subroutine INIEST(N, NB, NIV, HMX, JCOL, IROW, BASIS, IBLOCK, JBLOCK)
!************************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in) :: N
      integer, intent(in) :: NB
      integer  :: NIV
      integer, intent(in) :: JBLOCK
      integer, intent(in) :: JCOL(0:*)
      integer, intent(in) :: IROW(*)
      integer, intent(in) :: IBLOCK(*)
      real(kind=real64), intent(in) :: HMX(*)
      real(kind=real64)  :: BASIS(*)
      end subroutine
      end interface
      end module

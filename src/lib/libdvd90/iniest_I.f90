      MODULE iniest_I
      INTERFACE
!
      SUBROUTINE INIEST(N, NB, NIV, HMX, JCOL, IROW, BASIS, IBLOCK, JBLOCK)
!************************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: N
      INTEGER, INTENT(IN) :: NB
      INTEGER  :: NIV
      INTEGER, INTENT(IN) :: JBLOCK
      INTEGER, INTENT(IN) :: JCOL(0:*)
      INTEGER, INTENT(IN) :: IROW(*)
      INTEGER, INTENT(IN) :: IBLOCK(*)
      real(real64), INTENT(IN) :: HMX(*)
      real(real64)  :: BASIS(*)
      END SUBROUTINE
      END INTERFACE
      END MODULE

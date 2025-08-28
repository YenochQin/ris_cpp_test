!***********************************************************************
!                                                                      *
      SUBROUTINE CXK (S,IS,KAPS,NU,K,IBR,IEX)
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      IMPLICIT NONE
!      DIMENSION IS(4),KAPS(4),S(12)
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER, INTENT(IN) :: NU
      INTEGER :: K
      INTEGER, INTENT(IN) :: IBR
      INTEGER, INTENT(IN) :: IEX
      INTEGER, INTENT(IN) :: IS(4)
      INTEGER, INTENT(IN) :: KAPS(4)
      real(real64) , INTENT(INOUT) :: S(12)
!-----------------------------------------------
!
      STOP 'CXK: Error '
      END

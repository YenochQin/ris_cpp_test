!***********************************************************************
      subroutine SPICMV2(N, M, B, C)
!
!  Modified from the mpi version spicmvmpi.f by simply removing things
!  required by mpi communication.
!
!  Xinghong He  98-07-29
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:47   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use HMAT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dinit_I
!      use dmerge_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: N
      integer, intent(in) :: M
      real(real64), dimension(N,M), intent(in) :: B
      real(real64), dimension(N,M), INTENT(INOUT) :: C
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IBEG, ICOL, IEND, NELC, IV
      real(real64) :: DIAG, DL
!-----------------------------------------------
!
!
!   Initialise the result matrix; note that this is specific to the
!   data structure of DVDSON --- no overdimensioning
!
      CALL DINIT (N*M, 0.D0, C, 1)

      IBEG = 1
      DO ICOL = 1, N
            !IBEG = IENDC(ICOL-1)+1
            !IEND = IENDC(ICOL)
         IEND = IENDC(ICOL)
         NELC = IEND - IBEG + 1
         DO IV = 1, M
            DIAG = C(ICOL,IV) + EMT(IEND)*B(ICOL,IV)
            CALL DMERGE (NELC-1, B(:N,IV), C(:N,IV), IROW(IBEG), EMT(IBEG), B(&
               ICOL,IV), DL)
            C(ICOL,IV) = DIAG + DL
         END DO
         IBEG = IEND + 1
      END DO

      return
      end subroutine SPICMV2

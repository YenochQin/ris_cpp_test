!*******************************************************************
!                                                                  *
      subroutine SIXJ(I,J,K,L,M,N,ITIK,SI)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF 6j COEFFICIENT         *
!                                                                  *
!     | I/2  J/2  K/2 |                                            *
!     | L/2  M/2  N/2 |          (29.1A) [J.B.77]                  *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vanderbilt University,  Nashville               October  1996  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, ONE
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ixjtik_I
      use sixj5_I
      use sixj1_I
      use sixj35_I
      use sixj2_I
!      use gracah1_I
      use dracah_I
      use sixj3_I
      use sixj4_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer             :: I, J, K, L, M, N
      integer, intent(in) :: ITIK
      real(kind=real64)        :: SI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifA
      real(kind=real64), dimension(0:4,0:4,0:4,0:4,0:4,0:4) :: RACA
      real(kind=real64) :: UNDEF, A
      logical :: SAVE
!-----------------------------------------------
      DATA RACA/ 15625*1.D-20/
      DATA UNDEF/ 1.D-20/
      SI = ZERO
      if (ITIK /= 0) then
!
!     CHESKED TRIANGULAR CONDITIONS
!
         if (IXJTIK(I,J,K,L,M,N) == 0) return
      endif
      SAVE = .FALSE.
      if (MAX0(I,J,K,L,M,N) <= 4) then
         SI = RACA(I,J,K,L,M,N)
         if (SI == UNDEF) then
            SAVE = .TRUE.
         else
            return
         endif
      endif
!
!     CALCULATED IN CASE WHEN ONE OF PERAMETERS EQUAL 0
!
      if (I*J*K*L*M*N == 0) then
         if (I == 0) then
            A = DBLE((M + 1)*(K + 1))
            ifA = L + M + K
         else if (J == 0) then
            A = DBLE((L + 1)*(K + 1))
            ifA = I + M + N
         else if (K == 0) then
            A = DBLE((I + 1)*(L + 1))
            ifA = I + M + N
         else if (L == 0) then
            A = DBLE((J + 1)*(K + 1))
            ifA = I + J + K
         else if (M == 0) then
            A = DBLE((I + 1)*(K + 1))
            ifA = I + J + K
         else
            A = DBLE((I + 1)*(J + 1))
            ifA = I + J + K
         endif
         SI = ONE/DSQRT(A)
         if (MOD(ifA,4) /= 0) SI = -SI
!
!     THE CASE 1/2
!
      else if (MIN0(I,J,K,L,M,N) == 1) then
         if (I == 1) then
            CALL SIXJ5 (M, K, L, J, N, 0, SI)
         else if (J == 1) then
            CALL SIXJ5 (I, N, M, L, K, 0, SI)
         else if (K == 1) then
            CALL SIXJ5 (I, M, N, L, J, 0, SI)
         else if (L == 1) then
            CALL SIXJ5 (J, K, I, M, N, 0, SI)
         else if (M == 1) then
            CALL SIXJ5 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ5 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 1
!
      else if (MIN0(I,J,K,L,M,N) == 2) then
         if (I == 2) then
            CALL SIXJ1 (M, K, L, J, N, 0, SI)
         else if (J == 2) then
            CALL SIXJ1 (I, N, M, L, K, 0, SI)
         else if (K == 2) then
            CALL SIXJ1 (I, M, N, L, J, 0, SI)
         else if (L == 2) then
            CALL SIXJ1 (J, K, I, M, N, 0, SI)
         else if (M == 2) then
            CALL SIXJ1 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ1 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 3/2
!
      else if (MIN0(I,J,K,L,M,N) == 3) then
         if (I == 3) then
            CALL SIXJ35 (M, K, L, J, N, 0, SI)
         else if (J == 3) then
            CALL SIXJ35 (I, N, M, L, K, 0, SI)
         else if (K == 3) then
            CALL SIXJ35 (I, M, N, L, J, 0, SI)
         else if (L == 3) then
            CALL SIXJ35 (J, K, I, M, N, 0, SI)
         else if (M == 3) then
            CALL SIXJ35 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ35 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 2
!
      else if (MIN0(I,J,K,L,M,N) == 4) then
         if (I == 4) then
            CALL SIXJ2 (M, K, L, J, N, 0, SI)
         else if (J == 4) then
            CALL SIXJ2 (I, N, M, L, K, 0, SI)
         else if (K == 4) then
            CALL SIXJ2 (I, M, N, L, J, 0, SI)
         else if (L == 4) then
            CALL SIXJ2 (J, K, I, M, N, 0, SI)
         else if (M == 4) then
            CALL SIXJ2 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ2 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 5/2
!
      else if (MIN0(I,J,K,L,M,N) == 5) then
         CALL DRACAH (I, J, M, L, K, N, SI)
         if (MOD(I + J + M + L,4) /= 0) SI = -SI
!
!     CASES 3
!
      else if (MIN0(I,J,K,L,M,N) == 6) then
         if (I == 6) then
            CALL SIXJ3 (M, K, L, J, N, 0, SI)
         else if (J == 6) then
            CALL SIXJ3 (I, N, M, L, K, 0, SI)
         else if (K == 6) then
            CALL SIXJ3 (I, M, N, L, J, 0, SI)
         else if (L == 6) then
            CALL SIXJ3 (J, K, I, M, N, 0, SI)
         else if (M == 6) then
            CALL SIXJ3 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ3 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 7/2
!
      else if (MIN0(I,J,K,L,M,N) == 7) then
         CALL DRACAH (I, J, M, L, K, N, SI)
         if (MOD(I + J + M + L,4) /= 0) SI = -SI
!
!     CASES 4
!
      else if (MIN0(I,J,K,L,M,N) == 8) then
         if (I == 8) then
            CALL SIXJ4 (M, K, L, J, N, 0, SI)
         else if (J == 8) then
            CALL SIXJ4 (I, N, M, L, K, 0, SI)
         else if (K == 8) then
            CALL SIXJ4 (I, M, N, L, J, 0, SI)
         else if (L == 8) then
            CALL SIXJ4 (J, K, I, M, N, 0, SI)
         else if (M == 8) then
            CALL SIXJ4 (I, K, J, L, N, 0, SI)
         else
            CALL SIXJ4 (I, J, K, L, M, 0, SI)
         endif
!
!     THE CASE 9/2
!
      else if (MIN0(I,J,K,L,M,N) == 9) then
         CALL DRACAH (I, J, M, L, K, N, SI)
         if (MOD(I + J + M + L,4) /= 0) SI = -SI
!
!     CALCULATED OTHER CASES
!
      else
       CALL DRACAH(I,J,M,L,K,N,SI)
!         CALL GRACAH1 (I, J, M, L, K, N, SI)
!        CALL GRACAH(I,J,M,L,K,N,SI)
         if (MOD(I + J + M + L,4) /= 0) SI = -SI
      endif
      if (SAVE) RACA(I,J,K,L,M,N) = SI
      return
      end subroutine SIXJ

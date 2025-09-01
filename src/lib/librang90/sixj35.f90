!*******************************************************************
!                                                                  *
      subroutine SIXJ35(J,K,L,M,N,ITIK,SI)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF 6j COEFFICIENT         *
!                                                                  *
!     | J/2  K/2  L/2 |                                            *
!     | M/2  N/2  3/2 |             [B.M.X. 75]                    *
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
      use CONS_C,          only: ZERO, ONE, TWO, THREE, FOUR
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ixjtik_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer                   :: J, K, L, M, N
      integer, intent(in)       :: ITIK
      real(real64), intent(out) :: SI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I1
      real(real64) :: AS, A, B, C, AKA
!-----------------------------------------------
      SI = ZERO
      if (ITIK /= 0) then
!
!     CHESKED TRIANGULAR CONDITIONS
!
         if (IXJTIK(J,K,L,M,N,3) == 0) return
      endif
      I1 = (J + K + L)/2
      AS = DBLE(I1)
      A = DBLE(L)
      B = DBLE(J)
      C = DBLE(K)
      AKA = ONE
      if (MOD(I1,2) /= 0) AKA = -AKA
      if (J - N == 3) then
! -3
         if (K - M == 3) then
!  I                      -3/2  -3/2
            SI = AKA*DSQRT((AS - ONE)*AS*(AS + ONE)*(AS - A - TWO)*(AS - A - &
               ONE)*(AS - A)/((B - TWO)*(B - ONE)*B*(B + ONE)*(C - TWO)*(C - &
               ONE)*C*(C + ONE)))
         else if (M - K == 3) then
!  IV  P(12)              3/2   -3/2
            SI = AKA*DSQRT((AS - C - TWO)*(AS - C - ONE)*(AS - C)*(AS - B + ONE&
               )*(AS - B + TWO)*(AS - B + THREE)/((C + 1)*(C + TWO)*(C + THREE)&
               *(C + FOUR)*(B - TWO)*(B - ONE)*B*(B + ONE)))
         else if (K - M == 1) then
!  II  P(12)             -1/2   -3/2
            SI = AKA*DSQRT(THREE*AS*(AS + ONE)*(AS - A - ONE)*(AS - A)*(AS - C)&
               *(AS - B + ONE)/((C - ONE)*C*(C + ONE)*(C + TWO)*(B - TWO)*(B - &
               ONE)*B*(B + ONE)))
         else if (M - K == 1) then
!  III P(12)              1/2   -3/2
            SI = AKA*DSQRT(THREE*(AS + ONE)*(AS - A)*(AS - C - ONE)*(AS - C)*(&
               AS - B + ONE)*(AS - B + TWO)/(C*(C + ONE)*(C + TWO)*(C + THREE)*&
               (B - TWO)*(B - ONE)*B*(B + ONE)))
         endif
      else if (N - J == 3) then
!  3
         if (K - M == 3) then
!  IV                     -3/2   3/2
            SI = AKA*DSQRT((AS - B - TWO)*(AS - B - ONE)*(AS - B)*(AS - C + ONE&
               )*(AS - C + TWO)*(AS - C + THREE)/((B + ONE)*(B + TWO)*(B + &
               THREE)*(B + FOUR)*(C - TWO)*(C - ONE)*C*(C + ONE)))
         else if (M - K == 3) then
!  2       pataisyta               3/2   3/2
            SI = -AKA*DSQRT((AS + TWO)*(AS + THREE)*(AS + FOUR)*(AS - A + ONE)*&
               (AS - A + TWO)*(AS - A + THREE)/((B + ONE)*(B + TWO)*(B + THREE)&
               *(B + FOUR)*(C + ONE)*(C + TWO)*(C + THREE)*(C + FOUR)))
         else if (K - M == 1) then
!  1   P(12)   pataisytas          -1/2    3/2
            SI = -AKA*DSQRT(THREE*(AS + TWO)*(AS - A + ONE)*(AS - C + ONE)*(AS&
                - C + TWO)*(AS - B - ONE)*(AS - B)/((C - ONE)*C*(C + ONE)*(C + &
               TWO)*(B + ONE)*(B + TWO)*(B + THREE)*(B + FOUR)))
         else if (M - K == 1) then
!  3  P(12)     taisyta           1/2    3/2
            SI = AKA*DSQRT(THREE*(AS + TWO)*(AS + THREE)*(AS - A + ONE)*(AS - A&
                + TWO)*(AS - B)*(AS - C + ONE)/(C*(C + ONE)*(C + TWO)*(C + &
               THREE)*(B + ONE)*(B + TWO)*(B + THREE)*(B + FOUR)))
         endif
! -1
      else if (J - N == 1) then
         if (K - M == 3) then
!  II                   -3/2   -1/2
            SI = AKA*DSQRT((THREE*AS*(AS + ONE)*(AS - A - ONE)*(AS - A)*(AS - B&
               )*(AS - C + ONE))/((B - ONE)*B*(B + ONE)*(B + TWO)*(C - TWO)*(C&
                - ONE)*C*(C + ONE)))
         else if (M - K == 3) then
!  1                     3/2   -1/2
            SI = -AKA*DSQRT(THREE*(AS + TWO)*(AS - A + ONE)*(AS - B + ONE)*(AS&
                - B + TWO)*(AS - C - ONE)*(AS - C)/((B - ONE)*B*(B + ONE)*(B + &
               TWO)*(C + ONE)*(C + TWO)*(C + THREE)*(C + FOUR)))
         else if (K - M == 1) then
!  V                    -1/2   -1/2
            SI = AKA*(TWO*(AS - B)*(AS - C) - (AS + TWO)*(AS - A - ONE))*DSQRT(&
               (AS + ONE)*(AS - A)/((B - ONE)*B*(B + ONE)*(B + TWO)*(C - ONE)*C&
               *(C + ONE)*(C + TWO)))
         else if (M - K == 1) then
!  VI P(12)              1/2   -1/2
            SI = AKA*((AS - B + TWO)*(AS - C + ONE) - TWO*(AS - A + ONE)*(AS + &
               ONE))*DSQRT((AS - C)*(AS - B + ONE)/(C*(C + ONE)*(C + TWO)*(C + &
               THREE)*(B - ONE)*B*(B + ONE)*(B + TWO)))
         endif
! 1
      else if (N - J == 1) then
         if (K - M == 3) then
!  III                  -3/2    1/2
            SI = AKA*DSQRT(THREE*(AS + ONE)*(AS - A)*(AS - B - ONE)*(AS - B)*(&
               AS - C + ONE)*(AS - C + TWO)/(B*(B + ONE)*(B + TWO)*(B + THREE)*&
               (C - TWO)*(C - ONE)*C*(C + ONE)))
         else if (M - K == 3) then
!  3              pataisyta       3/2    1/2
            SI = AKA*DSQRT(THREE*(AS + TWO)*(AS + THREE)*(AS - A + ONE)*(AS - A&
                + TWO)*(AS - B + ONE)*(AS - C)/(B*(B + ONE)*(B + TWO)*(B + &
               THREE)*(C + ONE)*(C + TWO)*(C + THREE)*(C + FOUR)))
         else if (K - M == 1) then
!  VI                   -1/2    1/2
            SI = AKA*((AS - C + TWO)*(AS - B + ONE) - TWO*(AS - A + ONE)*(AS + &
               ONE))*DSQRT((AS - B)*(AS - C + ONE)/(B*(B + ONE)*(B + TWO)*(B + &
               THREE)*(C - ONE)*C*(C + ONE)*(C + TWO)))
         else if (M - K == 1) then
!  4      pataisyta               1/2    1/2
            SI = -AKA*(TWO*(AS - B)*(AS - C) - (AS + THREE)*(AS - A))*DSQRT((AS&
                + TWO)*(AS - A + ONE)/(B*(B + ONE)*(B + TWO)*(B + THREE)*C*(C&
                + ONE)*(C + TWO)*(C + THREE)))
         endif
      endif
      return
      end subroutine SIXJ35

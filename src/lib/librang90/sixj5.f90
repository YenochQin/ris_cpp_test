!*******************************************************************
!                                                                  *
      subroutine SIXJ5(J,K,L,M,N,ITIK,SI)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF 6j COEFFICIENT         *
!                                                                  *
!     | J/2  K/2  L/2 |                                            *
!     | M/2  N/2  1/2 |             [B.M.X. 75]                    *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             March    1995  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, ONE, TWO
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
      real(kind=real64), intent(out) :: SI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: I1
      real(kind=real64) :: AS, A, B, C, AKA
!-----------------------------------------------
      SI = ZERO
      if (ITIK /= 0) then
!
!     CHESKED TRIANGULAR CONDITIONS
!
         if (IXJTIK(J,K,L,M,N,1) == 0) return
      endif
      I1 = (J + K + L)/2
      AS = DBLE(I1)
      A = DBLE(L)
      B = DBLE(K)
      C = DBLE(J)
      AKA = ONE
      if (MOD(I1,2) /= 0) AKA = -AKA
      if (K < M) then
         if (J < N) then
!              M > K,  J < N.
            SI = -AKA*DSQRT((AS + TWO)*(AS - A + ONE)/((B + ONE)*(B + TWO)*(C&
                + ONE)*(C + TWO)))
         else if (J > N) then
!              M > K,  J > N.
            SI = AKA*DSQRT((AS - C + ONE)*(AS - B)/((B + ONE)*(B + TWO)*C*(C + &
               ONE)))
         endif
      else if (K > M) then
         if (J < N) then
!             M < K,  J < N.
            SI = AKA*DSQRT((AS - C)*(AS - B + ONE)/(B*(B + ONE)*(C + ONE)*(C + &
               TWO)))
         else if (J > N) then
!             M < K,  J > N.
            SI = AKA*DSQRT((AS + ONE)*(AS - A)/(B*(B + ONE)*C*(C + ONE)))
         endif
      endif
      return
      end subroutine SIXJ5

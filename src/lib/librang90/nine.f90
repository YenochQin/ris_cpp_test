!*******************************************************************
!                                                                  *
      subroutine NINE(J1,J2,J3,L1,L2,L3,K1,K2,K3,I,INN,AA)
!
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF 9j COEFFICIENT         *
!                                                                  *
!     |  J1/2  J2/2  J3/2 |                                        *
!     |  L1/2  L2/2  L3/2 |                                        *
!     |  K1/2  K2/2  K3/2 |                                        *
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
      use CONS_C,          only: ZERO
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      use nine0_I
      use sixj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: J1, J2, J3, L1, L2, L3, K1, K2, K3
      integer, intent(in)  :: I
      integer, intent(out) :: INN
      real(real64)  :: AA
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: N1, N2, N3, N4, N5, N6, MAX_, MIN_, IX
      real(real64) :: S1, S2, S3, X
!-----------------------------------------------
      if (I == 1) then
         INN = 0
         if (ITTK(J1,J2,J3) == 0) return
         if (ITTK(L1,L2,L3) == 0) return
         if (ITTK(K1,K2,K3) == 0) return
         if (ITTK(J1,L1,K1) == 0) return
         if (ITTK(J2,L2,K2) == 0) return
         if (ITTK(J3,L3,K3) == 0) return
         INN = 1
         return
      endif
      if (J1*J2*J3*L1*L2*L3*K1*K2*K3 == 0) then
         INN = 1
         CALL NINE0 (J1, J2, J3, L1, L2, L3, K1, K2, K3, AA)
      else
         N1 = IABS(J1 - K3)
         N2 = IABS(L3 - J2)
         N3 = IABS(L1 - K2)
         N4 = IABS(J2 - L3)
         N5 = IABS(K2 - L1)
         N6 = IABS(J1 - K3)
         MAX_ = MAX0(N1,N2,N3,N4,N5,N6)
         N1 = J1 + K3
         N2 = L3 + J2
         N3 = J2 + L3
         N4 = K2 + L1
         N5 = J1 + K3
         N6 = L1 + K2
         MIN_ = MIN0(N1,N2,N3,N4,N5,N6)
         INN = 1
         AA = ZERO
         DO IX = MAX_, MIN_, 2
            CALL SIXJ (J1, J2, J3, L3, K3, IX, 0, S1)
            CALL SIXJ (L1, L2, L3, J2, IX, K2, 0, S2)
            CALL SIXJ (K1, K2, K3, IX, J1, L1, 0, S3)
            X = S1*S2*S3*DBLE(IX + 1)
            if (MOD(IX,2) /= 0) X = -X
            AA = X + AA
         END DO
      endif
      return
      end subroutine NINE

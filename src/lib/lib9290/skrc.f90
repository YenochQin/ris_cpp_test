!***********************************************************************
!                                                                      *
      subroutine SKRC(IS, KAPS, KS, KD1, KD2, KE1, KE2)
!                                                                      *
!   Determines the range of the tensor rank k for Coulomb integral.    *
!                                                                      *
!                                           Last update: 09 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:43   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(out) :: KD1
      integer, intent(out) :: KD2
      integer, intent(out) :: KE1
      integer, intent(out) :: KE2
      integer, dimension(4), intent(in) :: IS
      integer, dimension(4), intent(in) :: KAPS
      integer, dimension(4), intent(in) :: KS
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ISD1, ISD2, KD1A, KD1B, KD2A, KD2B, ISE1, ISE2, KE1A, &
         KE1B, KE2A, KE2B
!-----------------------------------------------
!
!
      KD2 = 0
      KE2 = 0
!
!   Direct terms --- KD1 = minimum k , KD2 = number of terms
!
      ISD1 = 1
      if (KAPS(1)*KAPS(3) < 0) ISD1 = -1
      ISD2 = 1
      if (KAPS(2)*KAPS(4) < 0) ISD2 = -1
      KD1A = ABS(KS(1)-KS(3))
      if (ISD1 < 0) KD1A = KD1A + 2
      KD1B = ABS(KS(2)-KS(4))
      if (ISD2 < 0) KD1B = KD1B + 2
      if (MOD((KD1A - KD1B)/2,2) == 0) then
         KD2A = KS(1) + KS(3) - 2
         if (ISD1 > 0) KD2A = KD2A - 2
         KD2B = KS(2) + KS(4) - 2
         if (ISD2 > 0) KD2B = KD2B - 2
         KD1 = MAX(KD1A,KD1B)/2
         KD2 = MIN(KD2A,KD2B)/2
         KD2 = (KD2 - KD1)/2 + 1
      endif
!
!   Exchange terms --- KE1 = minimum k , KE2 = number of terms
!
      if (IS(1)==IS(2) .OR. IS(3)==IS(4)) return
      ISE1 = 1
      if (KAPS(1)*KAPS(4) < 0) ISE1 = -1
      ISE2 = 1
      if (KAPS(2)*KAPS(3) < 0) ISE2 = -1
      KE1A = ABS(KS(1)-KS(4))
      if (ISE1 < 0) KE1A = KE1A + 2
      KE1B = ABS(KS(2)-KS(3))
      if (ISE2 < 0) KE1B = KE1B + 2
      if (MOD((KE1A - KE1B)/2,2) /= 0) return
      KE2A = KS(1) + KS(4) - 2
      if (ISE1 > 0) KE2A = KE2A - 2
      KE2B = KS(2) + KS(3) - 2
      if (ISE2 > 0) KE2B = KE2B - 2
      KE1 = MAX(KE1A,KE1B)/2
      KE2 = MIN(KE2A,KE2B)/2
      KE2 = (KE2 - KE1)/2 + 1
!
      return
      end subroutine SKRC

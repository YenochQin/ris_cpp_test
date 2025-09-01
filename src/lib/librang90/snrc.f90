!***********************************************************************
!                                                                      *
      subroutine SNRC(IS, KAPS, KS, ND1, ND2, NE1, NE2, IBRD, IBRE)
!                                                                      *
!   Determines the range of tensor rank NU for direct/exchange terms,  *
!   and classifies the types of radial integral.                       *
!                                                                      *
!   Input variables:                                                   *
!                                                                      *
!      IS      : Orbital labels                                        *
!      KAPS    : Values of 2*kappa                                     *
!      KS      : Values of 2*J+1                                       *
!                                                                      *
!   Outputs:                                                           *
!                                                                      *
!      ND1/NE1 : Lowest NU value for direct/exchange types             *
!      ND2/NE2 : Corresponding number of contributing NU values: NU    *
!                = ND1, ND1+2 ,..., ND1+2*(ND2-1) etc                  *
!      IBRD    : Classify types of  radial  integrals  contributing;   *
!      IBRE      negative value implies null contribution              *
!                                                                      *
!                                           Last update: 09 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(out) :: ND1
      integer, intent(out) :: ND2
      integer, intent(out) :: NE1
      integer, intent(out) :: NE2
      integer, intent(out) :: IBRD
      integer, intent(out) :: IBRE
      integer, intent(in) :: IS(4)
      integer, intent(in) :: KAPS(4)
      integer, intent(in) :: KS(4)
!----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IAC, IAD, ND1A, ND2A, NE1A, NE2A
!-----------------------------------------------
!
      ND2 = 0
      NE2 = 0
!
!   2.0  Form limits for direct terms
!
      IAC = 1
      if (KAPS(1)*KAPS(3) < 0) IAC = -1
      IAD = 1
      if (KAPS(2)*KAPS(4) < 0) IAD = -1
      ND1 = ABS(KS(1)-KS(3))/2 - 1
      if (IAC == (-1)) ND1 = ND1 + 1
      if (ND1 == (-1)) ND1 = 1
      ND1A = ABS(KS(2)-KS(4))/2 - 1
      if (IAD == (-1)) ND1A = ND1A + 1
      if (ND1A == (-1)) ND1A = 1
      if (MOD(ND1 - ND1A,2) /= 0) then
         IBRD = -1
      else
         ND2 = ABS(KS(1)+KS(3))/2
         if (IAC == (-1)) ND2 = ND2 + 1
         ND2A = ABS(KS(2)+KS(4))/2
         if (IAD == (-1)) ND2A = ND2A + 1
         ND1 = MAX(ND1,ND1A)
         ND2 = MIN(ND2,ND2A)
         ND2 = (ND2 - ND1)/2 + 1
!
!   2.1  Identify type of radial integrals
!
         IBRD = 1
         if (IS(1)==IS(3) .AND. IS(2)/=IS(4) .OR. IS(1)/=IS(3) .AND. IS(2)==IS(&
            4)) IBRD = 2
         if (IS(1)==IS(3) .AND. IS(2)==IS(4)) IBRD = 3
      endif
!
!   3.0  Form limits for exchange terms
!
      if (IS(1)==IS(2) .OR. IS(3)==IS(4)) then
         IBRE = -1
         return
      endif
      IAC = 1
      if (KAPS(1)*KAPS(4) < 0) IAC = -1
      IAD = 1
      if (KAPS(2)*KAPS(3) < 0) IAD = -1
      NE1 = IABS(KS(1)-KS(4))/2 - 1
      if (IAC == (-1)) NE1 = NE1 + 1
      if (NE1 == (-1)) NE1 = 1
      NE1A = ABS(KS(2)-KS(3))/2 - 1
      if (IAD == (-1)) NE1A = NE1A + 1
      if (NE1A == (-1)) NE1A = 1
      if (MOD(NE1 - NE1A,2) /= 0) then
         IBRE = -1
         return
      endif
!
      NE2 = ABS(KS(1)+KS(4))/2
      if (IAC == (-1)) NE2 = NE2 + 1
      NE2A = ABS(KS(2)+KS(3))/2
      if (IAD == (-1)) NE2A = NE2A + 1
      NE1 = MAX(NE1,NE1A)
      NE2 = MIN(NE2,NE2A)
      NE2 = (NE2 - NE1)/2 + 1
!
!   3.1  Identify type of radial integrals
!
      IBRE = 1
      if (IS(1)==IS(4) .AND. IS(2)/=IS(3) .OR. IS(1)/=IS(4) .AND. IS(2)==IS(3)&
         ) IBRE = 2
      if (IS(1)==IS(3) .AND. IS(2)==IS(4)) IBRE = 4
      return
!
      end subroutine SNRC

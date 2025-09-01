!***********************************************************************
!                                                                      *
      subroutine CORD(JA, JB, JA1, IPCA, JB1)
!-----------------------------------------------
!                                                                      *
!   Computes the MCP coefficients for contributions involving closed   *
!   shells.  The  standard formulae are given in I P Grant, Advances   *
!   in Physics  19 (1970) 747, Eq. (8.33).  In this segment JA1, JB1   *
!   point to the JLIST array, IA1, IB1 to the full list of orbitals.   *
!                                                                      *
!   Call(s) to: CLRX, SPEAK.                                           *
!                                                                      *
!                                           Last update: 15 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:57   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use ORB_C
      use M_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use speak_I
      use clrx_I
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: JA
      integer  :: JB
      integer, intent(in) :: JA1
      integer, intent(in) :: IPCA
      integer, intent(in) :: JB1
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(kind=real64), parameter :: EPS = 1.0D-10
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IA1,IB1,NS,KAP1,J1,NQS1,NUMAX,NU,KAP2,J2,NQS2,NUMIN
      real(kind=real64) :: X,CONST,GAM
!-----------------------------------------------
!
!
!   Set quantum numbers required.
!
      if (IPCA == 2) then
         IA1 = KLIST(JA1)
      else
         IA1 = JLIST(JA1)
      endif
      IB1 = KLIST(JB1)
!
!   Force IA1 to be greater than IB1
!
      if (IA1 > IB1) then
         NS = IA1
         IA1 = IB1
         IB1 = NS
      endif
!
      KAP1 = NAK(IA1)
      J1 = IABS(KAP1)
      NQS1 = NQ1(IA1)
!
      if (IA1 == IB1) then
!
!   Case when IA1 .EQ. IB1
!
         X = DBLE(NQS1*(NQS1 - 1)/2)
         CALL SPEAK (JA, JB, IA1, IB1, IA1, IB1, 0, X)
         NUMAX = J1 + J1 - 2
         if (NUMAX <= 0) return
         CONST = DBLE(NQS1*NQS1/2)
         DO NU = 2, NUMAX, 2
            GAM = CLRX(KAP1,NU,KAP1)
            X = -CONST*GAM*GAM
            if (ABS(X) < EPS) CYCLE
            CALL SPEAK (JA, JB, IA1, IB1, IA1, IB1, NU, X)
         END DO
!
!   Case when IA1 .NE. IB1
!
      else
!
         KAP2 = NAK(IB1)
         J2 = ABS(KAP2)
         NQS2 = NQ1(IB1)
         CONST = DBLE(NQS1*NQS2)
         CALL SPEAK (JA, JB, IA1, IB1, IA1, IB1, 0, CONST)
         NUMIN = ABS(J1 - J2)
         NUMAX = J1 + J2 - 1
         if (KAP1*KAP2 < 0) NUMIN = NUMIN + 1
         DO NU = NUMIN, NUMAX, 2
            GAM = CLRX(KAP1,NU,KAP2)
            X = -CONST*GAM*GAM
            if (ABS(X) < EPS) CYCLE
            CALL SPEAK (JA, JB, IA1, IB1, IB1, IA1, NU, X)
         END DO
!
      endif
!
      return
      end subroutine CORD

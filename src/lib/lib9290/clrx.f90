!***********************************************************************
!                                                                      *
      real(real64) FUNCTION CLRX (KAPPAA, K, KAPPAB)
!                                                                      *
!   The value of CLRX is the 3-j symbol:                               *
!                                                                      *
!                    ( JA        K        JB  )                        *
!                    ( 1/2       0       -1/2 )                        *
!                                                                      *
!   The  K'S are kappa angular quantum numbers. The formula is taken   *
!   from D M Brink and G R Satchler, <Angular Momentum>, second edi-   *
!   tion (Oxford: Clarendon press, 1968), p 138.   The logarithms of   *
!   the first  MFACT  factorials must be available in  COMMON/FACTS/   *
!   for this program to function correctly. Note that  N!  is stored   *
!   in FACT(N+1)                                                       *
!                                                                      *
!   No subroutines called.                                             *
!                                                                      *
!   Written by Farid A Parpia, at Oxford   Last updated: 06 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:52   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use FACTS_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: KAPPAA, K, KAPPAB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KA, KB, KAPKB, KABKP, KAMKB, KBMKA
      real(real64) :: EXPTRM
!-----------------------------------------------
!
!
!   Determine the absolute values of the kappas
!
      KA = ABS(KAPPAA)
      KB = ABS(KAPPAB)
!
!   Perform the triangularity check
!
      if (ABS(KA - KB)<=K .AND. KA+KB-1>=K) then
!
!   Triangularity satisfied; compute the 3j coefficient
!
!   Begin with the logarithm of the square of the leading term
!
         EXPTRM = -LOG(DBLE(KA*KB))
!
!   Compute the logarithm of the square root of the leading term
!   and the factorial part that doesn't depend on the parity of
!   KA+KB+K (the delta factor)
!
         KAPKB = KA + KB
         KABKP = KAPKB + K
         KAMKB = KA - KB
         KBMKA = KB - KA
         EXPTRM = 0.5D00*(EXPTRM + GAM(KAPKB-K)+GAM(KAMKB+K+1)+GAM(KBMKA+K+1)-&
            GAM(KABKP+1))
!
!   The remainder depends on the parity of KA+KB+K
!
         if (MOD(KABKP,2) == 0) then
!
!   Computation for even parity case
!
!   Include the phase factor: a minus sign if necessary
!
            if (MOD(3*KABKP/2,2) == 0) then
               CLRX = 1.0D00
            else
               CLRX = -1.0D00
            endif
!
!   Include the contribution from the factorials
!
            EXPTRM = EXPTRM + GAM((KABKP+2)/2) - GAM((KAPKB-K)/2) - GAM((KAMKB+&
               K+2)/2) - GAM((KBMKA+K+2)/2)
!
         else
!
!   Computation for odd parity case
!
!   Include the phase factor: a minus sign if necessary
!
            if (MOD((3*KABKP - 1)/2,2) == 0) then
               CLRX = 1.0D00
            else
               CLRX = -1.0D00
            endif
!
!   Include the contribution from the factorials
!
            EXPTRM = EXPTRM + GAM((KABKP+1)/2) - GAM((KAPKB-K+1)/2) - GAM((&
               KAMKB+K+1)/2) - GAM((KBMKA+K+1)/2)
!
         endif
!
!   Final assembly
!
         CLRX = CLRX*EXP(EXPTRM)
!
      else
!
!   Triangularity violated; set the coefficient to zero
!
         CLRX = 0.0D00
!
      endif
!
      return
!
      END FUNCTION CLRX

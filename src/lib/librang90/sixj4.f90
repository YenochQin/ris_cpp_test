!*******************************************************************
!                                                                  *
      subroutine SIXJ4(JC,JE,JD,JB,JF,ITIK,SI)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF 6j COEFFICIENT         *
!                                                                  *
!     | JC/2  JE/2  JD/2 |                                         *
!     | JB/2  JF/2    4  |                                         *
!                                                                  *
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
      use CONS_C,          only: ZERO, HALF, ONE, TWO, THREE, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ixjtik_I
      use dracah_I
      use sixj2_I
      use sixj3_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer             :: JC, JE, JD, JB, JF
      integer, intent(in) :: ITIK
      real(kind=real64)        :: SI
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(kind=real64) :: A, C, E, D, B, F, X1, X2, X3, S2, S3
!-----------------------------------------------
      SI = ZERO
      if (ITIK /= 0) then
!
!     CHESKED TRIANGULAR CONDITIONS
!
         if (IXJTIK(JC,JE,JD,JB,JF,8) == 0) return
      endif
      if (IXJTIK(JC,JE,JD,JB,JF,6) == 0) then
         CALL DRACAH (JC, JE, JF, JB, JD, 8, SI)
         if (MOD(JC + JE + JF + JB,4) /= 0) SI = -SI
      else
         A = THREE
         C = DBLE(JC)*HALF
         E = DBLE(JE)*HALF
         D = DBLE(JD)*HALF
         B = DBLE(JB)*HALF
         F = DBLE(JF)*HALF
         X1 = A*DSQRT((A+B+E+TWO)*(A-B+E+ONE)*(A+B-E+ONE)*((-         &
            A)+B+E)*(A+C+F+TWO)*(A-C+F+ONE)*(A+C-F+ONE)*(             &
            (-A)+C+F))
         X2 = (A+ONE)*DSQRT((A+B+E+ONE)*(A-B+E)*(A+B-E)*((-A)         &
             + B+E+ONE)*(A+C+F+ONE)*(A-C+F)*(A+C-F)*((-A)+C           &
             + F+ONE))
         X3 = (TWO*A+ONE)*(TWO*(A*(A+ONE)*D*(D+ONE)-B*(B+ONE)*C*(C+   &
            ONE)-E*(E+ONE)*F*(F+ONE))+(A*(A+ONE)-B*(B+ONE)-E*(E       &
             +ONE))*(A*(A+ONE)-C*(C+ONE)-F*(F+ONE)))
         if (DABS(X2) < EPS) then
            S2 = ZERO
         else
            CALL SIXJ2 (JC, JE, JD, JB, JF, 0, S2)
         endif
         if (DABS(X3) < EPS) then
            S3 = ZERO
         else
            CALL SIXJ3 (JC, JE, JD, JB, JF, 0, S3)
         endif
         SI = (X3*S3 - X2*S2)/X1
      endif
      return
      end subroutine SIXJ4

!***********************************************************************
!                                                                      *
      real(real64) FUNCTION RINTDENS (I, J)
!                                                                      *
!   The value of RINTDENS is an approximation to:                      *
!                                                                      *
!                                                                      *
!      (4pi^)-1  r^-2 |P (r)*P (r) + Q (r)*Q (r) | r -> 0              *
!                     I     J       I     J                            *
!                                                                      *
!   Call(s) to: [SMS92]:  POLINT                                       *
!                                                                      *
!   Written by Per Jonsson             Last revision: 24 Dec 1992      *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:07:11   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  11/02/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use def_C,           only: cvac, pi
      use grid_C
      use wave_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use polint_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: I, J
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: L, II, NMIN
      real(real64), dimension(3) :: XA, YA
      real(real64) :: DENS
!-----------------------------------------------
!
      NMIN = 10

      II = 1
      DO L = NMIN,NMIN+4,2
         XA(II) = R(L)
         YA(II) = (PF(L,I)*PF(L,J) + QF(L,I)*QF(L,J))/(4.0D00*PI*R(L)*R(L))
         II = II + 1
      END DO
      CALL POLINT (XA,YA,DENS)
      RINTDENS = DENS
!
      return
      END FUNCTION RINTDENS

!*******************************************************************
!                                                                  *
      subroutine C1E0SM(Q,QM,C,CM,A)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING              *
!                                                 ---         ---  *
!                                                 I  Q   1  C   I  *
!     CLEBSCH - GORDAN COEFFICIENT:               I             I  *
!                                                 I  QM  0  CM  I  *
!                                                 ---         ---  *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             December 1993  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, TENTH, ONE, TWO, THREE, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(real64), intent(in)  :: Q, QM, C, CM
      real(real64), intent(out) :: A
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IIQ, IIC, IS, IG
!-----------------------------------------------
      A=ZERO
      IIQ=TWO*Q+TENTH
      IIC=TWO*C+TENTH
      if(ITTK(IIQ,IIC,2) == 0)return
      if(DABS(QM-CM) > EPS) return
      if((Q+TENTH) < DABS(QM)) return
      if((C+TENTH) < DABS(CM)) return
      if(DABS(QM) <= EPS) then
       IS=Q+C+ONE+TENTH
       if((IS/2)*2 /= IS) return
      END if
      IG=Q-C+TWO+TENTH
      if(IG <= 0) return
      if(IG > 3) return
      if (IG == 1) then
        A=DSQRT(((C+CM)*(C-CM))/((TWO*C-ONE)*C))
      else if (IG == 2) then
        A=CM/DSQRT(C*(C+ONE))
      else if (IG == 3) then
        A=-DSQRT(((C+CM+ONE)*(C-CM+ONE))/((C+ONE)*(TWO*C+THREE)))
      END if
      return
      end subroutine C1E0SM

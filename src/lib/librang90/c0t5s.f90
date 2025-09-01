!*******************************************************************
!                                                                  *
      subroutine C0T5S(Q,QM,SM,C,CM,A)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING              *
!                                                ---          ---  *
!                                                I  Q  1/2  C   I  *
!     CLEBSCH - GORDAN COEFFICIENT:              I              I  *
!                                                I  QM  SM  CM  I  *
!                                                ---          ---  *
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
      use CONS_C,          only: ZERO, TENTH , HALF, ONE, TWO, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(real64), intent(in)  :: Q, QM, SM, C, CM
      real(real64), intent(out) :: A
!      dimension GC(2)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer                    :: IIQ, IIC, IE
      real(real64), dimension(2) :: GC
!-----------------------------------------------
      GC(1)=ONE
      GC(2)=-ONE
      A=ZERO
      IIQ=TWO*Q+TENTH
      IIC=TWO*C+TENTH
      if(ITTK(IIQ,IIC,1) == 0)return
      if(DABS(QM+SM-CM) > EPS)return
      if((HALF+TENTH) < DABS(SM))return
      if((Q+TENTH) < DABS(QM))return
      if((C+TENTH) < DABS(CM))return
      IE=DABS(HALF-SM)+ONE+TENTH
      if(DABS(Q+HALF-C) < EPS) then
        A=DSQRT((C+GC(IE)*CM)/(TWO*C))
      else
        if(DABS(Q-HALF-C) > EPS)return
        A=-GC(IE)*DSQRT((C-GC(IE)*CM+ONE)/(TWO*C+TWO))
      endif
      return
      end subroutine C0T5S

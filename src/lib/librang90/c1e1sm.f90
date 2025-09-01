!*******************************************************************
!                                                                  *
      subroutine C1E1SM(Q,QM,SM,C,CM,A)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING              *
!                                                 ---         ---  *
!                                                 I  Q   1  C   I  *
!     CLEBSCH - GORDAN COEFFICIENT:               I             I  *
!                                                 I  QM  1  CM  I  *
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
      real(real64), intent(in)  :: Q, QM, SM, C, CM
      real(real64), intent(out) :: A
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer                    :: IE, IIQ, IIC
      real(real64), dimension(2) :: GC
!-----------------------------------------------
      GC(1)=ONE
      GC(2)=-ONE
      A=ZERO
      IIQ=TWO*Q+TENTH
      IIC=TWO*C+TENTH
      if(ITTK(IIQ,IIC,2).EQ.0)return
      if(DABS(QM+SM-CM).GT.EPS)return
      if((Q+TENTH).LT.DABS(QM))return
      if((C+TENTH).LT.DABS(CM))return
      IE=0
      if(DABS(SM-ONE).LT.EPS)IE=1
      if(DABS(SM+ONE).LT.EPS)IE=2
      if(IE.EQ.0)return
      if(DABS(Q+ONE-C).LT.EPS) then
        A=DSQRT((C+GC(IE)*CM-ONE)*(C+GC(IE)*CM)/ &
        ((TWO*C-ONE)*TWO*C))
      else if(DABS(Q-C).LT.EPS) then
        A=-GC(IE)*DSQRT((C+GC(IE)*CM)*(C-GC(IE)*CM+ONE)/ &
        ((C+ONE)*TWO*C))
      else if(DABS(Q-ONE-C).GT.EPS) then
        return
      else
        A=DSQRT((C-GC(IE)*CM+ONE)*(C-GC(IE)*CM+TWO)/ &
        ((TWO*C+TWO)*(TWO*C+THREE)))
      END if
      return
      end subroutine C1E1SM

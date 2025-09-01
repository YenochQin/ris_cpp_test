!*******************************************************************
!                                                                  *
      subroutine CLE0SM(Q,QM,S,C,CM,A)
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING              *
!                                                 ---         ---  *
!                                                 I  Q   S  C   I  *
!     CLEBSCH - GORDAN COEFFICIENT:               I             I  *
!                                                 I  QM  0  CM  I  *
!                                                 ---         ---  *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                            December 1993   *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, TENTH, ONE, TWO, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ittk_I
      use c1e0sm_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(kind=real64), intent(in)  :: Q, QM, S, C, CM
      real(kind=real64), intent(out) :: A
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IIQ, IIC, IIS
!-----------------------------------------------
      A=ZERO
      IIQ=TWO*Q+TENTH
      IIC=TWO*C+TENTH
      IIS=TWO*S+TENTH
      if(ITTK(IIQ,IIC,IIS).EQ.0)return
      if(S.LT.EPS) then
       if((Q+TENTH).LT.DABS(QM))return
        if((C+TENTH).LT.DABS(CM))return
        if(DABS(Q-C).GT.EPS)return
        if(DABS(QM-CM).GT.EPS)return
        A=ONE
      else
        CALL C1E0SM(Q,QM,C,CM,A)
      END if
      return
      end subroutine CLE0SM

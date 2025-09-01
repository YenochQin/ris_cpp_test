!*******************************************************************
!                                                                  *
      integer FUNCTION IZAS1(IB,QB,IK,QK)
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             March    1995  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: TENTH, TWO
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in) :: IB, IK
      real(real64), intent(in) :: QB, QK
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IQB, IQK
!-----------------------------------------------
      IZAS1=0
      IQB=TWO*DABS(QB)+TENTH
      if(IQB > IB)return
      if(MOD(IB+IQB,2) /= 0)return
      IQK=TWO*DABS(QK)+TENTH
      if(IQK > IK)return
      if(MOD(IK+IQK,2) /= 0)return
      IZAS1=1
      return
      END  FUNCTION IZAS1

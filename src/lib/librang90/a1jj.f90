!*******************************************************************
!                                                                  *
      subroutine A1JJ(IK,BK,ID,BD,QM1,A)
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, EPS
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in), dimension(7) :: IK, ID
      real(real64), intent(in), dimension(3) :: BK, BD
      real(real64), intent(in)               :: QM1
      real(real64), intent(out)              :: A
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: ifAZ, ISUMA
      real(real64) :: AB
!-----------------------------------------------
      A=ZERO
      if(QM1 < EPS) then
        ISUMA=(ID(6)+1)*ID(4)
        AB=DBLE(ISUMA)
        A=DSQRT(AB)
        ifAZ=ID(6)+ID(3)-IK(6)+ID(4)*2
        if((ifAZ/4)*4 /= ifAZ)A=-A
      else
        ISUMA=(IK(6)+1)*IK(4)
        AB=DBLE(ISUMA)
        A=DSQRT(AB)
        if((IK(4)/2)*2 /= IK(4))A=-A
      endif
      return
      end subroutine A1JJ

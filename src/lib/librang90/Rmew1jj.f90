!*******************************************************************
!                                                                  *
      subroutine RMEW1JJ(J1,J2,K1,K2,COEF)
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
      use CONS_C,          only: ZERO
      use ribojj_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: J1, J2, K1, K2
      real(kind=real64), intent(out) :: COEF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, dimension(2) :: I10, I01
!-----------------------------------------------
      DATA I01/6,0/
      DATA I10/0,6/
!
      COEF=ZERO
      if(IMPTJJ(J1) /= IMPTJJ(J2)) return
      if(J1 > 2) return
      if(K1 == 0 .AND. K2 == 0) then
        COEF=-DSQRT(DBLE(2))
      elseif(K1 == 1 .AND. K2 == 0) then
        COEF=-DSQRT(DBLE(I10(J1)))
      elseif(K1 == 0 .AND. K2 == 1) then
        COEF=-DSQRT(DBLE(I01(J1)))
      else
        WRITE(0,'(A,4I5)') ' J1 J2 = ',J1,J2
        WRITE(0,'(A)') ' ERROR IN SUB. RMEW1JJ '
        STOP
      endif
      return
      end subroutine RMEW1JJ

!*******************************************************************
!                                                                  *
      subroutine RMEW7JJ(J1,J2,K1,K2,COEF)
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
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rmew7bjj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: J1, J2, K1, K2
      real(kind=real64), intent(out) :: COEF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, dimension(14) :: I00, I10, I01
!-----------------------------------------------
      DATA I00/32,48,128,80,96,128,20,60,20,108,36,44,156,68/
      DATA I10/6,9,120,15,18,24,2*30,0,54,2*0,78,0/
      DATA I01/10,35,168,165,286,680,0,30,10,180,60,110,546,408/
      COEF=ZERO
      if(IMPTJJ(J1) /= IMPTJJ(J2)) return
      if(J1 < 12 .OR. J1 > 25) return
      if(K1 == 0 .AND. K2 == 0) then
        if(J1 /= J2) return
        COEF=-DSQRT(DBLE(I00(J1-11)))
      elseif(K1 == 1 .AND. K2 == 0) then
        if(J1 /= J2) return
        COEF=-DSQRT(DBLE(I10(J1-11)))
      elseif(K1 == 0 .AND. K2 == 1) then
        if(J1 /= J2) return
        COEF=-DSQRT(DBLE(I01(J1-11))/DBLE(7))
      else
        CALL RMEW7BJJ(J1,J2,K1,K2,COEF)
      endif
      return
      end subroutine RMEW7JJ

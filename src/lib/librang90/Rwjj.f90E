!*******************************************************************
!                                                                  *
      subroutine RWJJ(J,J1,J2,K1,K2,COEF)
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
      use CONS_C
      use ribojj_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rmew1jj_I
      use rmew3jj_I
      use rmew5jj_I
      use rmew7jj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: J, J1, J2, K1, K2
      real(kind=real64), intent(out) :: COEF
!-----------------------------------------------
      if(J == 1) then
         CALL RMEW1JJ(J1,J2,K1,K2,COEF)
      elseif(J == 3) then
         CALL RMEW3JJ(J1,J2,K1,K2,COEF)
      elseif(J == 5) then
         CALL RMEW5JJ(J1,J2,K1,K2,COEF)
      elseif(J == 7) then
         CALL RMEW7JJ(J1,J2,K1,K2,COEF)
!GG        elseif(J.EQ.9) then
!GG          CALL SUWJJ(K1,K2,J,J1,J2,COEF)
      else
         WRITE(0,'(A,I5)') ' KLAIDA SUB. RWJJ J=',J
         STOP
      endif
      return
      end subroutine RWJJ

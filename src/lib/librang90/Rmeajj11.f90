!*******************************************************************
!                                                                  *
      subroutine RMEAJJ11(J1,J2,LL,S)
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
      use CONS_C,          only: ZERO, HALF, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rumtjj_I
      use c0t5s_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: LL, J1, J2
      real(kind=real64), intent(out) :: S
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer      :: LQ, LV, LQS, LVS, J, J1S, N, INN
      real(kind=real64) :: A1, A4, Q, QQ, QS, QM, QMS
!-----------------------------------------------
      S=ZERO
      CALL RUMTJJ(J1,LL,LQ,LV,J)
      CALL RUMTJJ(J2,LL,LQS,LVS,J1S)
      QQ=HALF
      N=2
      Q=HALF*DBLE(LQ)
      QS=HALF*DBLE(LQS)
      QM=-HALF*DBLE(HALF*(LL+1)-N)
      QMS=-HALF*DBLE(HALF*(LL+1)-(N-1))
      CALL C0T5S(QS,QMS,QQ,Q,QM,A4)
      if(DABS(A4) < EPS) return
      A1=DBLE(N*(LQ+1)*(J+1))
      S=DSQRT(A1)/A4
      INN=-N-1
      if((INN/2)*2 /= INN)S=-S
      return
      end subroutine RMEAJJ11

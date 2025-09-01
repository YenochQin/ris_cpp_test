!*******************************************************************
!                                                                  *
      subroutine SUWJJ(K1,K2,LL,J1,J2,SUW)
!                                                                  *
!                 (k1 k2)                                          *
!     ( j QJ ::: W      ::: j QJ )                                 *
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
      use ribojj_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rumtjj_I
      use ixjtik_I
      use rmeajj_I
      use sixj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: K1, K2, J1, J2
      integer,      intent(out) :: LL
      real(kind=real64), intent(out) :: SUW
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KK1,KK2,I,IP,IG,L2Q1,L2V1,L2J1,L2Q2,L2V2,L2J2, &
                 L2QI,L2VI,L2JI
      real(kind=real64) :: COEF1, COEF2, S, SI1, SI2
!-----------------------------------------------
      SUW=ZERO
      if(IMPTJJ(J1) /= IMPTJJ(J2)) return
      S=ZERO
      CALL RUMTJJ(J1,LL,L2Q1,L2V1,L2J1)
      KK1=K1*2
      KK2=K2*2
      IP=IMPNJJ(J1)
      IG=IMGNJJ(J1)
      CALL RUMTJJ(J2,LL,L2Q2,L2V2,L2J2)
      DO I=IP,IG
        CALL RUMTJJ(I,LL,L2QI,L2VI,L2JI)
        if(IXJTIK(LL,LL,KK2,L2J2,L2J1,L2JI) /= 0) then
          if(IXJTIK(1,1,KK1,L2Q2,L2Q1,L2QI) /= 0) then
            CALL RMEAJJ(LL,J1,L2Q1,L2J1,I,L2QI,L2JI,COEF1)
            if(DABS(COEF1) > EPS) then
              CALL RMEAJJ(LL,I,L2QI,L2JI,J2,L2Q2,L2J2,COEF2)
              if(DABS(COEF2) > EPS) then
                CALL SIXJ(LL,LL,KK2,L2J2,L2J1,L2JI,0,SI1)
                CALL SIXJ(1,1,KK1,L2Q2,L2Q1,L2QI,0,SI2)
                S=S+SI1*SI2*COEF1*COEF2
              endif
            endif
          endif
        endif
      END DO
      SUW=S*DSQRT(DBLE((KK1+1)*(KK2+1)))
      if(MOD(L2Q1+L2J1+L2Q2+L2J2+KK1+KK2,4) /= 0)SUW=-SUW
      return
      end subroutine SUWJJ

!*******************************************************************
!                                                                  *
      subroutine REC3(JA1,JA2,JA3,K1,K2,K3,IRE,IAT,RECC)
!                                                                  *
!   ---------------  SECTION REC    SUBPROGRAM 07  --------------  *
!                                                                  *
!     subroutine CALLED:  RECO3                                    *
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
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reco3_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)       :: JA1,JA2,JA3,K1,K2,K3,IRE
      integer, intent(out)      :: IAT
      real(real64), intent(out) :: RECC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifAZ
!-----------------------------------------------
      if((JA3 > JA1).AND.(JA3 > JA2)) then
        if(JA1-JA2 < 0) then
          CALL RECO3(JA1,JA2,JA3,K1,K2,K3,IRE,IAT,RECC)
        else if(JA1-JA2 > 0) then
          CALL RECO3(JA2,JA1,JA3,K2,K1,K3,IRE,IAT,RECC)
          ifAZ=K1+K2-K3
          if((ifAZ/4)*4 /= ifAZ)RECC=-RECC
        else
          GO TO 10
        END if
      else if((JA3 < JA1).AND.(JA3 < JA2)) then
        if(JA1-JA2 < 0) then
          CALL RECO3(JA3,JA1,JA2,K3,K1,K2,IRE,IAT,RECC)
          if((K3/2)*2 /= K3)RECC=-RECC
        else if(JA1-JA2 > 0) then
          CALL RECO3(JA3,JA2,JA1,K3,K2,K1,IRE,IAT,RECC)
          ifAZ=K1+K2+K3
          if((ifAZ/4)*4 /= ifAZ)RECC=-RECC
        else
          GO TO 10
        END if
      else
        if(JA1-JA2 < 0)then
          CALL RECO3(JA1,JA3,JA2,K1,K3,K2,IRE,IAT,RECC)
          ifAZ=K1-K2-K3
          if((ifAZ/4)*4 /= ifAZ)RECC=-RECC
        else if(JA1-JA2 > 0) then
          CALL RECO3(JA2,JA3,JA1,K2,K3,K1,IRE,IAT,RECC)
          if((K1/2)*2 /= K1)RECC=-RECC
        else
          GO TO 10
        END if
      END if
      return
   10 WRITE(99,100)
  100 FORMAT(5X,'ERRO IN REC')
      STOP
      end subroutine REC3

!*******************************************************************
!                                                                  *
      subroutine RMEAJJ(LL,IT,LQ,J,ITS,LQS,J1S,COEF)
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
      use rmeajj11_I
      use rmeajj9_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)  :: LL, IT, LQ, J, ITS, LQS, J1S
      real(real64), intent(out) :: COEF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: I1, I2, IE1,  JT, JTS
      integer, dimension(1,2) :: IDS3, IDV3
      integer, dimension(3,3) :: IDS5, IDV5
      integer, dimension(6,8) :: IDS7, IDV7
!-----------------------------------------------
      DATA IDS3/12,20/
      DATA IDV3/1,1/
      DATA IDS5/-24,2*0,-30,120,-90,-54,-48,330/
      DATA IDV5/4*1,2*7,1,2*7/
      DATA IDS7/2*0,-40,3*0,-54,-33,-40,-65,30,0,88,-1,0,-1755,-40,  &
      0,198,-36,-72,4500,-234,-360,-78,156,0,108,-30,96,66,49,0,27,  &
      130,136,0,195,-104,-245,-624,1224,3*0,2720,-2448,-7752/
      DATA IDV7/6*1,7,2*1,7,2*1,7,2*1,77,11,1,7,11,1,77,2*11,35,5,1, &
      91,1,13,2*5,2*1,2*7,1,11,1,3*11,3*1,143,77,91/
!
      COEF=ZERO
      if(LL > 37) return
      if(LL <= 9) then
        if(IT < 300) then
          if(IMPTJJ(IT) /= IMPNJJ(ITS)) return
          if(IT < ITS) then
            JT=IT
            JTS=ITS
          else
            JT=ITS
            JTS=IT
          endif
        endif
      endif
      if(LL == 1) then
        COEF=-2
      elseif(LL == 3)then
        I1=JT-2
        I2=JTS-3
        COEF=-DSQRT(DBLE(IDS3(I1,I2))/DBLE(IDV3(I1,I2)))
      elseif(LL == 5)then
        I1=JT-5
        I2=JTS-8
        if(IDS5(I1,I2) >= 0) then
          COEF=DSQRT(DBLE(IDS5(I1,I2))/DBLE(IDV5(I1,I2)))
        else
          COEF=-DSQRT(-DBLE(IDS5(I1,I2))/DBLE(IDV5(I1,I2)))
        endif
      elseif(LL == 7)then
        I1=JT-11
        I2=JTS-17
        if(IDS7(I1,I2) >= 0) then
          COEF=DSQRT(DBLE(IDS7(I1,I2))/DBLE(IDV7(I1,I2)))
        else
          COEF=-DSQRT(-DBLE(IDS7(I1,I2))/DBLE(IDV7(I1,I2)))
        endif
      elseif(LL == 9) then
        if(IT > 300) then
          if(ITS < 300) then
            WRITE(6,'(A)') ' ERROR IN  RMEAJJ '
            STOP
          endif
          if(LL == J1S) then
            CALL RMEAJJ11(IT,ITS,LL,COEF)
          else
            CALL RMEAJJ11(ITS,IT,LL,COEF)
            IE1=LQ-LQS+J-J1S+LL-1
            if((IE1/4)*4 /= IE1)COEF=-COEF
          endif
        else
          CALL RMEAJJ9(IT,LQ,J,ITS,LQS,J1S,COEF)
          if(IT > ITS) then
            if(MOD(LQ+J-LQS-J1S+LL-1,4) /= 0) COEF=-COEF
          endif
          WRITE(0,'(A)') ' KLAIDA RMEAJJ SUB. '
          STOP
        END if
      else
        if(LL == J1S) then
          CALL RMEAJJ11(IT,ITS,LL,COEF)
        else
          CALL RMEAJJ11(ITS,IT,LL,COEF)
          IE1=LQ-LQS+J-J1S+LL-1
          if((IE1/4)*4 /= IE1)COEF=-COEF
        END if
      endif
      if(LL < 9) then
        if(IT > ITS) then
          if(MOD(LQ+J-LQS-J1S+LL-1,4) /= 0) COEF=-COEF
        endif
      endif
      return
      end subroutine RMEAJJ

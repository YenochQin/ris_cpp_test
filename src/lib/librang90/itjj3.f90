!******************************************************************
!                                                                  *
      integer FUNCTION ITJJ3(IK,ID,KG1,BK,BD,IBT,BT,ITP,ITG,IQ)
!                                                                  *
!   ---------------  SECTION SQJJ  SUBPROGRAM 08  --------------   *
!                                                                  *
!     FUNCTION CALLED: ITTK                                        *
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
      use CONS_C,          only: HALF
      use ribojj_C
      use ribojj9_C
      use ribojj11_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mes_I
      use ittk_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in)                :: KG1, IQ
      integer,      intent(out)               :: ITP, ITG
      integer,      intent(in),  dimension(7) :: IK, ID
      integer,      intent(out), dimension(7) :: IBT
      real(kind=real64), intent(in),  dimension(3) :: BK, BD
      real(kind=real64), intent(out), dimension(3) :: BT
!      dimension ID(7),IK(7),IBT(7),BT(3),BD(3),BK(3)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ITK, ITD, ITP1, ITG1
!-----------------------------------------------
      ITJJ3=0
      if(ID(3) > 37) return
      if(ITTK(ID(6),IK(6),KG1) == 0)return
      ITK=IK(1)
      ITD=ID(1)
      if(ID(3) < 9) then
        ITP1=IMPTJJ(ITK)
        ITP=IMPNJJ(ITD)
        if(ITP1 /= ITP)return
        ITG1=IMGTJJ(ITK)
        ITG=IMGNJJ(ITD)
      elseif(ID(3) == 9) then
        if(ITK > 300) then
          if(ITD < 300) CALL MES(53)
          if(ID(4) > 2) CALL MES(13)
          if(IK(4) > 2) CALL MES(13)
          ITK=ITK-300
          ITD=ITD-300
          ITP1=IMPTJJ9(ITK)
          ITP=IMPNJJ9(ITD)
          if(ITP1 /= ITP)return
          ITG1=IMGTJJ9(ITK)
          ITG=IMGNJJ9(ITD)
        else
          PRINT*, "ERROR in ITJJ3"
          STOP
        endif
      else
        if(ID(4) > 2) CALL MES(13)
        if(IK(4) > 2) CALL MES(13)
        ITP1=IMPTJJ11(ITK)
        ITP=IMPNJJ11(ITD)
        if(ITP1 /= ITP)return
        ITG1=IMGTJJ11(ITK)
        ITG=IMGNJJ11(ITD)
      endif
      if(ITG1 /= ITG)return
      ITJJ3=1
      IBT(2)=ID(2)
      IBT(3)=ID(3)
      IBT(4)=ID(4)+IQ
      BT(3)=BD(3)+HALF*DBLE(IQ)
      return
      END FUNCTION ITJJ3

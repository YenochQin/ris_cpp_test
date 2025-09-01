!*******************************************************************
!                                                                  *
      subroutine RUMTJJ(KNT,JJ,LQ,LV,L)
!                                                                  *
!   ---------------  SECTION SQJJ  SUBPROGRAM 16  --------------   *
!                                                                  *
!     NO subroutine CALLED                                         *
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
      use mtjj_C
      use mtjj2_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use jthn_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: JJ, KNT
      integer, intent(out) :: LQ, LV, L
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: KT, KNTMIN
!-----------------------------------------------

      if(JJ < 9) then
        KT=MT(KNT)
      else if(JJ == 9) then
        if(KNT > 300) then
          KNTMIN=KNT-300
          KT=MT9(KNTMIN)
        else
          PRINT*, "ERROR in RUMTJJ"
          STOP
!GG          CALL RUMT67(KNT,NR,LQ,LS,L)
!GG          return
        endif
      else
        KT=MT11(KNT)
      endif
      LQ=JTHN(KT,3,100)
      LV=JTHN(KT,2,100)
      L=JTHN(KT,1,100)
      return
      end subroutine RUMTJJ

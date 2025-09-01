!*******************************************************************
!                                                                  *
      subroutine GG12(IK1,IK2,BK1,BK2,ID1,ID2,BD1,BD2,QM1,QM2,WW)
!                                                                  *
! ----------------  SECTION METWO    SUBPROGRAM 18  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF FOLLOWING MATRIX       *
!                                                                  *
!                   N1     (j1)  N1'      N2     (j2)  N2'      +- *
!     ELEMENTS:   (j Q J::A(1)::j Q'J')*(j Q J::A(2)::j Q'J')*  -+ *
!                   1 1 1        1 1 1    2 2 2        2 2 2    ++ *
!                                                               -- *
!     subroutine CALLED: C0T5S,RMEAJJ,WJ1                          *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use CONS_C,          only: ZERO, TENTH, EPS
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mes_I
      use c0t5s_I
      use rmeajj_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,      intent(in), dimension(7) :: IK1, IK2, ID1, ID2
      real(real64), intent(in)               :: QM1, QM2
      real(real64), intent(in), dimension(3) :: BK1, BK2, BD1, BD2
      real(real64), intent(out)              :: WW
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IQMM1, IQMM2
      real(real64) :: A1, C, C1, S
!-----------------------------------------------
      WW=ZERO
      if(IK1(3) > 9) then
        if(IK1(4) > 2) CALL MES(32)
        if(ID1(4) > 2) CALL MES(32)
      endif
      if(IK2(3) > 9) then
        if(IK2(4) > 2) CALL MES(32)
        if(ID2(4) > 2) CALL MES(32)
      endif
      IQMM1=QM1+QM1+TENTH*QM1
      if(IK1(4) /= (ID1(4)+IQMM1))return
      IQMM2=QM2+QM2+TENTH*QM2
      if(IK2(4) /= (ID2(4)+IQMM2))return
      CALL C0T5S(BD1(1),BD1(3),QM1,BK1(1),BK1(3),A1)
      if(DABS(A1) < EPS)return
      CALL C0T5S(BD2(1),BD2(3),QM2,BK2(1),BK2(3),C1)
      if(DABS(C1) < EPS)return
      A1=A1*C1
      CALL RMEAJJ(IK1(3),IK1(1),IK1(7),IK1(6),ID1(1),ID1(7),ID1(6),S)
      if(DABS(S) < EPS)return
      CALL RMEAJJ(IK2(3),IK2(1),IK2(7),IK2(6),ID2(1),ID2(7),ID2(6),C)
      if(DABS(C) < EPS)return
      WW=A1*S*C/DSQRT(DBLE((IK1(7)+1)*(IK2(7)+1)))
      return
      END

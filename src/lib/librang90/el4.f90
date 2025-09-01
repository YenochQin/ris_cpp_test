!*******************************************************************
!                                                                  *
      subroutine EL4(JJA,JJB,JA,JB,JC,JD,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 09  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF TWO PARTICLE OPERATOR IN CASE :      N'1 = N1 +- 1        *
!                                             N'2 = N2 +- 1        *
!                                             N'3 = N3 -+ 2        *
!                                                                  *
!     subroutine CALLED: EL41                                      *
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
      use m_C,            only: NPEEL
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use el41_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJA,JJB,JA,JB,JC,JD,ICOLBREI
!-----------------------------------------------
      if(NPEEL <= 2)return
      if(JA == JB) then
        CALL EL41(JJA,JJB,JC,JD,JA,1,JA,JB,JC,JD,ICOLBREI)
      else if(JC == JD) then
        CALL EL41(JJA,JJB,JA,JB,JC,2,JA,JB,JC,JD,ICOLBREI)
      else
        WRITE(99,100)
        STOP
      END if
      return
  100 FORMAT(5X,'ERRO IN EL4 ')
      end subroutine EL4

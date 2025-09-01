!*******************************************************************
!                                                                  *
      subroutine EL5(JJA,JJB,JA,JB,JC,JD,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 11  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF TWO PARTICLE OPERATOR IN CASE :    N'1 = N1 (+-) 1        *
!                                           N'2 = N2 (+-) 1        *
!                                           N'3 = N3 (+-) 1        *
!                                           N'4 = N4 (+-) 1        *
!                                                                  *
!      subroutine CALLED: EL51,EL52,EL53                           *
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
      use m_C,          only: NPEEL
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use el51_I
      use el52_I
      use el53_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJA,JJB,JA,JB,JC,JD,ICOLBREI
!-----------------------------------------------
      if(NPEEL <= 3)return
      if(JB < JC) then
        CALL EL51(JJA,JJB,JA,JB,JC,JD,1,ICOLBREI)
      else if(JA > JD.AND.JB > JD) then
        CALL EL51(JJA,JJB,JC,JD,JA,JB,2,ICOLBREI)
      else if(JB > JC.AND.JB < JD.AND.JA < JC) then
        CALL EL52(JJA,JJB,JA,JC,JB,JD,1,ICOLBREI)
      else if(JB > JC.AND.JB > JD.AND.JA > JC) then
        CALL EL52(JJA,JJB,JC,JA,JD,JB,2,ICOLBREI)
      else if(JB > JC.AND.JB > JD.AND.JA < JC) then
        CALL EL53(JJA,JJB,JA,JC,JD,JB,1,ICOLBREI)
      else if(JB > JC.AND.JB < JD.AND.JA > JC) then
        CALL EL53(JJA,JJB,JC,JA,JB,JD,2,ICOLBREI)
      else
        WRITE(99,100)
        STOP
      END if
      return
  100 FORMAT(5X,'ERRO IN EL5 ')
      end subroutine EL5

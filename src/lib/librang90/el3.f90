!*******************************************************************
!                                                                  *
      subroutine EL3(JJA,JJB,JA,JB,JC,JD,ICOLBREI)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 05  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF TWO PARTICLE OPERATOR IN CASE :       N'1 = N1 - 1        *
!                                              N'2 = N2 + 1        *
!                                                                  *
!     subroutine CALLED: EL31,EL32,EL33                            *
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
      use m_C,         only:  NPEEL
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use el31_I
      use el32_I
      use el33_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JJA,JJB,JA,JB,JC,JD,ICOLBREI
!-----------------------------------------------
      if(NPEEL <= 1)return
      if(JB == JD) then
        if(JA == JB.OR.JC == JB) then
          if(JA == JC)GO TO 10
          if(JC /= JB) then
            CALL EL32(JJA,JJB,JC,JA,JA,JB,JC,JD,ICOLBREI)
          else
            CALL EL31(JJA,JJB,JC,JA,JA,JB,JC,JD,ICOLBREI)
          END if
        else
          CALL EL33(JJA,JJB,JC,JA,JB,1,JA,JB,JC,JD,ICOLBREI)
        END if
        return
      else if(JA == JC) then
        if(JB == JA.OR.JD == JA) then
          if(JB == JD)GO TO 10
          if(JD /= JA) then
            CALL EL32(JJA,JJB,JD,JB,JA,JB,JC,JD,ICOLBREI)
          else
            CALL EL31(JJA,JJB,JD,JB,JA,JB,JC,JD,ICOLBREI)
          END if
        else
          CALL EL33(JJA,JJB,JD,JB,JA,1,JA,JB,JC,JD,ICOLBREI)
        END if
        return
      else if(JA == JD) then
        if(JB == JA.OR.JC == JA) then
          if(JB == JC)GO TO 10
          if(JC /= JD) then
             CALL EL32(JJA,JJB,JC,JB,JA,JB,JC,JD,ICOLBREI)
          else
             CALL EL31(JJA,JJB,JC,JB,JA,JB,JC,JD,ICOLBREI)
          END if
        else
          CALL EL33(JJA,JJB,JC,JB,JA,2,JA,JB,JD,JC,ICOLBREI)
        END if
        return
      else if(JB == JC) then
        if(JA == JB.OR.JD == JB) then
          if(JA == JD)GO TO 10
          if(JD /= JB) then
            CALL EL32(JJA,JJB,JD,JA,JA,JB,JC,JD,ICOLBREI)
          else
            CALL EL31(JJA,JJB,JD,JA,JA,JB,JC,JD,ICOLBREI)
          END if
        else
          CALL EL33(JJA,JJB,JD,JA,JB,2,JA,JB,JD,JC,ICOLBREI)
        END if
        return
      END if
   10 WRITE(99,100)
  100 FORMAT(5X,'ERRO IN EL3  PMGG RAGG')
      STOP
      end subroutine EL3

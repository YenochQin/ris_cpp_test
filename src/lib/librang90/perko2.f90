!*******************************************************************
!                                                                  *
      subroutine PERKO2(JA1,JA2,JA3,JA4,I)
!                                                                  *
!   --------------  SECTION METWO    SUBPROGRAM 23  -------------  *
!                                                                  *
!     interface BETWEEN "GRASP" AND BOLCK "SQ"                     *
!                                               (GENERAL CASE)     *
!                                                                  *
!     subroutine CALLED: PERKO1                                    *
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
      use trk_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use perko1_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JA1, JA2, JA3, JA4, I
!-----------------------------------------------
      CALL PERKO1(JA1,BK1,IK1,BD1,ID1)
      if(I == 1)return
      CALL PERKO1(JA2,BK2,IK2,BD2,ID2)
      if(I == 2)return
      CALL PERKO1(JA3,BK3,IK3,BD3,ID3)
      if(I == 3)return
      CALL PERKO1(JA4,BK4,IK4,BD4,ID4)
      return
      end subroutine PERKO2

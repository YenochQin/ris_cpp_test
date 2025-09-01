!*******************************************************************
!                                                                  *
      subroutine EILE(JA,JB,JC,JAA,JBB,JCC)
!                                                                  *
!     ------------  SECTION METWO    SUBPROGRAM 02  ------------   *
!                                                                  *
!     NO subroutine CALLED                                         *
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: JA, JB, JC
      integer, intent(out) :: JAA, JBB, JCC
!-----------------------------------------------
      JAA=JA
      JCC=JA
      if(JAA > JB)JAA=JB
      if(JCC < JB)JCC=JB
      if(JAA > JC)JAA=JC
      if(JCC < JC)JCC=JC
      if((JA > JAA).AND.(JA < JCC))JBB=JA
      if((JB > JAA).AND.(JB < JCC))JBB=JB
      if((JC > JAA).AND.(JC < JCC))JBB=JC
      return
      end subroutine EILE

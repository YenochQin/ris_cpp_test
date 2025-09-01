!*******************************************************************
!                                                                  *
      subroutine MES(I)
!                                                                  *
!   Written by G. Gaigalas,                                        *
!   Vilnius,  Lithuania                             December 1993  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      IMPLICIT NONE
      integer, intent(in) :: I
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: J
      character(LEN=10), dimension(6) :: STRING5
!
      DATA STRING5/'   I T L S',' I T L S 2',' I T L S 3', &
                   'AW P 1 L S','WA P 1 L S','       W 1'/
!-----------------------------------------------
      if(I > 50) then
        J=I-50
        WRITE(6,'(A)') ' error in func./sub. '
        WRITE(6,'(20X,A10)') STRING5(J)
        WRITE(6,'(A)') ' susimaise f sluoksnio termu kodavimas  '
      else
        WRITE(6,'(A)') ' yra daugiau nei 2 ele. sluoks. f,g,h,i,k,l,m'
        WRITE(6,'(3X,I5)') I
        if(I == 1) then
          WRITE(6,'(A)') ' error in subroutine   W 1 G '
        elseif(I == 2) then
          WRITE(6,'(A)') ' error in subroutine   W 1 '
        elseif(I == 11) then
          WRITE(6,'(A)') ' error in Function     I T L S  '
        elseif(I == 12) then
          WRITE(6,'(A)') ' error in Function     I T L S 2  '
        elseif(I == 13) then
          WRITE(6,'(A)') ' error in Function     I T L S 3  '
        elseif(I == 30) then
          WRITE(6,'(A)') ' error in subroutine   A 1 A 2 A 3 A 4 L S '
        elseif(I == 31) then
          WRITE(6,'(A)') ' error in subroutine   A 1 A 2 L S '
        elseif(I == 32) then
          WRITE(6,'(A)') ' error in subroutine   A 1 A 2 W 3 L S '
        elseif(I == 33) then
          WRITE(6,'(A)') ' error in subroutine   A 1 A W 2 L S '
        elseif(I == 34) then
          WRITE(6,'(A)') ' error in subroutine   W A 1 A 2 L S '
        elseif(I == 35) then
          WRITE(6,'(A)') ' error in subroutine   W 1 W 2 L S '
        else
          WRITE(6,'(A)') ' error in unknown subroutine  '
        endif
      endif
      WRITE(6,'(A)') ' Contact to   G. Gediminas please ! ! ! '
      STOP
      end subroutine MES

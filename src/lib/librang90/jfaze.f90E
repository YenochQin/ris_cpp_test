!*******************************************************************
!                                                                  *
      integer FUNCTION JFAZE(I1,I2,I3,I4)
!                                                                  *
!     ------------  SECTION METWO    SUBPROGRAM 20  -------------  *
!                                                                  *
!     DETERMINATE THE PHASE FACTOR WHICH APPEAR FROM PERMUTATION   *
!     OF OPERATORS OF SECOND QUANTIZATION                          *
!                                                                  *
!     NO FUNCTION CALLED                                           *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!*******************************************************************
!
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: I1, I2, I3, I4
!-----------------------------------------------
      JFAZE=1
      if(I1 > I2)JFAZE=-JFAZE
      if(I1 > I3)JFAZE=-JFAZE
      if(I1 > I4)JFAZE=-JFAZE
      if(I2 > I3)JFAZE=-JFAZE
      if(I2 > I4)JFAZE=-JFAZE
      if(I3 > I4)JFAZE=-JFAZE
      return
      END FUNCTION JFAZE

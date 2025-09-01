!*******************************************************************
!                                                                  *
      integer FUNCTION NMTEJJ(I2Q,I2J,J,NK,ND)
!                                                                  *
!     ------------  SECTION METWO    SUBPROGRAM 21  -------------  *
!                                                                  *
!     NO FUNCTION CALLED                                           *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!                                                                  *
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use mtjj_C
      use mtjj2_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: I2Q, I2J, J, NK, ND
!      dimension LP(9),LG(9),LP3(27),LG3(27)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer                :: I2V, INN, IL, JP, JG, JJ
      integer, dimension(9)  :: LP, LG
      integer, dimension(27) :: LP3, LG3
!-----------------------------------------------
      DATA LP/1,0,3,0,6,0,12,0,26/
      DATA LG/2,0,5,0,11,0,25,0,63/
      DATA LP3/1,0,8,0,16,0,25,0,35,0,46,0,58,0,71,0,85,0,100,0, &
      116,0,133,0,151,0,170/
      DATA LG3/7,0,15,0,24,0,34,0,45,0,57,0,70,0,84,0,99,0,115,0,&
      132,0,150,0,169,0,189/
      NMTEJJ=0
      if(J > 37)return
      I2V=J+1-2*I2Q
      INN=(I2Q*100+I2V)*100+I2J
      if(J < 9) then
         JP=LP(J)
         if(JP == 0)return
         JG=LG(J)
         if(JG == 0)return
         JJ=JP
         DO WHILE (JJ <= JG)
            if (INN == MT(JJ)) then
               NMTEJJ=JJ
               return
            END if
            JJ=JJ+1
         END DO
         return
      elseif(J == 9) then
        if(MAX0(NK,ND) < 3) then
          JP=1
          JG=6
          JJ=JP
          DO WHILE (JJ <= JG)
             if (INN == MT9(JJ)) then
                NMTEJJ=JJ+300
                return
             END if
             JJ=JJ+1
          END DO
          return
        else
          PRINT*, "ERROR in FUNCTION NMTEJJ"
          STOP
        END if
      else
        IL=J-10
        JP=LP3(IL)
        JG=LG3(IL)
        JJ=JP
        DO WHILE (JJ <= JG)
           if (INN == MT11(JJ)) then
              NMTEJJ=JJ
              return
           END if
           JJ=JJ+1
        END DO
        return
      endif
      return
      END FUNCTION NMTEJJ

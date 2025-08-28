!*******************************************************************
!                                                                  *
      INTEGER FUNCTION NMTEJJ(I2Q,I2J,J,NK,ND)
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
      USE mtjj_C
      USE mtjj2_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER, INTENT(IN) :: I2Q, I2J, J, NK, ND
!      DIMENSION LP(9),LG(9),LP3(27),LG3(27)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER                :: I2V, INN, IL, JP, JG, JJ
      INTEGER, DIMENSION(9)  :: LP, LG
      INTEGER, DIMENSION(27) :: LP3, LG3
!-----------------------------------------------
      DATA LP/1,0,3,0,6,0,12,0,26/
      DATA LG/2,0,5,0,11,0,25,0,63/
      DATA LP3/1,0,8,0,16,0,25,0,35,0,46,0,58,0,71,0,85,0,100,0, &
      116,0,133,0,151,0,170/
      DATA LG3/7,0,15,0,24,0,34,0,45,0,57,0,70,0,84,0,99,0,115,0,&
      132,0,150,0,169,0,189/
      NMTEJJ=0
      IF(J > 37)RETURN
      I2V=J+1-2*I2Q
      INN=(I2Q*100+I2V)*100+I2J
      IF(J < 9) THEN
         JP=LP(J)
         IF(JP == 0)RETURN
         JG=LG(J)
         IF(JG == 0)RETURN
         JJ=JP
         DO WHILE (JJ <= JG)
            IF (INN == MT(JJ)) THEN
               NMTEJJ=JJ
               RETURN
            END IF
            JJ=JJ+1
         END DO
         RETURN
      ELSEIF(J == 9) THEN
        IF(MAX0(NK,ND) < 3) THEN
          JP=1
          JG=6
          JJ=JP
          DO WHILE (JJ <= JG)
             IF (INN == MT9(JJ)) THEN
                NMTEJJ=JJ+300
                RETURN
             END IF
             JJ=JJ+1
          END DO
          RETURN
        ELSE
          PRINT*, "ERROR in FUNCTION NMTEJJ"
          STOP
        END IF
      ELSE
        IL=J-10
        JP=LP3(IL)
        JG=LG3(IL)
        JJ=JP
        DO WHILE (JJ <= JG)
           IF (INN == MT11(JJ)) THEN
              NMTEJJ=JJ
              RETURN
           END IF
           JJ=JJ+1
        END DO
        RETURN
      ENDIF
      RETURN
      END FUNCTION NMTEJJ

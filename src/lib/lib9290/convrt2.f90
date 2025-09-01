!***********************************************************************
!                                                                      *
      subroutine CONVRT2(INTNUM, CNUM, LENTH, FROM)
!                                                                      *
!   Converts the  integer number  INTNUM  into the  character string   *
!   CNUM of length LENTH. integer lengths of up to 64 bits are acco-   *
!   modated.                                                           *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 22 Sep 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:54   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use IOUNIT_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: INTNUM
      integer, intent(out) :: LENTH
      character, INTENT(INOUT) :: CNUM*(*)
      character, intent(in) :: FROM*(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      character :: FORM*6
      character, dimension(0:10) :: C1020*2
      character, dimension(9) :: C19
!
!
      DATA C19/ '1', '2', '3', '4', '5', '6', '7', '8', '9'/
      DATA C1020/ '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', &
         '20'/
!
      if (INTNUM < 0) then
         LENTH = LOG10(DBLE((-INTNUM))) + 2
      else if (INTNUM == 0) then
         LENTH = 1
      else
         LENTH = LOG10(DBLE(INTNUM)) + 1
      endif
!
!   Ensure that the length of CNUM as dimensioned is adequate;
!   stop with an error message if it isn't
!
      if (LENTH > LEN(CNUM)) then
         WRITE (ISTDE, *) 'CONVRT: Length of CNUM inadeuate. (from:', FROM, ')'
         ERROR STOP
      else
         if (LENTH <= 9) then
            FORM = '(1I'//C19(LENTH)//')'
            WRITE (CNUM(1:LENTH), FORM(1:5)) INTNUM
         else
            FORM = '(1I'//C1020(LENTH-10)//')'
            WRITE (CNUM(1:LENTH), FORM(1:6)) INTNUM
         endif
      endif
!
      return
      end subroutine CONVRT2

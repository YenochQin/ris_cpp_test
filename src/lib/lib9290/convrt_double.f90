!***********************************************************************
!                                                                      *
      subroutine CONVRT_DOUBLE(INTNUM, CNUM, LENTH)
!                                                                      *
!   Converts the  integer number  INTNUM  into the  character string   *
!   CNUM of length LENTH. integer lengths of up to 64 bits are acco-   *
!   modated.                                                           *
!                                                                      *
!   Written by G. Gaigalas,                                 May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E 10:46:53 2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!====================================================================

!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,   intent(in)    :: INTNUM
      integer,   intent(out)   :: LENTH
      character, INTENT(INOUT) :: CNUM*(*)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer                           :: INTNUMGG
      character(LEN=6)                  :: FORM
      character(LEN=2), dimension(0:10) :: C1020
      character, dimension(9)           :: C19
!
      DATA C19 /'1','2','3','4','5','6','7','8','9'/
      DATA C1020 /'10','11','12','13','14','15','16','17','18','19','20'/
!-----------------------------------------------
      if(mod(INTNUM,2) == 0) then
         INTNUMGG = INTNUM/2
      else
         INTNUMGG = INTNUM
      endif
!
      if (INTNUMGG < 0) then
         LENTH = LOG10(DBLE((-INTNUMGG))) + 2
      else if (INTNUMGG == 0) then
         LENTH = 1
      else
         LENTH = LOG10(DBLE(INTNUMGG)) + 1
      endif
!
!   Ensure that the length of CNUM as dimensioned is adequate;
!   stop with an error message if it isn't
!
      if (LENTH > LEN(CNUM)) then
         WRITE (6, *) 'CONVRT_DOUBLE: Length of CNUM inadeuate.'
         ERROR STOP
      else
         if (LENTH <= 9) then
            FORM = '(1I'//C19(LENTH)//')'
            WRITE (CNUM(1:LENTH), FORM(1:5)) INTNUMGG
         else
            FORM = '(1I'//C1020(LENTH-10)//')'
            WRITE (CNUM(1:LENTH), FORM(1:6)) INTNUMGG
         endif
         if(mod(INTNUM,2) /= 0) then
            if (LENTH+2 > LEN(CNUM)) then
               WRITE (6, *) 'CONVRT_DOUBLE: Length of CNUM inadeuate.'
               ERROR STOP
            else
               CNUM(1:LENTH+2) = CNUM(1:LENTH)//'/2'
               LENTH = LENTH + 2
            endif
         endif
      endif
      return
      end subroutine CONVRT_DOUBLE

!***********************************************************************
!                                                                      *
!                                                                      *
!   Checks if the angular files name.IOB (one-body) or                 *
!   name.ITB (Two-body) are available                                  *
!                                                                      *
!                                                                      *
!   Modified by C. Naz\'e  Oct. 2011                                   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use orb_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character (LEN = 24), intent(in) :: NAME
      logical, intent(out) :: AVAIL
      integer, intent(in)  :: WHICHONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      logical :: FOUND
      integer :: J, NF, istat, IWORD
!-----------------------------------------------
!
      J = INDEX(NAME,' ')
      if (WHICHONE.EQ.1) then
          NF = 50
          INQUIRE (FILE = NAME(1:J-1)//'.IOB', EXIST = FOUND)
      elseif (WHICHONE.EQ.2) then
          NF = 51
          INQUIRE (FILE = NAME(1:J-1)//'.ITB', EXIST = FOUND)
      else
         print*,'This message should never appear'
      endif

      if (.NOT.FOUND) then
      if (WHICHONE.EQ.1) PRINT *, ' One-body angular file not available'
      if (WHICHONE.EQ.2) PRINT *, ' Two-body angular file not available'
        AVAIL = .FALSE.
        return
      else
        if (WHICHONE.EQ.1) then
          OPEN(UNIT=NF,FILE = NAME(1:J-1)//'.IOB',STATUS='UNKNOWN'     &
            ,POSITION='APPEND',FORM='UNFORMATTED')
        else
          OPEN(UNIT=NF,FILE = NAME(1:J-1)//'.ITB',STATUS='UNKNOWN'     &
            ,POSITION='APPEND',FORM='UNFORMATTED')
        endif
        BACKSPACE (UNIT = NF,IOSTAT=istat)
        READ(NF) IWORD
        REWIND(UNIT = NF)
        if (IWORD.EQ.-1) then
          if (WHICHONE.EQ.1) PRINT *, ' One-body angular file available'
          if (WHICHONE.EQ.2) PRINT *, ' Two-body angular file available'
          AVAIL = .TRUE.
        else
          AVAIL = .FALSE.
          if (WHICHONE.EQ.1) PRINT *,' One-body angular file incomplete'
          if (WHICHONE.EQ.2) PRINT *,' Two-body angular file incomplete'
        endif
      endif
      return
      end subroutine ANGDATA

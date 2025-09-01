!***********************************************************************
!                                                                      *
      subroutine SETRWFA(NAME)
!                                                                      *
!   Open, check, load data from and close the  .rwf  file.             *
!                                                                      *
!   Call(s) to: [LIB92]: LODRWF, OPENFL.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 06 Oct 1992   *
!   Modified    by Xinghong He            Last revision: 09 Jul 1998   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:42   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use IOUNIT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use openfl_I
      use lodrwf_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character (LEN = *), intent(in) :: NAME
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: IERR, IOS
      character :: G92RWF*6


      CALL OPENFL (23, NAME, 'UNFORMATTED', 'OLD', IERR)
      if (IERR == 1) then
         WRITE (ISTDE, *) 'Error when opening', NAME(1:LEN_TRIM(NAME))
         STOP
      endif
!
!   Check the file; if not as expected, stop.
!
      READ (23, IOSTAT=IOS) G92RWF
      if (IOS/=0 .OR. G92RWF/='G92RWF') then
         WRITE (ISTDE, *) 'This is not a Radial WaveFunction File;'
         CLOSE(23)
         STOP
      endif
!
!   Attempt to load the radial wavefunctions; if this fails, stop
!
      CALL LODRWF (IERR)

      if (IERR /= 0) then
         WRITE (ISTDE, *) 'Radial wavefunctions defined in CSL file', &
            ' not found in Radial WaveFunction File'
         CLOSE(23)
         STOP
      endif

      CLOSE(23)

      return
      end subroutine SETRWFA

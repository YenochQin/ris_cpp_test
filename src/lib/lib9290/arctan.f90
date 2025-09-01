!***********************************************************************
!                                                                      *
      real(kind=real64) FUNCTION ARCTAN (ARG1, ARG2)
!-----------------------------------------------
!                                                                      *
!                   -1                                                 *
!       ARCTAN = tan   (ARG1/ARG2),      0 .LE. ARCTAN .LT. 2*\pi      *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 06 Oct 1992   *
!   Editted by C. Froese Fischer after translation                     *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:34   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use DEF_C
!
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(kind=real64), intent(in) :: ARG1
      real(kind=real64), intent(in) :: ARG2
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      logical, SAVE :: FIRST = .TRUE., INTRIN = .TRUE.
!-----------------------------------------------
!
!
!   Determine whether the FORTRAN intrinsic function ATAN2 always
!   returns a positive value
!
      if (FIRST) then
         ARCTAN = ATAN2(-1.0D00,-1.0D00)
         if (ARCTAN > 0.0D00) then
            INTRIN = .TRUE.
         else
            INTRIN = .FALSE.
         endif
         FIRST = .FALSE.
      endif
!
!   Use the intrinsic function if it passes the above test; otherwise
!   add 2*PI to the negative values returned by the intrinsic function
!
      if (INTRIN) then
         ARCTAN = ATAN2(ARG1,ARG2)
      else
         if (ARG1 >= 0.0D00) then
            ARCTAN = ATAN2(ARG1,ARG2)
         else
            ARCTAN = PI + PI + ATAN2(ARG1,ARG2)
         endif
      endif
!
      return
      END FUNCTION ARCTAN

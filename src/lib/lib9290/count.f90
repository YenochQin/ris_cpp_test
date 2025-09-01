!***********************************************************************
!                                                                      *
      subroutine COUNT(FR, MTPFR, NNCFF, SGN)
!                                                                      *
!   This subroutine counts the nodes in the radial function FR using   *
!   the criteria  given by C Froese Fischer, Comp Phys Rep, 3 (1986)   *
!   314-315 . The  function FR is assumed defined on the first MTPFR   *
!   points of the radial grid. The sign of the function at the first   *
!   oscillation is also determined.                                    *
!                                                                      *
!   Written by Farid A Parpia, at Oxford    Last update: 08 Dec 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:46:58   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------                       *
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNP
      use COUN_C
      use DEF_C, only: ACCY
      use GRID_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: MTPFR
      integer, intent(out) :: NNCFF
      real(real64), intent(out) :: SGN
      real(real64), dimension(NNNP), intent(in) :: FR
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, dimension(NNNP) :: LCEXT
      integer :: NEXT, I, LOC, NSTPS
      real(real64) :: EXT, EMX, ABFRI, TEST, THRESE, ABLCL
!-----------------------------------------------
!
!
!   (1) Find all extrema in FR
!   (2) Find the maximum amplitudes of FR
!
      NEXT = 1
      EXT = 0.0D00
      LCEXT(1) = 1
      EMX = 0.0D00
      DO I = 2, MTPFR
         ABFRI = ABS(FR(I))
         TEST = ABS(SIGN(1.0D00,FR(I))+SIGN(1.0D00,FR(I-1)))
         if (TEST <= ACCY) then
            NEXT = NEXT + 1
            LCEXT(NEXT) = 0
            EXT = 0.0D00
         endif
         if (ABFRI > EXT) then
            EXT = ABFRI
            LCEXT(NEXT) = I
         endif
         if (ABFRI <= EMX) CYCLE
         EMX = ABFRI
      END DO
!
!   Eliminate oscillations with amplitude less than THRESH times
!   the maximum
!
      LOC = 0
      THRESE = THRESH*EMX
    4 CONTINUE
      LOC = LOC + 1
      if (LOC <= NEXT) then
         if (LCEXT(LOC) == 0) then
            ABLCL = 0.0D00
         else
            ABLCL = ABS(FR(LCEXT(LOC)))
         endif
         if (ABLCL < THRESE) then
            NEXT = NEXT - 1
            NSTPS = NEXT - LOC
            LCEXT(LOC:NSTPS+LOC) = LCEXT(LOC+1:NSTPS+1+LOC)
            LOC = LOC - 1
         endif
         GO TO 4
      endif
!
!   Count changes of sign using the remaining oscillations
!
      NNCFF = 0
      DO I = 2, NEXT
         TEST = ABS(SIGN(1.0D00,FR(LCEXT(I)))+SIGN(1.0D00,FR(LCEXT(I-1))))
         if (TEST > ACCY) CYCLE
         NNCFF = NNCFF + 1
      END DO
!
!   Determine the position of the first oscillation, and the
!   sign of the function at this location
!
      SGN = SIGN(1.0D00,FR(LCEXT(1)))
!
      return
      end subroutine COUNT

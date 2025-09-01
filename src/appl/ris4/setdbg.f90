!***********************************************************************
!                                                                      *
      subroutine SETDBG
!-----------------------------------------------
!                                                                      *
!   This subroutine sets the arrays that control debug printout from   *
!   the radial and angular modules of the GRASP92 suite.               *
!                                                                      *
!   Call(s) to: [LIB92]: GETYN, LENGTH, OPENFL.                        *
!                                                                      *
!   Written by Farid A Parpia               Last update: 24 Dec 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use debug_C
      use default_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use getyn_I
      use openfl_I
      use iso_fortran_env, only: real64
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer   :: I, IERR
      logical   :: YES
      character :: FILNAM*256, DEFNAM*11, FORM*11, STATUS*3
!-----------------------------------------------
!
!   Initialise the arrays that control the debug printout
!
      LDBPA = .FALSE.
!
      LDBPG = .FALSE.
!
      LDBPR = .FALSE.
!
      if (NDEF == 0) return

      WRITE (6, *) 'Generate debug printout?'
      YES = GETYN()
      if (YES) then
!
!   The  .dbg  file is formatted; open it on unit 99
!
         DEFNAM = 'rdensity.dbg'
         FORM = 'FORMATTED'
         STATUS = 'NEW'
!
         WRITE (6, *) 'File  rdensity.dbg  will be created as the'
         WRITE (6, *) ' rdensity DeBuG Printout File; enter another'
         WRITE (6, *) ' file name if this is not acceptable;'
         WRITE (6, *) ' null otherwise:'
         READ (*, '(A)') FILNAM
!
         if (LEN_TRIM(FILNAM) == 0) FILNAM = DEFNAM
!
    4    CONTINUE
         CALL OPENFL (99, FILNAM, FORM, STATUS, IERR)
         if (IERR /= 0) then
    5       CONTINUE
            WRITE (6, *) 'Enter a name for the rdensity DeBuG Printout'
            WRITE (6, *) ' file that is to be created:'
            READ (*, '(A)') FILNAM
            if (LEN_TRIM(FILNAM) == 0) GO TO 5
            GO TO 4
         endif
!
!   Set options for general printout
!
         WRITE (6, *) ' Print out the machine constants used?'
         YES = GETYN()
         if (YES) LDBPG(1) = .TRUE.
         WRITE (6, *) ' Print out the physical constants used?'
         YES = GETYN()
         if (YES) LDBPG(2) = .TRUE.
!
!   Set options for radial modules
!
         WRITE (6, *) ' Printout from radial modules?'
         YES = GETYN()
         if (YES) then
            WRITE (6, *) ' Printout from RADGRD?'
            YES = GETYN()
            if (YES) LDBPR(1) = .TRUE.
            WRITE (6, *) ' Printout from NUCPOT?'
            YES = GETYN()
            if (YES) LDBPR(2) = .TRUE.
            WRITE (6, *) ' Printout from LODRWF?'
            YES = GETYN()
            if (YES) LDBPR(3) = .TRUE.
!
         endif
!
!   Set options for angular modules
!
         WRITE (6, *) ' Printout from angular modules?'
         YES = GETYN()
         if (YES) then
            WRITE (6, *) ' Printout from LODCSL?'
            YES = GETYN()
            if (YES) LDBPA(1) = .TRUE.
            WRITE (6, *) ' Print out V coefficients?'
            YES = GETYN()
            if (YES) LDBPA(2) = .TRUE.
         endif
!
      endif
!
      return
      end subroutine SETDBG

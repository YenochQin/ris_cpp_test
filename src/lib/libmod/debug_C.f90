!
!***********************************************************************
!                                                                      *
      MODULE debug_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  14:35:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6
      LOGICAL, DIMENSION(5) :: LDBPA
      LOGICAL, DIMENSION(5) :: LDBPG
      LOGICAL, DIMENSION(30) :: LDBPR
      real(real64) :: cutoff  ! used by bioscl
      END MODULE debug_C

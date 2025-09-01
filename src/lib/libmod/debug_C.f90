!
!***********************************************************************
!                                                                      *
      module debug_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  14:35:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: IBUG1, IBUG2, IBUG3, IBUG4, IBUG5, IBUG6
      logical, dimension(5) :: LDBPA
      logical, dimension(5) :: LDBPG
      logical, dimension(30) :: LDBPR
      real(kind=real64) :: cutoff  ! used by bioscl
      end module debug_C

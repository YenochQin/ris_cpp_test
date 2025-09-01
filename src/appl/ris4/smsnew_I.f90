      module smsnew_I
      use iso_fortran_env, only: real64
      interface
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine smsnew (DOIT,VINT,VINT2)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only: NNNW
      real(kind=real64), dimension(NNNW,NNNW), intent(in) :: VINT, VINT2
      integer, intent(in) :: DOIT
      end subroutine
      end interface
      end module

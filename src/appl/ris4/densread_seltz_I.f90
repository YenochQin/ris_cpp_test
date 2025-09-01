      module densread_seltz_I
      use iso_fortran_env, only: real64
      interface
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine densread_seltz (DINT1,DINT2,DINT3,                    &
                           DINT4,DINT5,DINT6,                          &
                           DINT7,DINT1VEC,DENS1VEC,NRNUC)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNW, NNNP
      use prnt_C,           only: NVEC
      real(kind=real64), dimension(NNNW,NNNW), intent(in) :: DINT1, DINT2,  &
                                          DINT3, DINT4, DINT5, DINT6,  &
                                          DINT7
      integer, intent(in) :: NRNUC
      real(kind=real64), dimension(NVEC,NRNUC), intent(out)     :: DENS1VEC
      real(kind=real64), dimension(NNNW,NNNW,NRNUC), intent(in) :: DINT1VEC
      end subroutine
      end interface
      end module

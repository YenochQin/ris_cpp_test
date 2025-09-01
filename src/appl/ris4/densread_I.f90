      module densread_I
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine densread (DINT1,DINT2,DINT3,                          &
                           DINT4,DINT5,DINT6,                          &
                           DINT7)
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNW, NNNP
      use prnt_C,           only: NVEC
      real(kind=real64), dimension(NNNW,NNNW), intent(in) :: DINT1, DINT2,  &
                                          DINT3, DINT4, DINT5, DINT6,  &
                                          DINT7
      end subroutine
      end interface
      end module

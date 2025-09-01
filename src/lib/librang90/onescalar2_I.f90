      module onescalar2_I
      interface
!
      subroutine ONESCALAR2(JJA,JJB,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in) :: JJA,JJB,JA,JB
      real(real64), intent(out) :: COEFF
      end subroutine
      end interface
      end module

      module oneparticlejj1_I
      interface
!
      subroutine ONEPARTICLEJJ1(NS,KA,JJA,JJB,JA,JB,COEFF)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in) :: NS,KA,JJA,JJB,JA,JB
      real(kind=real64), intent(out) :: COEFF
      end subroutine
      end interface
      end module

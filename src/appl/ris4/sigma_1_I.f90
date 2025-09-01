      module sigma_1_I
      interface
      subroutine sigma_1 (IPAR,I1,I2,APART)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in) :: IPAR, I1, I2
      real(real64), intent(out) :: APART
      end subroutine sigma_1
      end interface
      end module

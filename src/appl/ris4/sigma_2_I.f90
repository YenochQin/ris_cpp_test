      MODULE sigma_2_I
      INTERFACE
      SUBROUTINE sigma_2 (IPAR,I1,I2,APART)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER, INTENT(IN) :: IPAR, I1, I2
      real(real64), INTENT(OUT) :: APART
      END SUBROUTINE sigma_2
      END INTERFACE
      END MODULE

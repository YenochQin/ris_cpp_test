      module perko2_I
      interface
      subroutine PERKO2(JA1,JA2,JA3,JA4,I)
      use iso_fortran_env, only: real64, int32, int64, real128
      use trk_C
      integer, intent(in) :: JA1, JA2, JA3, JA4, I
      end subroutine
      end interface
      end module

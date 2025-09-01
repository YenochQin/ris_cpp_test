      module recop00_I
      interface
!
      subroutine RECOP00(NS,JA1,JA2,KA,IAT)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)  :: NS, JA1, JA2, KA
      integer, intent(out) :: IAT
      end subroutine
      end interface
      end module

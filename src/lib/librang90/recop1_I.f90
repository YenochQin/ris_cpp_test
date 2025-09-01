      module recop1_I
      interface
!
      subroutine RECOP1(NS,JA1,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)       :: NS, JA1, KA, IRE
      integer, intent(out)      :: IAT
      real(kind=real64), intent(out) :: RECC
      end subroutine
      end interface
      end module

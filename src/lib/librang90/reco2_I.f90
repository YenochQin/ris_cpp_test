      module reco2_I
      interface
!
      subroutine RECO2(JA1,JA2,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)       :: JA1,JA2,KA,IRE
      integer, intent(out)      :: IAT
      real(real64), intent(out) :: RECC
      end subroutine
      end interface
      end module

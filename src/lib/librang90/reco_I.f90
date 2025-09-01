      module reco_I
      interface
!
      subroutine RECO(JA1,JA2,JA3,JA4,KA,IAT)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)  :: JA1, JA2, JA3, JA4, KA
      integer, intent(out) :: IAT
      end subroutine
      end interface
      end module

      module rec3_I
      interface
!
      subroutine REC3(JA1,JA2,JA3,K1,K2,K3,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)       :: JA1,JA2,JA3,K1,K2,K3,IRE
      integer, intent(out)      :: IAT
      real(kind=real64), intent(out) :: RECC
      end subroutine
      end interface
      end module

      module diaga5_I
      interface
!
      subroutine DIAGA5(NPEELGG,JA1,KA,IRE,IAT,RECC)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in)       :: NPEELGG,JA1,KA,IRE
!      integer, intent(out)      :: IAT
      integer, INTENT(INOUT)      :: IAT
      real(real64), intent(out) :: RECC
      end subroutine
      end interface
      end module

      module coulom_I
      interface
!
      subroutine COULOM(J1,J2,J3,J4,L1,L2,L3,L4,K,AA)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer, intent(in) :: J1, J2, J3, J4, L1, L2, L3, L4, K
      real(real64),intent(out)  :: AA
      end subroutine
      end interface
      end module

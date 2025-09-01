      module gracah1_I
      interface
!
      subroutine GRACAH1(I,J,K,L,M,N,RAC)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer MFACT
      parameter (MFACT = 500)
      integer , intent(in) :: I
      integer , intent(in) :: J
      integer , intent(in) :: K
      integer , intent(in) :: L
      integer , intent(in) :: M
      integer , intent(in) :: N
      real(real64) , intent(out) :: RAC
      end subroutine
      end interface
      end module

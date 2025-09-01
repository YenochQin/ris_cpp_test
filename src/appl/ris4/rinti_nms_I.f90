      module rinti_nms_I
      use iso_fortran_env, only: real64
      interface
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine rinti_nms (J, K, VALNMSK123,VALNMSK1)
      use iso_fortran_env, only: real64, int32, int64, real128
      real(kind=real64), intent(out) :: VALNMSK123, VALNMSK1
      integer, intent(in) :: J
      integer, intent(in) :: K
      end subroutine
      end interface
      end module

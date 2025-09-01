      module rkco_GG_I
      interface
!
      subroutine RKCO_GG (JA,JB,CORD,INCOR,ICOLBREI)
      use iso_fortran_env, only: real64, int32, int64, real128
      EXTERNAL CORD
      integer, intent(in) :: JA,JB,INCOR,ICOLBREI
      end subroutine
      end interface
      end module

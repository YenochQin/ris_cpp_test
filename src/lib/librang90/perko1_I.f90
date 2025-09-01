      module perko1_I
      interface
!
      subroutine PERKO1(JA,BK,IK,BD,ID)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in)                :: JA
      integer,      intent(out), dimension(7) :: IK, ID
      real(real64), intent(out), dimension(3) :: BK, BD
      end subroutine
      end interface
      end module

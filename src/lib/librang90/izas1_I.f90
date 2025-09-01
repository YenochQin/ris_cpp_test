      module izas1_I
      interface
!
      integer FUNCTION IZAS1(IB,QB,IK,QK)
      use iso_fortran_env, only: real64, int32, int64, real128
      integer,      intent(in) :: IB, IK
      real(kind=real64), intent(in) :: QB, QK
      END FUNCTION
      end interface
      end module

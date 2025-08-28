      MODULE izas1_I
      INTERFACE
!
      INTEGER FUNCTION IZAS1(IB,QB,IK,QK)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN) :: IB, IK
      real(real64), INTENT(IN) :: QB, QK
      END FUNCTION
      END INTERFACE
      END MODULE

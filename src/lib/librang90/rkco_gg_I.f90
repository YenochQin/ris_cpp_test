      MODULE rkco_GG_I
      INTERFACE
!
      SUBROUTINE RKCO_GG (JA,JB,CORD,INCOR,ICOLBREI)
      use iso_fortran_env, only: real64, int32, int64, real128
      EXTERNAL CORD
      INTEGER, INTENT(IN) :: JA,JB,INCOR,ICOLBREI
      END SUBROUTINE
      END INTERFACE
      END MODULE

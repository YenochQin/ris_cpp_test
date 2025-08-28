      MODULE itjj3_I
      INTERFACE
!
      INTEGER FUNCTION ITJJ3(IK,ID,KG1,BK,BD,IBT,BT,ITP,ITG,IQ)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)                :: KG1, IQ
      INTEGER,      INTENT(OUT)               :: ITP, ITG
      INTEGER,      INTENT(IN),  DIMENSION(7) :: IK, ID
      INTEGER,      INTENT(OUT), DIMENSION(7) :: IBT
      real(real64), INTENT(IN),  DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT), DIMENSION(3) :: BT
      END FUNCTION
      END INTERFACE
      END MODULE

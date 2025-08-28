      MODULE itjj_I
      INTERFACE
!
      INTEGER FUNCTION ITJJ(IK,ID,KG,BK,BD,IBT,BT,KG1,ITP,ITG,IQ)
      use iso_fortran_env, only: real64, int32, int64, real128
      INTEGER,      INTENT(IN)                :: KG, KG1, IQ
      INTEGER,      INTENT(OUT)               :: ITP, ITG
      INTEGER,      INTENT(IN),  DIMENSION(7) :: IK, ID
      INTEGER,      INTENT(OUT), DIMENSION(7) :: IBT
      real(real64), INTENT(IN),  DIMENSION(3) :: BK, BD
      real(real64), INTENT(OUT), DIMENSION(3) :: BT
      END FUNCTION
      END INTERFACE
      END MODULE

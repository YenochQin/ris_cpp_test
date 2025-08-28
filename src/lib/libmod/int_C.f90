      MODULE int_C
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNP
!...Created by Pacific-Sierra Research 77to90  4.3E  06:37:37  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: MTP0
      real(real64), DIMENSION(NNNP) :: P, Q
      real(real64) :: P0, Q0
      real(real64), DIMENSION(NNNP) :: TF, TG, XU, XV
      END MODULE int_C

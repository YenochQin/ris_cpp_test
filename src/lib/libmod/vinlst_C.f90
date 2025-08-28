      MODULE vinlst_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:35:13  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NDVIN, NVINTI
      INTEGER, DIMENSION(:), pointer :: indtei
      real(real64), DIMENSION(:), pointer :: valtei
      LOGICAL :: FRSTVI
      END MODULE vinlst_C

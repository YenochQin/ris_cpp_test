      MODULE vpilst_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:35:13  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NDVPA, NVPI
      INTEGER, DIMENSION(:), pointer :: indvpi
      real(real64), DIMENSION(:), pointer :: valvpi
      LOGICAL :: FRSTVP
      END MODULE vpilst_C

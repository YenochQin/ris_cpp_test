      MODULE mcpdata_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:25:32  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NCOEFF, NINTG
      INTEGER, DIMENSION(:), pointer :: jann, jbnn, intgrl, intptr
      real(real64), DIMENSION(:), pointer :: cnn
      END MODULE mcpdata_C

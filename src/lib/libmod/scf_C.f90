      MODULE scf_C
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,    ONLY: NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:38:40  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(real64), DIMENSION(NNNW) :: UCF
      INTEGER, DIMENSION(NNNW) :: METHOD
      real(real64), DIMENSION(NNNW) :: SCNSTY
      real(real64) :: EPSMIN, EPSMAX, EMIN, EMAX, ZINF
      INTEGER :: NDCOF, NXCOF, NYCOF, NDDIM, NXDIM, NYDIM
      real(real64), DIMENSION(:), pointer :: da, xa, ya
      INTEGER, DIMENSION(:), pointer :: nda, nxa, nya
      END MODULE scf_C

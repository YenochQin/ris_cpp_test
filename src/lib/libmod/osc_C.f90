      MODULE osc_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  07:26:50   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NSDIM
      real(real64), DIMENSION(:), pointer :: hb1, hb2, hc1, hc2, hm1, hm2
      INTEGER, DIMENSION(:), pointer :: jja, jjb
      INTEGER :: LK, KK
      INTEGER :: NTDIM
      real(real64), DIMENSION(:), pointer :: xsldr, totc, totb
      INTEGER, DIMENSION(:), pointer :: isldr, isldr1
      INTEGER :: NINT, NINTEG
      INTEGER, DIMENSION(:), pointer :: nptr, lab
      INTEGER :: NKP
      INTEGER, DIMENSION(:), pointer :: kp
      LOGICAL, DIMENSION(10) :: LTC
      END MODULE osc_C

      MODULE hmat_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:33:54  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer(int64) :: NELMNT
      real(real64), DIMENSION(:), pointer :: emt
      INTEGER, DIMENSION(:), pointer :: iendc
      INTEGER, DIMENSION(:), pointer :: irow
      INTEGER, DIMENSION(6) :: NTPITMP
      integer(int64) :: NELMNTTMP
      INTEGER :: NCOEITMP, NCOECTMP, NCTEITMP, NCTECTMP, NMCBPTMP,  &
                 NCORETMP, NVPITMP, NKEITMP, NVINTITMP, NCFTMP
      real(real64) :: CUTOFFTMP
      END MODULE hmat_C

      module hmat_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:33:54  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer(int64) :: NELMNT
      real(real64), dimension(:), pointer :: emt
      integer, dimension(:), pointer :: iendc
      integer, dimension(:), pointer :: irow
      integer, dimension(6) :: NTPITMP
      integer(int64) :: NELMNTTMP
      integer :: NCOEITMP, NCOECTMP, NCTEITMP, NCTECTMP, NMCBPTMP,  &
                 NCORETMP, NVPITMP, NKEITMP, NVINTITMP, NCFTMP
      real(real64) :: CUTOFFTMP
      end module hmat_C

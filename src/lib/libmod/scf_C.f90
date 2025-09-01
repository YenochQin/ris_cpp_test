      module scf_C
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:38:40  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(kind=real64), dimension(NNNW) :: UCF
      integer, dimension(NNNW) :: METHOD
      real(kind=real64), dimension(NNNW) :: SCNSTY
      real(kind=real64) :: EPSMIN, EPSMAX, EMIN, EMAX, ZINF
      integer :: NDCOF, NXCOF, NYCOF, NDDIM, NXDIM, NYDIM
      real(kind=real64), dimension(:), pointer :: da, xa, ya
      integer, dimension(:), pointer :: nda, nxa, nya
      end module scf_C

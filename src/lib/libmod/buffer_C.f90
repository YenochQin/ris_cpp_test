      module buffer_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  13:03:28   1/25/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: NBDIM, NVCOEF
      integer, dimension(:,:), pointer :: label
      real(kind=real64), dimension(:), pointer :: coeff
      end module buffer_C

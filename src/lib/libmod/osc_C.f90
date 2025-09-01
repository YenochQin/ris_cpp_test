      module osc_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  07:26:50   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: NSDIM
      real(kind=real64), dimension(:), pointer :: hb1, hb2, hc1, hc2, hm1, hm2
      integer, dimension(:), pointer :: jja, jjb
      integer :: LK, KK
      integer :: NTDIM
      real(kind=real64), dimension(:), pointer :: xsldr, totc, totb
      integer, dimension(:), pointer :: isldr, isldr1
      integer :: NINT, NINTEG
      integer, dimension(:), pointer :: nptr, lab
      integer :: NKP
      integer, dimension(:), pointer :: kp
      logical, dimension(10) :: LTC
      end module osc_C

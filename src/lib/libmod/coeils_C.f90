      module coeils_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:33:54  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: NDCOEA, NCOEI
      integer, dimension(:), pointer :: indoei
      real(kind=real64), dimension(:), pointer :: valoei
      logical :: FRSTCO
      end module coeils_C

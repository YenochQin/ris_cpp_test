!
!***********************************************************************
!                                                                      *
      module prnt_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  07:38:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: NVEC, NVECMX
      integer :: NVECFF, NVECMXFF
      integer :: NVECII, NVECMXII
      real(kind=real64) :: PNIVECII
      integer, dimension(:), pointer :: ivec, ivecff, ivecii
      end module prnt_C

!
!***********************************************************************
!                                                                      *
      module blk_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  14:35:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer, dimension(20) :: NCFBLK, NEVINBLK, NCFINBLK, TWO_J
      integer :: NBLOCK
      real(real64), dimension(:), pointer :: IDXBLK
      integer :: NELECTOT, NCFTOT, NWTOT, NVECTOT, NVECSIZTOT, NBLOCK1
      integer :: NBLOCKI
      integer :: NBLOCKF
      integer, dimension(10) :: NCFI
      integer, dimension(10) :: NCFF
      end module blk_C

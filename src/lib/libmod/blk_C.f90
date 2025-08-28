!
!***********************************************************************
!                                                                      *
      MODULE blk_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  14:35:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER, DIMENSION(20) :: NCFBLK, NEVINBLK, NCFINBLK, TWO_J
      INTEGER :: NBLOCK
      real(real64), DIMENSION(:), pointer :: IDXBLK
      INTEGER :: NELECTOT, NCFTOT, NWTOT, NVECTOT, NVECSIZTOT, NBLOCK1
      INTEGER :: NBLOCKI
      INTEGER :: NBLOCKF
      INTEGER, DIMENSION(10) :: NCFI
      INTEGER, DIMENSION(10) :: NCFF
      END MODULE blk_C

      MODULE hblock_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:29:39  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NBLOCK  !! NCFBLK as an integer needs to be renamed
!     real(real64) :: PNCFBLK
!     real(real64) :: PNEVBLK, PNCMAXBLK
      INTEGER, DIMENSION(:), pointer :: ncfblk  !! this is a problem
      INTEGER, DIMENSION(:), pointer :: nevblk, ncmaxblk
      END MODULE hblock_C

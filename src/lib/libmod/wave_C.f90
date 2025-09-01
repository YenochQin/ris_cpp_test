      module wave_C
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  07:38:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer, dimension(NNNW) :: MF
      real(real64), dimension(NNNW) :: PZ
      integer, dimension(NNNW) :: MFFF
      real(real64), dimension(NNNW) :: PZFF
      integer, dimension(NNNW) :: MFII
      real(real64), dimension(NNNW) :: PZII
      real(real64), dimension(:,:), pointer :: PF,QF
      real(real64), dimension(:,:), pointer :: pfff,qfff,pfii,qfii
      end module wave_C

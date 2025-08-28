      MODULE wave_C
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY:  NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  07:38:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER, DIMENSION(NNNW) :: MF
      real(real64), DIMENSION(NNNW) :: PZ
      INTEGER, DIMENSION(NNNW) :: MFFF
      real(real64), DIMENSION(NNNW) :: PZFF
      INTEGER, DIMENSION(NNNW) :: MFII
      real(real64), DIMENSION(NNNW) :: PZII
      real(real64), DIMENSION(:,:), pointer :: PF,QF
      real(real64), DIMENSION(:,:), pointer :: pfff,qfff,pfii,qfii
      END MODULE wave_C

!
!***********************************************************************
!                                                                      *
      MODULE orb_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, int8, real128
      USE parameter_def,   ONLY:  NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  08:57:22  12/25/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17

      CHARACTER(LEN=2), DIMENSION(NNNW) :: NH
      CHARACTER(LEN=2), DIMENSION(NNNW) :: NHR
      real(real64), DIMENSION(NNNW) :: E, GAMA, PED
      INTEGER :: NCF, NW, NCFR, NWR
      integer(int8), DIMENSION(:,:), pointer :: IQA
      INTEGER, DIMENSION(:,:), pointer :: IQAR
      INTEGER, DIMENSION(NNNW) :: NP, NAK, NPR, NAKR
      INTEGER, DIMENSION(NNNW) :: NKL, NKJ
      END MODULE orb_C

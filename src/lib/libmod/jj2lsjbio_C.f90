!
!***********************************************************************
!                                                                      *
      MODULE jj2lsjbio_C
!                                                                      *
!                                                                      *
!     Written by G. Gaigalas,                                          *
!     NIST                                     last update: May 2017   *
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!
      real(real64), DIMENSION(:), pointer :: RLev_ENER_1,RLev_ENER_2
      CHARACTER(LEN=64), POINTER, DIMENSION(:) :: string_CSF1
      CHARACTER(LEN=64), POINTER, DIMENSION(:) :: string_CSF2
      INTEGER :: NVECTOTI,NVECTOTF,IOPEN_STATUS1,IOPEN_STATUS2
      INTEGER, DIMENSION(:), pointer :: Lev_POS_1,Lev_J_1,Lev_Par_1
      INTEGER, DIMENSION(:), pointer :: Lev_POS_2,Lev_J_2,Lev_Par_2
!
      END MODULE jj2lsjbio_C

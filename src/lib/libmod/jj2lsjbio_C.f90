!
!***********************************************************************
!                                                                      *
      module jj2lsjbio_C
!                                                                      *
!                                                                      *
!     Written by G. Gaigalas,                                          *
!     NIST                                     last update: May 2017   *
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!
      real(kind=real64), dimension(:), pointer :: RLev_ENER_1,RLev_ENER_2
      character(LEN=64), pointer, dimension(:) :: string_CSF1
      character(LEN=64), pointer, dimension(:) :: string_CSF2
      integer :: NVECTOTI,NVECTOTF,IOPEN_STATUS1,IOPEN_STATUS2
      integer, dimension(:), pointer :: Lev_POS_1,Lev_J_1,Lev_Par_1
      integer, dimension(:), pointer :: Lev_POS_2,Lev_J_2,Lev_Par_2
!
      end module jj2lsjbio_C

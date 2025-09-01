!
!***********************************************************************
!                                                                      *
      module orb_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, int8, real128
      use parameter_def,   only:  NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  08:57:22  12/25/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17

      character(LEN=2), dimension(NNNW) :: NH
      character(LEN=2), dimension(NNNW) :: NHR
      real(kind=real64), dimension(NNNW) :: E, GAMA, PED
      integer :: NCF, NW, NCFR, NWR
      integer(int8), dimension(:,:), pointer :: IQA
      integer, dimension(:,:), pointer :: IQAR
      integer, dimension(NNNW) :: NP, NAK, NPR, NAKR
      integer, dimension(NNNW) :: NKL, NKJ
      end module orb_C

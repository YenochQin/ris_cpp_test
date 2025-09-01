!
!***********************************************************************
!                                                                      *
      module sacoef_C
!                                                                      *
!                                                                      *
!     Written by G. Gaigalas,                                          *
!     NIST                                     last update: May 2017   *
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!
      real(real64), dimension(:,:), pointer :: TCOEFF
      integer, dimension(:,:), pointer :: IICLMN, IINDEX, ILABEL
!
      end module sacoef_C

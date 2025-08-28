!
!***********************************************************************
!                                                                      *
      MODULE eigv_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  07:38:02   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(real64) :: EAV, EAVFF, EAVII
      real(real64), DIMENSION(:), pointer :: EVAL, EVALFF, EVALII
      real(real64), DIMENSION(:), pointer :: EVEC, EVECFF, EVECII
      END MODULE eigv_C

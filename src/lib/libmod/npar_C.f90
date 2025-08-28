!
!***********************************************************************
!                                                                      *
      MODULE npar_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:23:52  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER :: NPARM
      real(real64), DIMENSION(2) :: PARM
      END MODULE npar_C

      MODULE nsmdat_C
      use iso_fortran_env, only: real64
!...Created by Pacific-Sierra Research 77to90  4.3E  06:36:34  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(real64) :: SQN,DMOMNM,QMOMB
      real(real64) :: HFSI, HFSD, HFSQ
      real(real64) :: SMSI, SMSD, SMSQ
      END MODULE nsmdat_C

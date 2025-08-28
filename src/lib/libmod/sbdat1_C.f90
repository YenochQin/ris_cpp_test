      MODULE sbdat1_C
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY: NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:25:32  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER, PARAMETER :: NLMAX = 20
      INTEGER, DIMENSION(NLMAX,NLMAX) :: NSHLP
      INTEGER, DIMENSION(NLMAX,NNNW) :: NSHLPP
      END MODULE sbdat1_C

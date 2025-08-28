!
!***********************************************************************
!                                                                      *
      MODULE def_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY:  NNNP
!...Created by Pacific-Sierra Research 77to90  4.3E  11:02:52   1/ 2/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(real64) :: TENMAX, EXPMAX, EXPMIN, PRECIS
      real(real64) :: AUCM, AUEV, CCMS, FASI, FBSI
      real(real64) :: FMTOAU, AUMAMU, B1
      INTEGER :: IONCTY, NELEC
      real(real64) :: EMN, Z
      INTEGER :: IONCTYFF, NELECFF
      real(real64) :: EMNFF, ZFF
      INTEGER :: IONCTYII, NELECII
      real(real64) :: EMNII, ZII
      INTEGER :: NELECR
      real(real64) :: C
      real(real64) :: EMPAM, RBCM
      INTEGER :: NSCF, NSIC, NSOLV
      real(real64) :: ACCY
      real(real64), DIMENSION(:), pointer :: wt, weight
      INTEGER :: NCMIN, NCMAX
      INTEGER, DIMENSION(:), pointer :: iccmin
      real(real64) :: CVAC, PI
      real(real64), DIMENSION(NNNP) :: DP, DQ
      END MODULE def_C

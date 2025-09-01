!
!***********************************************************************
!                                                                      *
      module def_C
!                                                                      *
!***********************************************************************
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  NNNP
!...Created by Pacific-Sierra Research 77to90  4.3E  11:02:52   1/ 2/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      real(kind=real64) :: TENMAX, EXPMAX, EXPMIN, PRECIS
      real(kind=real64) :: AUCM, AUEV, CCMS, FASI, FBSI
      real(kind=real64) :: FMTOAU, AUMAMU, B1
      integer :: IONCTY, NELEC
      real(kind=real64) :: EMN, Z
      integer :: IONCTYFF, NELECFF
      real(kind=real64) :: EMNFF, ZFF
      integer :: IONCTYII, NELECII
      real(kind=real64) :: EMNII, ZII
      integer :: NELECR
      real(kind=real64) :: C
      real(kind=real64) :: EMPAM, RBCM
      integer :: NSCF, NSIC, NSOLV
      real(kind=real64) :: ACCY
      real(kind=real64), dimension(:), pointer :: wt, weight
      integer :: NCMIN, NCMAX
      integer, dimension(:), pointer :: iccmin
      real(kind=real64) :: CVAC, PI
      real(kind=real64), dimension(NNNP) :: DP, DQ
      end module def_C

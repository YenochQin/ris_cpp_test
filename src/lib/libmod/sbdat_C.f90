      module sbdat_C
      use parameter_def,   only: NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:27:59  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer, parameter :: NLMAX = 20
      integer, dimension(NNNW) :: NAKINVII
      integer, dimension(NLMAX) :: NSHLII
      integer, dimension(NLMAX,NLMAX) :: NSHLPII
      integer, dimension(NNNW) :: NAKINVFF
      integer, dimension(NLMAX) :: NSHLFF
      integer, dimension(NLMAX,NLMAX) :: NSHLPFF
      integer, dimension(NLMAX,NNNW) :: NSHLPPII, NSHLPPFF
      integer, dimension(NLMAX) :: NINII, NINFF, IKAPPA
      integer :: KAMAX
      end module sbdat_C

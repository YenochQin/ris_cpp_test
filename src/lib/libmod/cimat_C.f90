      module cimat_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:25:32  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer, parameter :: NLMAX = 20
      real(kind=real64), dimension(20*NLMAX*NLMAX) :: CFCI
      real(kind=real64), dimension(20*NLMAX*NLMAX) :: CICI
      end module cimat_C

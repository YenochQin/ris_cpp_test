      module int_C
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: NNNP
!...Created by Pacific-Sierra Research 77to90  4.3E  06:37:37  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: MTP0
      real(real64), dimension(NNNP) :: P, Q
      real(real64) :: P0, Q0
      real(real64), dimension(NNNP) :: TF, TG, XU, XV
      end module int_C

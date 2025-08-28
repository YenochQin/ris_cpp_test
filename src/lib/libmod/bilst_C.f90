      MODULE bilst_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:33:54  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      INTEGER, DIMENSION(6) :: NDTPA, NTPI
      INTEGER, DIMENSION(:), pointer :: indtp1, indtp2, indtp3, indtp4, &
                                        indtp5, indtp6
      real(real64), DIMENSION(:), pointer :: valtp1, valtp2, valtp3, valtp4, &
                                        valtp5, valtp6
      LOGICAL, DIMENSION(6) :: FIRST
      END MODULE bilst_C

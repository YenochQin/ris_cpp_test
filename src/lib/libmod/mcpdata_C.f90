      module mcpdata_C
      use iso_fortran_env, only: real64, int32, int64, real128
!...Created by Pacific-Sierra Research 77to90  4.3E  06:25:32  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      integer :: NCOEFF, NINTG
      integer, dimension(:), pointer :: jann, jbnn, intgrl, intptr
      real(kind=real64), dimension(:), pointer :: cnn
      end module mcpdata_C

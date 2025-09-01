      module angdata_I
  use iso_fortran_env, only: real64
      interface
!...Translated by Gediminas Gaigalas 11/18/19
      subroutine angdata (NAME,AVAIL,WHICHONE)
      character (LEN = 24), intent(in) :: NAME
      logical, intent(out) :: AVAIL
      integer, intent(in)  :: WHICHONE
      end subroutine
      end interface
      end module

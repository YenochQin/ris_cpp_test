      MODULE biorb_C
      use iso_fortran_env, only: real64, int32, int64, real128
      USE parameter_def,   ONLY: NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:22:09  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17

      CHARACTER, DIMENSION(NNNW) :: NHFF*2, NHII*2
      real(real64), DIMENSION(NNNW) :: EFF, GAMAFF , EII, GAMAII
      INTEGER :: NCFFF, NWFF, NCFII, NWII
      INTEGER, DIMENSION(NNNW) :: NPFF, NAKFF, NPII, NAKII
      INTEGER, DIMENSION(NNNW) :: NKLFF, NKJFF, NKLII, NKJII
      INTEGER, DIMENSION(NNNW) :: NNFF, NNII
      END MODULE biorb_C

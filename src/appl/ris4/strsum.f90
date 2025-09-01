!***********************************************************************
!                                                                      *
      use iso_fortran_env, only: real64
      subroutine STRSUM
!                                                                      *
!   Generates the first part of  grasp92.sum  (on stream 24).          *
!                                                                      *
!   Call(s) to: [LIB92] CALEN, convrt2.                                *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 09 Dec 1992   *
!   Modified by Xinghong He               Last revision: 22 Dec 1997   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      Use decide_C
      use def_C
      use grid_C
      use npar_C
      use npot_C
      use orb_C
      use prnt_C
      use wave_C
      use wfac_C
      use iounit_C
      use syma_C
      use npar_C
      use eigv_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use convrt2_I
      use engout_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer   :: LENTH, NB, ICCUT, I, IEND, IBEG
      character :: RECORD*256, CDATA*26, CLEVEL*2
!-----------------------------------------------
!
!   Get the date and time of day; make this information the
!   header of the summary file
!
!   Write out the basic dimensions of the electron cloud
!
      WRITE (24, *)
      CALL CONVRT2 (NELEC, RECORD, LENTH, 'strsum.nelec')
      WRITE (24, *) 'There are '//RECORD(1:LENTH)//' electrons in the cloud'
      CALL CONVRT2 (NCF, RECORD, LENTH, 'strsum.ncf')
      WRITE (24, *) ' in '//RECORD(1:LENTH)//' relativistic CSFs'
      CALL CONVRT2 (NW, RECORD, LENTH, 'strsum.nw')
      WRITE (24, *) ' based on '//RECORD(1:LENTH)//' relativistic subshells.'
!
!   If the CSFs are not treated uniformly, write out an
!   informative message
!
      if (LFORDR) then
         CALL CONVRT2 (ICCUT, RECORD, LENTH, 'strsum.icccut')
         WRITE (24, *) ' CSFs 1--'//RECORD(1:LENTH)//' constitute'//   &
              ' the zero-order space;'
      endif
!
!   Write out the nuclear parameters
!
      WRITE (24, *)
      WRITE (24, 300) Z
      if (NPARM == 2) then
         WRITE (24, *) ' Fermi nucleus:'
         WRITE (24, 301) PARM(1), PARM(2)
         CALL CONVRT2 (NNUC, RECORD, LENTH, 'strsum.nnuc')
         WRITE (24, *) ' there are '//RECORD(1:LENTH)//&
            ' tabulation points in the nucleus.'
      else
         WRITE (24, *) ' point nucleus.'
      endif
!
!   Write out the physical effects specifications
!
      WRITE (24, *)
      WRITE (24, 305) C
!
!   Write out the parameters of the radial grid
!
      WRITE (24, *)
      if (HP == 0.0D00) then
         WRITE (24, 306) RNT, H, N
      else
         WRITE (24, 307) RNT, H, HP, N
      endif
      WRITE (24, 308) R(1), R(2), R(N)
!
!   Write out the orbital properties
!
      WRITE (24, *)
      WRITE (24, *) 'Subshell radial wavefunction summary:'
      WRITE (24, *)
      WRITE (24, 309)
      WRITE (24, *)
      DO I = 1, NW
         WRITE (24, 310) NP(I), NH(I), E(I), PZ(I), GAMA(I), PF(2,I), QF(2,I), &
            MF(I)
      END DO
!
!   Write the list of eigenpair indices
!
      WRITE (24,*)
      CALL ENGOUT (EAV,EVAL,IATJPO,IASPAR,IVEC,NVEC,3)
!
      return
!
  300 FORMAT ('The atomic number is ',1F14.10,';')
  301 FORMAT ('  c =',1P,1D19.12,' Bohr radii,'                        &
             /'  a =',   1D19.12,' Bohr radii;')
  305 FORMAT ('Speed of light = ',1PD19.12,' atomic units.')
  306 FORMAT ( 'Radial grid: R(I) = RNT*(exp((I-1)*H)-1),',            &
               ' I = 1, ..., N;'                                       &
             //' RNT  = ',1P,D19.12,' Bohr radii;'                     &
              /' H    = ',   D19.12,' Bohr radii;'                     &
              /' N    = ',1I4,';')
  307 FORMAT ( 'Radial grid: ln(R(I)/RNT+1)+(H/HP)*R(I) = (I-1)*H,',   &
               ' I = 1, ..., N;'                                       &
             //' RNT  = ',1P,D19.12,' Bohr radii;'                     &
              /' H    = ',   D19.12,' Bohr radii;'                     &
              /' HP   = ',   D19.12,' Bohr radii;'                     &
              /' N    = ',1I4,';')
  308 FORMAT ( ' R(1) = ',1P,1D19.12,' Bohr radii;'                    &
              /' R(2) = ',   1D19.12,' Bohr radii;'                    &
              /' R(N) = ',   1D19.12,' Bohr radii.')
  309 FORMAT (' Subshell',11X,'e',20X,'p0',18X,                        &
              'gamma',19X,'P(2)',18X,'Q(2)',10X,'MTP')
  310 FORMAT (3X,1I2,1A2,1X,1P,5(3X,1D19.12),3X,1I3)
!
      end subroutine STRSUM

!***********************************************************************
!                                                                      *
      subroutine LODRWF(IIERR)
!                                                                      *
!   This subroutine loads  radial wavefunctions from the  .rwf  file   *
!   and performs some related setup.                                   *
!                                                                      *
!   Call(s) to: [LIB92]: ALLOC, DALLOC, INTRPQ, ORTHSC.                *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 05 Oct 1992   *
!   Block version by Xinghong He          Last revision: 27 May 1997   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:24:43   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only: NNNP
      use memory_man
      use DEBUG_C
      use DEF_C,           only: C, Z
      use GRID_C
      use NPAR_C
      use ORB_C
      use WAVE_C,          only: PZ, PF,QF
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use intrpq_I
      use orthsc_I
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(out) :: IIERR
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: J, I, K, NWIN, IOS, NPY, NAKY, MY
      integer :: ierr
      real(real64) :: CON, FKK, EY, PZY, DNORM
      real(real64), dimension(:), pointer :: PA, QA, RA
!-----------------------------------------------
!
!
!   Write entry message
!
      WRITE (6, *) 'Loading Radial WaveFunction File ...'
!
!   Allocate storage to orbital arrays
!
      CALL ALLOC(PF, NNNP, NW, 'PF', 'LODRWF')
      CALL ALLOC(QF, NNNP, NW, 'QF', 'LODRWF')

!
!   Setup: (1) Orbital arrays to zero
!          (2) Array E to -1 (no orbitals estimated)
!          (3) Parameters GAMMA for each orbital
!
      CON = Z/C
      CON = CON*CON
!
      DO J = 1, NW
!
         PF(:N,J) = 0.0D00
         QF(:N,J) = 0.0D00
!
         E(J) = -1.0D00
!
         K = ABS(NAK(J))
         if (NPARM > 0) then
            GAMA(J) = DBLE(K)
         else if (NPARM == 0) then
            FKK = DBLE(K*K)
            if (FKK >= CON) then
               GAMA(J) = SQRT(FKK - CON)
            else
               !WRITE (istde,*) 'LODRWF: Imaginary gamma parameter'
               !WRITE (istde,*) ' for ',NP(J),NH(J),' orbital; the'
               !WRITE (istde,*) ' point model for the nucleus'
               !WRITE (istde,*) ' is inappropriate for Z > ',C,'.'
               STOP 'lodrwf: Inappropriate gamma'
            endif
         endif
!
      END DO
!
!   Read orbital information from Read Orbitals File; write summary
!   to  .dbg  file if option set
!
      if (LDBPR(3)) WRITE (99, 300)
      NWIN = 0
    3 CONTINUE
      READ (23, IOSTAT=IOS) NPY, NAKY, EY, MY

      if (IOS == 0) then
         CALL ALLOC (PA,MY, 'PA', 'LODRWF')
         CALL ALLOC (QA,MY, 'QA', 'LODRWF')
         CALL ALLOC (RA,MY, 'RA', 'LODRWF')

         READ (23) PZY, (PA(I),I=1,MY), (QA(I),I=1,MY)
         READ (23) (RA(I),I=1,MY)

         DO J = 1, NW
            if (.NOT.(E(J)<0.0D00 .AND. NPY==NP(J) .AND. NAKY==NAK(J))) CYCLE
            PZ(J) = PZY
            E(J) = EY
            CALL INTRPQ(PA, QA, MY, RA, J, DNORM)
            if (LDBPR(3)) WRITE (99, 301) NP(J), NH(J), E(J), DNORM
            NWIN = NWIN + 1
         END DO
         CALL DALLOC (PA, 'PA', 'LODRWF' )
         CALL DALLOC (QA, 'QA', 'LODRWF')
         CALL DALLOC (RA, 'RA', 'LODRWF')
         GO TO 3
      endif
      if (LDBPR(3)) WRITE (99, *) ' orbitals renormalised;'
!
!   Stop with an error message if all orbitals are not known
!
      if (NWIN < NW) then
         IIERR = 1
         GO TO 5
      endif
!
!   Schmidt orthogonalise the orbitals
!
      CALL ORTHSC
      if (LDBPR(3)) WRITE (99, *) ' orbitals orthogonalised and renormalised;'
!
      IIERR = 0
    5 CONTINUE
      return
!
  300 FORMAT(/,'From subroutine LODRWF:'/,' Orbital',8X,'Eigenvalue',19X,'Norm')
  301 FORMAT(2X,I2,A2,4X,1P,1D22.15,4X,1D22.15)
      return
!
      end subroutine LODRWF

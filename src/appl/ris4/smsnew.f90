!***********************************************************************
!                                                                      *
      subroutine SMSNEW(DOIT,VINT,VINT2)
!                                                                      *
!   This routine controls the main sequence of routine calls for the   *
!   calculation  of the  sms parameter, the electron density at the    *
!   origin.
!                                                                      *
!   Call(s) to: [LIB92]: ALCBUF, CONVRT, GETYN                         *
!                        ITJPO,                                        *
!               [SMS92]: RINTISO, RINTDENS, VINTI                      *
!               [LIBRANG]: RKCO_GG                                     *
!                                                                      *
!   Written by Per Jonsson                                             *
!                                                                      *
!                                         Last revision: 10 Nov 1995   *
!                                                                      *
!   Modified by C. Naz\'e  Mai. 2012                                   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: KEYORB, NNNW
      use debug_C
      use prnt_C
      use orb_C
      use BUFFER_C
      use eigv_C
      use ris_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use alcbuf_I
      use itjpo_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(real64), dimension(NNNW,NNNW), intent(in) :: VINT, VINT2
      integer, intent(in) :: DOIT
!-----------------------------------------------
!  E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      EXTERNAL     :: CORD
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: KEY = KEYORB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      character*11 :: CNUM
      character*4  :: JLBL,LABJ,LABP
      character*2  :: CK
      logical      :: GETYN,FIRSTT,VSH,NUCDE,SMSSH,YES
      logical      :: LFORDR,LTRANS,LVP,LSE,LNMS,LSMS
      real(real64) :: CONTRI, CONTRIK1, VCOEFF
      integer      :: I,J,K,LAB,LOC,IC,IR,ITJPOC,IIA,IIB,IIC,IID,INCOR
      integer      :: LCNUM
!-----------------------------------------------
!
      INCOR = 1
!
!   Allocate storage for the arrays in BUFFER
!
      CALL ALCBUF (1)
!
!   Sweep through the Hamiltonian matrix to determine the
!   sms parameter
!
      DO 13 IC = 1,NCF
!
!   Output IC on the screen to show how far the calculation has preceede
!
        CALL CONVRT (IC,CNUM,LCNUM)
        if (mod(IC,100).eq.0) then
          PRINT *, 'Column '//CNUM(1:LCNUM)//' complete;'
        end if
!
        ITJPOC = ITJPO (IC)
        DO 12 IR = IC,NCF
!
!   Call the MCP package to generate V coefficients; ac and bd
!   are the density pairs
!
!   Initialize
!
!
!   Matrix elements are diagonal in J
!
          if (ITJPO(IR) .EQ. ITJPOC) then
            NVCOEF = 0
!GG            CALL RKCO (IC,IR,COR,CORD,INCOR)
            CALL RKCO_GG (IC, IR, CORD, INCOR, 1)
!
            DO 11 I = 1,NVCOEF
              VCOEFF = COEFF(I)
              if (ABS (VCOEFF) .GT. CUTOFF) then
                IIA = LABEL(1,I)
                IIB = LABEL(2,I)
                IIC = LABEL(3,I)
                IID = LABEL(4,I)
                K   = LABEL(5,I)
!
!   Only K = 1   LABEL(5,I) .EQ. 1
!
                if (LABEL(5,I) .EQ. 1) then
                  if (LDBPA(2)) then
                    WRITE (99,309) K,IC,IR,                            &
                    NP(IIA),NH(IIA),NP(IIB),NH(IIB),                   &
                    NP(IIC),NH(IIC),NP(IID),NH(IID),VCOEFF
                  endif
                  if(DOIT.EQ.1) WRITE(51) IC,IR
!** Storage sequence
                  LAB  = ((IIA*KEY + IIC)*KEY+IIB)*KEY+IID
                  if(DOIT.EQ.1) then
                    WRITE(51) VCOEFF,LAB
                  endif
!***
                  DO 10 J = 1,NVEC
                    LOC = (J-1)*NCF
                    CONTRIK1 = - EVEC(IC+LOC)*EVEC(IR+LOC)             &
                           * VCOEFF                                    &
                           * VINT (LABEL(1,I),LABEL(3,I))              &
                           * VINT (LABEL(2,I),LABEL(4,I))
                    CONTRI = - EVEC(IC+LOC)*EVEC(IR+LOC)               &
                           * VCOEFF                                    &
                           * ( VINT2(LABEL(1,I),LABEL(3,I))            &
                           * VINT (LABEL(2,I),LABEL(4,I))              &
                           + VINT2(LABEL(2,I),LABEL(4,I))              &
                           * VINT (LABEL(1,I),LABEL(3,I)) )/2.0D00
                    if (IR.NE.IC) then
                      CONTRI = 2.0D00 * CONTRI
                      CONTRIK1 = 2.0D00 * CONTRIK1
                    endif
                    SMSC1(J) = SMSC1(J) + CONTRIK1
                    SMSC2(J) = SMSC2(J) + CONTRI
   10             CONTINUE
                endif
              endif
   11       CONTINUE
          endif
   12   CONTINUE
   13 CONTINUE
      if(DOIT.EQ.1) WRITE(51) -1
!
! Empty the buffer and close file
      if(DOIT.EQ.1) CLOSE(51)
!
!   Deallocate storage for the arrays in BUFFER
!
      CALL ALCBUF (3)
      return
  309 FORMAT (' V^[(',1I2,')]_[',1I3,',',1I3,']',                      &
         ' (',1I2,1A2,',',1I2,1A2,';',                                 &
              1I2,1A2,',',1I2,1A2,') = ',1PD19.12)
!
      end subroutine SMSNEW

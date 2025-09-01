!***********************************************************************
!                                                                      *
!JE   subroutine DENSREAD(DINT1,DINT2,DINT3,DINT4,DINT5,DINT6,DINT7)
!                                                                      *
!   if angular coefficients already exist                              *
!   This routine controls combines the radial and angular parts for the*
!   calculation of the NMS parameter, the electron density at the      *
!   origin and radial expectation values.                              *
!                                                                      *
!   Written by C\'edric Naz\'e                                         *
!                                                                      *
!                                         Last revision: Feb. 2011     *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,    only: KEYORB, NNNW, NNNP
      use prnt_C
      use ris_C
      use orb_C
      use eigv_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: NRNUC
      real(kind=real64), dimension(NNNW,NNNW), intent(in) :: DINT1, DINT2,  &
                                          DINT3, DINT4, DINT5, DINT6,  &
                                          DINT7
      real(kind=real64), dimension(NVEC,NRNUC), intent(out)     :: DENS1VEC !  JE ADD
      real(kind=real64), dimension(NNNW,NNNW,NRNUC), intent(in) :: DINT1VEC !  JE ADD
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: KEY = KEYORB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(kind=real64), dimension(NNNW) :: TSHELL_R
      real(kind=real64), dimension(NRNUC) :: CONTRI1VEC, ELEMNT1VEC
      real(kind=real64) :: ELEMNT1, ELEMNT2, ELEMNT3, ELEMNT4, ELEMNT5
      real(kind=real64) :: ELEMNT6, ELEMNT7
      real(kind=real64) :: CONTRI1, CONTRI2, CONTRI3, CONTRI4, CONTRI5
      real(kind=real64) :: CONTRI6, CONTRI7
      integer :: ICOLD, IROLD, IOS, IA, IB, IC, IR, I, J, L, LOC, LAB
      integer :: NCOUNT
!-----------------------------------------------
!
! DINT1 contain the density
! DINT2 contain the uncorrected NMS parameter: K^1_NMS
! DINT3 contain the expect. value <r>
! DINT4 contain the expect. value <r2>
! DINT5 contain the expect. value <r-1>
! DINT6 contain the expect. value <r-2>
! DINT7 contain the sum of NMS parameters: K^1_NMS+K^2_NMS+K^3_NMS
!
      DENS1VEC(:,:) = 0.0D00                                     ! JE ADD

      ICOLD = 0
      IROLD = 0

      REWIND (50)
   16 READ (50,IOSTAT = IOS) IC,IR,NCOUNT

      if (IOS .EQ. 0) then
!*      print*, 'ic', IC,IR,NCOUNT
!
!   Read successful; decode the labels of I(ab)
!
!      Initialise
        if ((IC.NE.ICOLD).OR.(IR.NE.IROLD)) then
!*            ISPARC = ISPAR (IC)
!*            ITJPOC = ITJPO (IC)
!*            ITJPOR = ITJPO (IR)
!*            IDifF = ITJPOC - ITJPOR
            ICOLD = IC
            IROLD = IR
!*        DO I = 1,NELMNT
          ELEMNT1 = 0.0D00
          ELEMNT2 = 0.0D00
          ELEMNT3 = 0.0D00
          ELEMNT4 = 0.0D00
          ELEMNT5 = 0.0D00
          ELEMNT6 = 0.0D00
          ELEMNT7 = 0.0D00
          ELEMNT1VEC(:) = 0.0D00                               ! JE ADD
!*        ENDDO
        endif
        DO I= 1,NCOUNT
           READ(50) TSHELL_R(I),LAB
           IA = LAB/KEY
           IB = MOD(LAB,KEY)
!*           LOC = INDEX(I)
           ELEMNT1 = ELEMNT1 + DINT1(IA,IB)*TSHELL_R(I)
           DO 21 L = 2,NRNUC                                                  ! JE ADD
              ELEMNT1VEC(L) = ELEMNT1VEC(L) + DINT1VEC(IA,IB,L)*       &     ! JE ADD
                   TSHELL_R(I)                                               ! JE ADD
   21      CONTINUE                                                          ! JE ADD
           ELEMNT2 = ELEMNT2 + DINT2(IA,IB)*TSHELL_R(I)
           ELEMNT3 = ELEMNT3 + DINT3(IA,IB)*TSHELL_R(I)
           ELEMNT4 = ELEMNT4 + DINT4(IA,IB)*TSHELL_R(I)
           ELEMNT5 = ELEMNT5 + DINT5(IA,IB)*TSHELL_R(I)
           ELEMNT6 = ELEMNT6 + DINT6(IA,IB)*TSHELL_R(I)
           ELEMNT7 = ELEMNT7 + DINT7(IA,IB)*TSHELL_R(I)
        ENDDO
!
!   Return to the start of the loop
!

!*      ICI = 0
!*      DO 21 I = 1,NELMNT
!*        IRI = IROW(I)
!*         if (I .GT. IENDC(ICI)) ICI = ICI+1
         DO 22 J = 1,NVEC
            LOC = (J-1)*NCF
            CONTRI1 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT1
            CONTRI1VEC(:) = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT1VEC(:)           ! JE ADD
            CONTRI2 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT2
            CONTRI3 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT3
            CONTRI4 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT4
            CONTRI5 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT5
            CONTRI6 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT6
            CONTRI7 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT7
            if (IR.NE.IC) then
               CONTRI1 = 2.0D00 * CONTRI1
               CONTRI1VEC(:) = 2.0D00 * CONTRI1VEC(:)                        ! JE ADD
               CONTRI2 = 2.0D00 * CONTRI2
               CONTRI3 = 2.0D00 * CONTRI3
               CONTRI4 = 2.0D00 * CONTRI4
               CONTRI5 = 2.0D00 * CONTRI5
               CONTRI6 = 2.0D00 * CONTRI6
               CONTRI7 = 2.0D00 * CONTRI7
            endif
            DENS1(J) = DENS1(J) + CONTRI1
            DO 23 L = 2,NRNUC                                                  ! JE ADD
               DENS1VEC(J,L) = DENS1VEC(J,L) + CONTRI1VEC(L)                  ! JE ADD
   23       CONTINUE                                                          ! JE ADD
            DENS2(J) = DENS2(J) + CONTRI2
            DENS3(J) = DENS3(J) + CONTRI3
            DENS4(J) = DENS4(J) + CONTRI4
            DENS5(J) = DENS5(J) + CONTRI5
            DENS6(J) = DENS6(J) + CONTRI6
            DENS7(J) = DENS7(J) + CONTRI7
   22    CONTINUE
!*   21 CONTINUE

      GOTO 16
      endif
      return
      end subroutine DENSREAD_SELTZ

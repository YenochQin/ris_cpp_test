!***********************************************************************
!                                                                      *
!
!   Call(s) to: [LIB92]: ALCBUF                                        *
!                                                                      *
!   WRITTEN  by C. Naz\'e  Oct. 2011                                   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use parameter_def,    only: KEYORB, NNNW
      use prnt_C
      use ris_C
      use orb_C
      use eigv_C
      use BUFFER_C
      use debug_C,          only: CUTOFF
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use alcbuf_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(kind=real64), dimension(NNNW,NNNW), intent(in) :: VINT, VINT2
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: KEY = KEYORB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(kind=real64) :: CONTRI, CONTRIK1, COEFFSMS
      integer :: IC, IR, I, J, IIA, IIB, IIC, IID, LOC, LAB, IOS
!-----------------------------------------------
!
      CALL ALCBUF (1)

      REWIND(51)
  16  READ (51,IOSTAT = IOS) IC,IR
!C      WRITE(*,*) IC,IR
      if (IOS .EQ. 0) then
!cc        DO J = 1,NVEC
          READ(51) COEFFSMS,LAB
          IID = MOD (LAB, KEY)
          LAB = LAB/KEY
          IIB  = MOD (LAB, KEY)
          LAB = LAB/KEY
          IIC = MOD (LAB, KEY)
          IIA = LAB/KEY
        DO J = 1,NVEC
          LOC = (J-1)*NCF
            CONTRIK1 = - EVEC(IC+LOC)*EVEC(IR+LOC)                     &
               * COEFFSMS                                              &
               * VINT (IIA,IIC)*VINT(IIB,IID)
            CONTRI = - EVEC(IC+LOC)*EVEC(IR+LOC)                       &
               * COEFFSMS                                              &
               * ( VINT2(IIA,IIC)*VINT(IIB,IID)                        &
               + VINT2(IIB,IID)*VINT(IIA,IIC))/2.0D00
          if (IR.NE.IC) then
            CONTRI = 2.0D00 * CONTRI
            CONTRIK1 = 2.0D00 * CONTRIK1
          endif
          SMSC1(J) = SMSC1(J) + CONTRIK1
          SMSC2(J) = SMSC2(J) + CONTRI
        ENDDO

      GOTO 16
      endif
      CALL ALCBUF (3)
      return
      end subroutine SMSREAD

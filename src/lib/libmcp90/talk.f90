!***********************************************************************
!                                                                      *
      subroutine TALK(JA, JB, NU, IA, IB, IC, ID, ITYPE, COEF)
!                                                                      *
!   Print  coefficients  and  integral  parameters  if IBUG1 > 0 and   *
!   write to disk.                                                     *
!                                                                      *
!                                           Last update: 14 Oct 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  15:16:29   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use parameter_def,   only:  KEYORB
      use BUFFER_C
      use debug_C
      use orb_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use alcbuf_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: JA, JB, NU, IA, IB, IC, ID, ITYPE
      real(real64), intent(in) :: COEF
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: KEY = KEYORB
!-----------------------------------------------
!   Print coefficient if requested
!
      if (IBUG1 /= 0) WRITE (99, 300) JA, JB, NP(IA), NH(IA), NP(IB), NH(IB), &
         NP(IC), NH(IC), NP(ID), NH(ID), NU, ITYPE, COEF
!
!   Increment counter
!
      NVCOEF = NVCOEF + 1
!
!   Ensure that arrays are of adequate size; reallocate if necessary
!
      if (NVCOEF > NBDIM) CALL ALCBUF (2)
!
!   Store integral indices and coefficient in COMMON/BUFFER/
!
      LABEL(1,NVCOEF) = IA
      LABEL(2,NVCOEF) = IB
      LABEL(3,NVCOEF) = IC
      LABEL(4,NVCOEF) = ID
      LABEL(5,NVCOEF) = NU
      LABEL(6,NVCOEF) = ITYPE
      COEFF(NVCOEF) = COEF
!
      return
!
  300 FORMAT(2(1X,1I2),4(1X,I2,A2),1X,1I2,1X,1I2,1X,1P,D19.12)
      return
!
      end subroutine TALK

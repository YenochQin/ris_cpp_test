!***********************************************************************
!                                                                      *
      integer FUNCTION ICHKQ1 (JA, JB)
!                                                                      *
!   This routine is to check the occupation condition for one electron *
!   operator.                                                          *
!                                                                      *
!   Call(s) to: [LIB92]: IQ.                                           *
!                                                                      *
!   Yu Zou                                Last revision: 8/16/00       *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:20   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use DEBUG_C
      use ORB_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use iq_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer :: JA
      integer :: JB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: K, I, I_IQA, I_IQB

!-----------------------------------------------
!
!
      ICHKQ1 = 0
      K = 0
      DO I = 1, NW
         I_IQA = IQ(I,JA)
         I_IQB = IQ(I,JB)
         if (I_IQA == I_IQB) CYCLE
         K = K + 1
         if (K > 2) return
         if (IABS(I_IQA - I_IQB) <= 1) CYCLE
         return
      END DO
      if (K==2 .OR. K==0) ICHKQ1 = 1
      return
      END FUNCTION ICHKQ1

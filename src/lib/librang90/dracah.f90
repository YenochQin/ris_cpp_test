!***********************************************************************
!                                                                      *
      subroutine DRACAH(I, J, K, L, M, N, RAC)
!                                                                      *
!   subroutine  to calculate Racah coefficients. The arguments I, J,   *
!   K, L, M, N should be twice their actual value. Works for integer   *
!   and  half-integer  values of  angular momenta. The routine makes   *
!   use of the GAM  array, thus  subroutine FACTT must be called be-   *
!   fore this routine is used.                                         *
!                                                                      *
!   Written by N S Scott                    Last update: 16 Oct 1992   *
!   The last modification made by G. Gaigalas           October 2017   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  07:27:24   2/14/04
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use FACTS_C
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: I
      integer , intent(in) :: J
      integer , intent(in) :: K
      integer , intent(in) :: L
      integer , intent(in) :: M
      integer , intent(in) :: N
      real(kind=real64) , intent(out) :: RAC
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: J1, J2, J3, J4, J5, J6, J7, NUMIN, NUMAX, ICOUNT, KK, KI
!-----------------------------------------------
      J1 = I + J + M
      J2 = K + L + M
      J3 = I + K + N
      J4 = J + L + N
      if (2*MAX(MAX(I,J),M) - J1>0 .OR. MOD(J1,2)/=0) GO TO 2
      if (2*MAX(MAX(K,L),M) - J2>0 .OR. MOD(J2,2)/=0) GO TO 2
      if (2*MAX(MAX(I,K),N) - J3>0 .OR. MOD(J3,2)/=0) GO TO 2
      if (2*MAX(MAX(J,L),N) - J4>0 .OR. MOD(J4,2)/=0) GO TO 2
      GO TO 1
    2 CONTINUE
      RAC = 0.0D00
      return
!
    1 CONTINUE
      J1 = J1/2
      J2 = J2/2
      J3 = J3/2
      J4 = J4/2
      J5 = (I + J + K + L)/2
      J6 = (I + L + M + N)/2
      J7 = (J + K + M + N)/2
      NUMIN = MAX(MAX(MAX(J1,J2),J3),J4) + 1
      NUMAX = MIN(MIN(J5,J6),J7) + 1
      RAC = 1.0D00
      ICOUNT = 0
!
      if (NUMIN /= NUMAX) then
         NUMIN = NUMIN + 1
!
         DO KK = NUMIN, NUMAX
            KI = NUMAX - ICOUNT
            RAC = 1.0D00 - RAC*DBLE(KI*(J5 - KI + 2)*(J6 - KI + 2)*(J7 - KI + 2&
               ))/DBLE((KI - 1 - J1)*(KI - 1 - J2)*(KI - 1 - J3)*(KI - 1 - J4))
            ICOUNT = ICOUNT + 1
         END DO
!
         NUMIN = NUMIN - 1
      endif
      RAC = RAC*(-1.0D00)**(J5 + NUMIN + 1)*EXP((GAM(NUMIN+1)-GAM(NUMIN-J1)-GAM&
         (NUMIN-J2)-GAM(NUMIN-J3)-GAM(NUMIN-J4)-GAM(J5+2-NUMIN)-GAM(J6+2-NUMIN)&
         -GAM(J7+2-NUMIN))+(GAM(J1+1-I)+GAM(J1+1-J)+GAM(J1+1-M)-GAM(J1+2)+GAM(&
         J2+1-K)+GAM(J2+1-L)+GAM(J2+1-M)-GAM(J2+2)+GAM(J3+1-I)+GAM(J3+1-K)+GAM(&
         J3+1-N)-GAM(J3+2)+GAM(J4+1-J)+GAM(J4+1-L)+GAM(J4+1-N)-GAM(J4+2))*&
         0.5D00)
!
      return
      end subroutine DRACAH

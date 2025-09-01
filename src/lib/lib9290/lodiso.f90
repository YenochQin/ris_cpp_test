!***********************************************************************
!                                                                      *
      subroutine LODISO
!                                                                      *
!   Loads the data from the  .iso  file.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 29 Sep 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
      use DEF_C
      use NPAR_C
      use NSMDAT_C,        only: SQN, DMOMNM, QMOMB
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(kind=real64) :: A, APARM, CPARM, EMNAMU
!
!   Read and echo pertinent information from  .iso  file
!
!   Atomic number
!
      READ (22, *) Z
!
!   Nuclear geometry
!
      READ (22, *)
      READ (22, *) A
      READ (22, *)
      READ (22, *) APARM
      READ (22, *)
      READ (22, *) CPARM
!
      if (A /= 0.D0) then
         NPARM = 2
         PARM(1) = CPARM*FMTOAU
         PARM(2) = APARM*FMTOAU
      else
         NPARM = 0
      endif
!
!   Nuclear mass
!
      READ (22, *)
      READ (22, *) EMNAMU

!
      if (EMNAMU /= 0.D0) then
         EMN = EMNAMU/AUMAMU
      else
         EMN = 0.D0
      endif


!
!   Nuclear spin and moments
!
      READ (22, *)
      READ (22, *) SQN
      READ (22, *)
      READ (22, *) DMOMNM
      READ (22, *)
      READ (22, *) QMOMB
!
!   Grid parameters from isodata
!   Jon Grumer (Lund, 2013)
!
!     READ (22,*)
!     READ (22,*) RNT
!     READ (22,*)
!     READ (22,*) H
!     READ (22,*)
!     READ (22,*) HP
!     READ (22,*)
!     READ (22,*) N

      return
      end subroutine LODISO

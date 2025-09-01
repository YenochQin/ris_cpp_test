!!***********************************************************************
!                                                                      *
      integer FUNCTION JQS (IWHICH, ISUBSH, ICSF)
!                                                                      *
!   JQS is a subshell quantum number for subshell ISUBSH in configu-   *
!   ration state function  ICSF:  the seniority if IWHICH is 1;  the   *
!   quantum number w if IWHICH is 2, and 2J+1 if IWHICH is 3.          *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 02 Nov 1992   *
!   Modified by G. Gaigalas                                 May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:48:50   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use parameter_def, only: NNNW
      use STAT_C,        only: JQSA
      use IOUNIT_C,      only: istde
      use orb_C,         only: NCF
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer             :: IWHICH
      integer, intent(in) :: ISUBSH
      integer             :: ICSF
!-----------------------------------------------
!
      jqs = jqsa(isubsh,iwhich,icsf)
!
      return
      END FUNCTION JQS

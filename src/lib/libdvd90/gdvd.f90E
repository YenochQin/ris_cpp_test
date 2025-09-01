
!***************************************************************************

      subroutine GDVD(OP, N, LIM, DIAG, ILOW, IHIGH, ISELEC, NIV, MBLOCK, CRITE&
         , CRITC, CRITR, ORTHO, MAXITER, WORK, IWRSZ, IWORK, IIWSZ, HIEND, &
         NLOOPS, NMV, IERR)
!      Written by M. Saparov
!
! Note:
! HIEND, ISELEC() not used outside dvdson
!***************************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  20:12:31   2/12/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use iso_fortran_env, only: real64, int32, int64, real128
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      !use op_I
      use dvdson_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: N
      integer  :: LIM
      integer  :: ILOW
      integer  :: IHIGH
      integer  :: NIV
      integer  :: MBLOCK
      integer  :: MAXITER
      integer  :: IWRSZ
      integer  :: IIWSZ
      integer  :: NLOOPS
      integer, intent(out) :: NMV
      integer  :: IERR
      real(kind=real64)  :: CRITE
      real(kind=real64)  :: CRITC
      real(kind=real64)  :: CRITR
      real(kind=real64)  :: ORTHO
      logical  :: HIEND
      integer  :: ISELEC(LIM)
      integer  :: IWORK(IIWSZ)
      real(kind=real64), dimension(N), INTENT(INOUT) :: DIAG
      real(kind=real64)  :: WORK(IWRSZ)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, dimension(7) :: IREV
      integer :: NOC, IRC, NB, IW1, IW2, IW3, IIW, IIN, IW4, ICUR, I, INDX, J
      real(kind=real64) :: VALUE, OVALUE, RNORM, EPSIL
!
!**********************************************************************
!  NOC = number of orthogonalization constraints
!  IRC = reverse communication switch

      NOC = 0
      IRC = 0

!***********************************************************************
! CALLING DAVIDSON with reverse communication
!***********************************************************************
! Initial estimates
!
      if (NIV == 0) then
         WRITE (6, *) 'GDVD Error : No initial estimate!!!'
         IERR = -1000
         return
      endif

      !ttt=etime_(tarray)
      NMV = 0
      !xhh print*, 'MBLOCK = ', mblock
      !xhh print *, ' gdvd:  niv = ', niv
   99 CONTINUE
      CALL DVDSON (IRC, IREV, N, LIM, NOC, ILOW, IHIGH, ISELEC, NIV, MBLOCK, &
         CRITE, CRITC, CRITR, MAXITER, WORK, IWRSZ, IWORK, IIWSZ, HIEND, NLOOPS&
         , IERR)
!
! * * Start Reverse Communication * * * * * * * * * * * * * * * * * * *
!
      NB = IREV(1)
      IW1 = IREV(2)
      IW2 = IREV(3)
      IW3 = IREV(4)
      IIW = IREV(5)
      IIN = IREV(6)
      IW4 = IREV(7)

      if (IRC == 1) then
!********* ..Preconditioning. Solve NB times(M work(iw2)=work(iw1))
!          ..Results always on work(iw2)

         ICUR = 0
         DO I = 1, NB
            INDX = IWORK(IIW+I-1) - 1
            VALUE = WORK(IW3+INDX)
            OVALUE = WORK(IW4+INDX)
            RNORM = WORK(IIN+INDX)
            EPSIL = WORK(IIN+LIM+INDX)
! The current approximation of the eigenvector is x=Bc
! If needed it should be saved e.g: call dcopy(N,work(iw2),1,curx,1)

!            write(*,11) nloops,value,rnorm
   11       FORMAT('It ',I4,'  Dl',D10.3,' Res:',D10.3)
!
!-------------
! (M-lI)      .. Compute temporarily (M-valueI) (M preconditioner)
!             .. Needed for (M-valueI)^-1 res
!             .. Here M is the DIAG
            DIAG(:N) = DIAG(:N) - VALUE
!-------------
!* Choice of Diagonal preconditioning
            DO J = 1, N
               if (ABS(DIAG(J)) > 1.0D-06) then
                  WORK(IW2+ICUR+J-1) = WORK(IW1+ICUR+J-1)/DIAG(J)
               else
                  WORK(IW2+ICUR+J-1) = WORK(IW1+ICUR+J-1)*1.0D06
               endif
            END DO
!*-------------
!* e.g: For No preconditioner: Lanczos
!*            call dcopy(N,WORK(iw1+icur),1,WORK(iw2+icur),1)
!*-------------
!* (M+lI)      .. Restore (M+valueI)
            DIAG(:N) = DIAG(:N) + VALUE

            ICUR = ICUR + N
         END DO

         GO TO 99

!*********
      else if (IRC==2 .OR. IRC==3) then
!********* ..Matrix-vector multiply.
         CALL OP (N, NB, WORK(IW1), WORK(IW2))
         NMV = NMV + NB

         GO TO 99
      endif
! * * * * End of Reverse Communication * * * * * * * * * * * * * * * * *

      return
      end subroutine GDVD

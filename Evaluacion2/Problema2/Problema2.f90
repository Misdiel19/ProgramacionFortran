!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                       !
! Please Note:                                                          !
!                                                                       !
! (1) This computer program is written by Tao Pang in conjunction with  !
!     his book, "An Introduction to Computational Physics," published   !
!     by Cambridge University Press in 1997.                            !
!                                                                       !
! (2) No warranties, express or implied, are made for this program.     !
!                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM ONE_D_MOTION

! Program for the motion of a particle subject to an external
! force f(x) = -x.   We have divided the total time 2*pi into
! 10000 intervals with an equal time step.   The position and
! velocity of the particle are written out at every 500 steps.
! Copyright (c) Tao Pang 1997.

  IMPLICIT NONE
  INTEGER, PARAMETER :: N=10001,IN=500
  INTEGER :: I
  REAL :: PI,DT,k
  REAL, DIMENSION (N):: T,V,X
  
  PRINT*, "Favor de introducir el valor de la constante k"
  READ*, k


! Assign constants, initial position, and initial velocity

  PI   = 4.0*ATAN(1.0)
  DT   = 2.0*PI/FLOAT(N-1)
  X(1) = 0.0
  T(1) = 0.0
  V(1) = 1.0

! Recursion for position and velocity at later time

open(1,file="Resorte.dat", status="unknown")
 DO I = 1, N-1
    T(I+1) = DT*I
    X(I+1) =(X(I)+V(I)*DT)
    V(I+1) = V(I)-(K*X(I))*DT
    write(1,*) T(I),X(I),V(I)
  END DO

! Write the position and velocity every 500 steps

  WRITE (6,"(3F16.8)") (T(I),X(I),V(I),I=1,N,IN)
  write(1,*) T(I),X(I),V(I)
END PROGRAM ONE_D_MOTION

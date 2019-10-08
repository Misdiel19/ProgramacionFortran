PROGRAM pade
IMPLICIT NONE

REAL(kind=8),external::ExpPx,ExpPz,ExpPj
REAL(kind=8)::ExpPaX,y,x,Error,z,j
INTEGER::i

 OPEN (1,FILE='ExpP02.dat')
  DO i=-31415926,31415926,1000
    x=i*0.0000001
     ExpPaX=Exp(x)
     y=ExpPx(x)
      Error=ExpPaX-(y/ExpPaX)
	Print*, x, Error
       WRITE(1,*) x,Error
  END DO
 CLOSE (1)

 OPEN (2,FILE='ExpP11.dat')
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpPaX=Exp(z)
     y=ExpPz(z)
      Error=ExpPaX-(y/ExpPaX)
	
       WRITE(2,*) z,Error
  END DO
 CLOSE (2)

 OPEN (3,FILE='ExpP20.dat')
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpPaX=Exp(j)
     y=ExpPj(j)
      Error=ExpPaX-(y/ExpPaX)
       
       WRITE(3,*) j,Error
  END DO
 CLOSE (3)

 
END PROGRAM pade
!------------------------------------------------
FUNCTION ExpPx(x)
IMPLICIT NONE

REAL(kind=8),intent(in)::x
REAL(kind=8)::ExpPx,m,n

m=1.0

n=1-x+(x**2)*(1.0/2.0)

ExpPx=m/n

END FUNCTION ExpPx
!-----------------------------------------------------

FUNCTION ExpPz(z)

IMPLICIT NONE

REAL(kind=8),intent(in)::z
REAL(kind=8)::ExpPz,m,n

m=1+z*(1.0/2.0)

n=1-z*(1.0/2.0)

ExpPz=m/n

END FUNCTION ExpPz
!------------------------------------------------------

FUNCTION ExpPj(j)
 IMPLICIT NONE

REAL(kind=8),intent(in)::j
REAL(kind=8)::ExpPj,m,n

m=1+j+(j**2)*(1.0/2.0)

n=1.0

ExpPj=m/n

END FUNCTION ExpPj
!------------------------------------------------------


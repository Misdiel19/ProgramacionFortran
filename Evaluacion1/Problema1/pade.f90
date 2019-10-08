	PROGRAM pade
	 IMPLICIT NONE

	  REAL(kind=8),external::SnP
	  REAL(kind=8)::Seno_X,y,x,b,ErrorR
	  INTEGER::i

 	b=0

	 OPEN (1,FILE='padeS.dat')
  	DO i=-31415926,31415926,1000
  	  x=i*0.0000001
	     Seno_X=Sin(x)
  	    WRITE(1,*) x,Seno_X,b
	  END DO

	 WRITE(1,*) ' '
	 b=1

 	 DO i=-31415926,31415926,1000
 	   x=i*0.0000001
	     y=SnP(x)
 	     WRITE(1,*) x,y,b
	  END DO
	 CLOSE (1)
 
	 OPEN (2,FILE='ErrorSP.dat')
 	 DO i=0,31415926,1000
 	   x=i*0.0000001
 	    Seno_X=Sin(x)
 	    y=SnP(x)
  	    ErrorR=Seno_X-(y/Seno_X)
	    Print*, x, ErrorR
   	    WRITE(2,*) x,ErrorR
 	 END DO
 	CLOSE (2)
	
	END PROGRAM pade
	
	FUNCTION SnP(x)

	IMPLICIT NONE

	REAL(kind=8),intent(in)::x
	REAL(kind=8)::SnP,m,n

	m=x-(x**3)*(2363.0/18183.0)+(x**5)*(12671.0/4363920.0)

	n=1+(x**2)*(445.0/12122.0)+(x**4)*(601.0/872784.0)+(x**6)*(121.0/16662240.0)

	SnP=m/n

	END FUNCTION SnP

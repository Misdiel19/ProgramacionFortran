PROGRAM ppal

  IMPLICIT NONE

  REAL::x,h,f,a,b
  INTEGER::i,n,nterm

  a=0
  b=1
  n=250
  h=(b-a)/n
  nterm=16

  OPEN(1,FILE="logaritmo",STATUS="UNKNOWN")
  
  DO i=-30,n-1
     x=a+i*h

     CALL loga(x,nterm,f)
     PRINT*, x,f
     WRITE(1,*)x,f
     WRITE(1,*)
     WRITE(1,*)
     WRITE(1,*)
  ENDDO
  CLOSE(1)
  
  
END PROGRAM Ppal

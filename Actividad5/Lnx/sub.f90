SUBROUTINE loga(x,nt,f)

  IMPLICIT NONE

  REAL::x,f,m1
  INTEGER::i,nt,m

  f=0
  DO i=1,nt
     m=(-1)**(i+1)
     m1=x**i/i
     f=f+m*m1
     !print*, i,x,m,m1,f
  ENDDO

END SUBROUTINE LOGA

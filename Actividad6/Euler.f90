!--------------------------------------------------------
! En este programa resolveremos la ecuacion del oscilador
! armonico, utilizando el metodo de Euler.
!--------------------------------------------------------
Program Euler
  implicit none

  real:: A, h, aw, t, y, B
  integer:: j
  real,dimension(2):: M
  real,parameter:: l = 9.81
  real,parameter:: g = 9.81
  
 !calculamos 
   aw = sqrt(g/l)	
 !Le pedimos al usuario el angulo y el numero de pasos
   print*," Favor de introducir el angulo y tamaño de pasos"
   read(*,*) A, h
 
 !Calculamos el movimiento del pendulo dada la funcion 
  open(1, file="Pendulo.dat", status="unknown")
   do j=0,7000
    t=float(j)* h
    if(t>6.3) exit
    y=A*cos(aw*t)
     print*, t, y
     write(1,*) t,y,1
   end do

  write(1,*) " "
  B = A 
   do j=0,9000
    t=float(j)*h
    if(t>6.3) exit
   call Matriz(A, aw, h, l, g, M)
     write(1,*) t, M(1), 2
    A = M(1)
    aw = M(2)
   end do

  close(1)

!Calculamos el error relativo
print*, "Error", abs((B-A)/B)

End Program Euler

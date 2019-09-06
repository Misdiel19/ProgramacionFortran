program Proyectil
 implicit none 

 !definimos variables 
 real:: tf,vo, x, y, t 
 integer:: n, np, m 
 real,parameter:: g = 9.8
 real,parameter:: pi=3.1415927
 real,parameter:: Dt=1./10.
 real,dimension(6)::a

np=1200
a=(/15,30,45,60,75,90/)

 !Leer los valores para el angulo a, el tiempo t, y la velocidad vo desde la terminal.
 print*, "Favor de introducir el tiempo y la velocidad inicial."
 read*,  tf, vo

OPEN(unit =11,file="salida.dat",status="unknown")

!Convertimos el angulo a radianes.
 a = a * pi / 180.0
 do m=1,6
   do n=0,np
     t=float(n)*Dt
     x = vo * cos(a(m)) * t
     y = vo * sin(a(m)) * t - 0.5 * g * t * t
     print*, x, y
    if (y < 0.0) exit 
    write (11,*) x, y
   end do
  write (11,*)" "
  write (11,*)"# "
 write (11,*)" "
end do
 stop

close (11)
end program Proyectil


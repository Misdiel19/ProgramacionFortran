Program Sin_Aire

implicit none 

 !Definimos Variables

 real,parameter:: pi= 3.1415927
 real,parameter:: g = 9.81
 real:: t, x, y, dt, m, Vx0, Vy0
 real:: Vt, Vx, Vy, Vi, a, Xmax, Ymax, ty 
 integer:: np, n

 x= 0
 y= 0
 t= 0
 dt= 0.1
 Vi = 44.7
 Vt = -33
 np= 200
 m = 0.145
 a= 45
 ty = 0

open(unit=11, file="Parabola.dat",status="unknown")
 
 a = a * pi / 180.0

 Vx = Vi * Cos(a) 
 Vy = Vi * Sin(a)

do n=0,np
 
 x = Vx * t
 
 y = (Vy*t) + (0.5 * (-g) * (t**2))

 t = t + dt
 
  Print*, x, y

  if ( y < 0.0 ) exit

  write (11,*) x, y

     write (11,*)" "
     write (11,*)"# "
     write (11,*)" "

end do 

Xmax = ((Vi**2) * Sin(2*a)) / g

 print*, "El alcance maximo en x es de:" , Xmax , "en un tiempo de: " , t 

Ymax = ((Vi**2) *(Sin(a)*Sin(a)))/ (2*g)

ty = t / 2

print*, "La altura maxima es de:" , Ymax , "en un tiempo de: " , ty 

close (11)

end program Sin_Aire

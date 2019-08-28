!Se calculara la altura maxima de un proyectil dados el angulo
! y la rapidez del mismo proyectil.
 
 program altura_max
  implicit none

 !definimos las variables 
  real:: AM, a, v
 !definimos constantes
  real,parameter:: g= 9.8
  real,parameter:: pi=3.1416

 !Le pedimos al usuario el angulo (a) y la rapidez del proyectil (v) 
 print*, "Favor de introducir el angulo y la rapidez del proyectil"
 read*, a,v

 !convertimos el angulo en radianes
 a = a * pi/180

 !Calculamos la altura maxima del proyectil
 AM = ((v**2)* (sin(a)*sin(a))) / 2*g

 print*, "La altura maxima del proyectil es de:", AM
end program 


!Se calculara el despalzamiento maximo en hay en el eje de la x
!dado el angulo y la rapidel del proyectil.

program X
 implicit none 

!definimos las variables por usar 
 real:: d, v, a

!definimos constantes
 real,parameter:: pi=3.1416
 real,parameter:: g=9.8

!Le pedimos al usuario que ingrese los datos necesarios para calcular lo pedido

 print*, "Favor de introducir un angulo y la rapidez de un proyectil."
 read*, a, v

!convertimos el angulo a radianes.
a = a * pi/180

!realizamos la siguiente operacionm para calcular el alcance maximo en x.

d = ((v**2)/g) * sin(2*a)


print*, "El despazamiento maximo en x es de:", d

end program 

!Se calculara el tiempo de vuelo del proyectil dada
! la rapidez del proyectil y el angulo del mismo.

program Tiempo
 implicit none 

 !definimos las variables.
 real:: a, v, tv

 !definimos constantes
 real,parameter:: g= 9.8

 !Le pedimos al usuario que ingrese los datos que se van a utilizar para la operacion 
 !donde a es el angulo y v es la rapidez del proyectil.
 
 print*, "Dame el angulo y la rapidez del proyectil."
 read*, a, v
 
 tv = ((2*v) * sin(a)) / g

 print*, "El tiempo de vuelo del proyectil es de:", tv," segundos."

end program Tiempo

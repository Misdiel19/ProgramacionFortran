!----------------------------------------------------------------------------------
PROGRAM Triangle
     IMPLICIT NONE
     REAL :: a, b, c, Area, Volumen 
     PRINT *, 'Favor de introducir los tres lados del Triangulo'
     PRINT*, "Tammbien con estos mismo lados, se calculara el Volumen de un Paralepipedo"
     READ *, a, b, c
     PRINT *, 'Area del Triangulo es:  ', Area(a,b,c)
     PRINT *, 'El volumen del Paralepipedo es:  ', Volumen (a,b,c)
    END PROGRAM Triangle
!-------------------------------------------------------------------------------------
    FUNCTION Area(x,y,z)
     IMPLICIT NONE
     REAL :: Area            ! Tipo de Funcion 
     REAL, INTENT( IN ) :: x, y, z
     REAL :: theta, height
     theta = ACOS(((x**2+y**2-z**2)/(2.0*x*y)))
     height = x*SIN(theta); Area = 0.5*y*height
    END FUNCTION Area
!--------------------------------------------------------------------------------------
    FUNCTION Volumen(x,y,z)
     Implicit none 
      real:: Volumen 	!Tipo de Funcion 
      real, intent (IN):: x, y, z
      real:: Vol  
       Vol = x*y*z
    END FUNCTION 
!-----------------------------------------------------------------------------------

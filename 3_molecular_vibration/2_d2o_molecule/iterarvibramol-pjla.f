*     iterarvibra-pjla.f
************************************************************************
*
*     Subrutina que itera de la vibracion de los atomos
*     de una molecula de acuerdo al metodo de Verlet
*
************************************************************************
*
*  nmol  -  numero de particulas en el interior de nuestra cadena       (input)
*  h     -  paso de avance en el barrido temporal                       (input)
*  vmass -  masa de las particulas de la cadena                         (input)
*  x     -  vector con la posicion x de cada particula                  (input-output)
*  y     -  vector con la posicion y de cada particula                  (input-output)
*  vx    -  vector con la velocidad x de cada particula                 (input-output)
*  vy    -  vector con la velocidad y de cada particula                 (input-output)
*  d12   -  distancia original H1-O                                     (output)
*  d32   -  distancia original H2-O                                     (output)
*  d12n  -  distancia nueva H1-O                                        (output)
*  d32n  -  distancia nueva H2-O                                        (output)
*
*
*     Pedro Jesus Lopez Abenza                     01-02-2017
************************************************************************

      subroutine iterarvibramol(nmol,h,vmass,x,y,vx,vy,alpha,alphan,d12,
     &                          d32,d12n,d32n)
      implicit real*8 (a-h,o-z)
      integer j,nmol      
      dimension x(nmol),y(nmol),xnew(nmol),ynew(nmol)
      dimension vx(nmol),vy(nmol),vxnew(nmol),vynew(nmol)
      dimension vmass(nmol),flx(nmol),fly(nmol),fax(nmol),fay(nmol)
      dimension flxnew(nmol),flynew(nmol),faxnew(nmol),faynew(nmol)

*Definimos las constantes en unidades atomicas
      
      pi=4.0d0*datan(1.0d0)
      dis0=1.809d0      
      alpha0=104.45d0*(pi/180.0d0)   
      ctel=0.5371d0            !cte longitudinal
      ctea=0.0477d0            !cte angular
      
*Definimos las distancias entre moleculas en nuestro instante segun las posiciones
      d12=dsqrt((x(2)-x(1))**2+(y(2)-y(1))**2)
      d32=dsqrt((x(2)-x(3))**2+(y(2)-y(3))**2)
      
*Calculamos el angulo alpha y la diferencia entre alpha y alpha0 segun las posiciones
      alpha=dacos(((y(2)-y(1))/d12)*((y(2)-y(3))/d32)-((x(1)-x(2))/d12)*
     & ((x(2)-x(3))/d32))
      dalpha=alpha-alpha0
      
*Calculamos el modulo de las fuerzas longitudinales y angulares
      fzl1=-ctel*(d12-dis0)
      fzl3=-ctel*(d32-dis0)
      fza1=-ctea*dalpha
      fza3=-ctea*dalpha
        
*Definimos las fuerzas longitudinales
      flx(1)=fzl1/d12*(x(1)-x(2))
      fly(1)=fzl1/d12*(y(1)-y(2))
      flx(3)=fzl3/d32*(x(3)-x(2))
      fly(3)=fzl3/d32*(y(3)-y(2))
      flx(2)=-flx(1)-flx(3)
      fly(2)=-fly(1)-fly(3)
      
*Definimos las fuerzas angulares
      fax(1)=fza1/d12*(y(2)-y(1))
      fay(1)=fza1/d12*(x(1)-x(2))
      fax(3)=fza3/d32*(y(3)-y(2))
      fay(3)=fza3/d32*(x(2)-x(3))
      fax(2)=-fax(1)-fax(3)
      fay(2)=-fay(1)-fay(3)

*Bucles para calcular las nuevas posiciones
      do j=1,nmol      
          ax=(fax(j)+flx(j))/vmass(j)
          ay=(fay(j)+fly(j))/vmass(j)
*-----Calculamos las posiciones por las fuerzas angulares
          xnew(j)=x(j)+vx(j)*h+0.5d0*ax*h**2 
          ynew(j)=y(j)+vy(j)*h+0.5d0*ay*h**2 
      end do
      
*Definimos las distancias entre moleculas en nuestro instante segun las posiciones
      d12n=dsqrt((xnew(2)-xnew(1))**2+(ynew(2)-ynew(1))**2)
      d32n=dsqrt((xnew(2)-xnew(3))**2+(ynew(2)-ynew(3))**2)
      
*Calculamos el NUEVO angulo alpha y la NUEVA diferencia entre alpha y alpha0     
      alphan=dacos(((ynew(2)-ynew(1))/d12n)*((ynew(2)-ynew(3))/d32n) -
     &       ((xnew(1)-xnew(2))/d12n)*((xnew(2)-xnew(3))/d32n))
      dalphan=alphan-alpha0
      
*Calculamos el modulo de las NUEVAS fuerzas longitudinales y angulares
      fzl1n=-ctel*(d12n-dis0)
      fzl3n=-ctel*(d32n-dis0)
      fza1n=-ctea*dalphan
      fza3n=-ctea*dalphan
        
*Definimos las NUEVAS fuerzas longitudinales
      flxnew(1)=fzl1n/d12n*(xnew(1)-xnew(2))
      flynew(1)=fzl1n/d12n*(ynew(1)-ynew(2))
      flxnew(3)=fzl3n/d32n*(xnew(3)-xnew(2))
      flynew(3)=fzl3n/d32n*(ynew(3)-ynew(2))
      flxnew(2)=-flxnew(1)-flxnew(3)
      flynew(2)=-flynew(1)-flynew(3)
      
*Definimos las NUEVAS fuerzas angulares
      faxnew(1)=fza1n/d12n*(ynew(2)-ynew(1))
      faynew(1)=fza1n/d12n*(xnew(1)-xnew(2))
      faxnew(3)=fza3n/d32n*(ynew(3)-ynew(2))
      faynew(3)=fza3n/d32n*(xnew(2)-xnew(3))
      faxnew(2)=-faxnew(1)-faxnew(3)
      faynew(2)=-faynew(1)-faynew(3)
      
*Bucle para calcular las nuevas velocidades
*-------(aqui no nos interesa hacer una separacion angular-longitudinal)
      do j=1,nmol
          ax=(flx(j)+fax(j))/vmass(j)
          ay=(fly(j)+fay(j))/vmass(j) 
          axnew=(flxnew(j)+faxnew(j))/vmass(j)
          aynew=(flynew(j)+faynew(j))/vmass(j)  
*-------Calculamos las velocidades nuevas
          vxnew(j)=vx(j)+0.5d0*h*(ax+axnew)
          vynew(j)=vy(j)+0.5d0*h*(ay+aynew)
      end do

*Actualizamos los vectores de velocidad y posicion de la particula j
      do j=1,nmol
          x(j)=xnew(j)
          y(j)=ynew(j)
          vx(j)=vxnew(j)
          vy(j)=vynew(j)
      end do
      
      return
      end

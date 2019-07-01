*     iterarvibra-pjla.f
************************************************************************
*
*     Subrutina que itera de la vibracion de un cierto atomo
*     de una cierta masa m con los metodos
*                        -metodo euler           (k=1)
*                        -metodo euler-cromer    (k=2)
*                        -metodo euler-mejorado  (k=3)
*                        -metodo verlet          (k>3)
*
************************************************************************
*
*  k     -  entero que nos indica el metodo de euler a utilizar   (input)
*  np    -  numero de particulas en el interior de nuestra cadena (input)
*  h     -  paso de avance en el barrido temporal                 (input)
*  mass  -  masa de las particulas de la cadena                   (input)
*  c     -  constantes de amortiguamiento de la cadena            (input)
*  x     -  vector con elongaciones iniciales de cada particula   (input-output)
*  v     -  vector con la velocidades iniciales de cada particula (input-output)
*
*     Pedro Jesus Lopez Abenza                     01-02-2017
************************************************************************

      subroutine iterarvibra(k,np,h,c,mass,x,v)
      integer j,k,np
      real*8 x,xnew,v,vnew,mass,c,h,a,anew
      dimension x(0:np+1),v(0:np+1),xnew(0:np+1),vnew(0:np+1)
      dimension mass(0:np+1),c(0:np)
      
      xnew(0)=x(0)
      xnew(np+1)=x(np+1)

      if (k.ne.4) then
         do j=1,np   
        vnew(j)=v(j)+h/mass(j)*(c(j-1)*(x(j-1)-x(j))+c(j)*(x(j+1)-x(j)))
            if (k.eq.1) xnew(j)=x(j)+h*v(j)                   !euler-simple
            if (k.eq.2) xnew(j)=x(j)+h*vnew(j)                !euler-cromer
            if (k.eq.3) xnew(j)=x(j)+h*((v(j)+vnew(j))/2.0d0) !euler-mejorado 
         end do
*----Actualizamos los vectores de velocidad y posicion de la particula j
         do j=1,np
          x(j)=xnew(j)
          v(j)=vnew(j)
         end do  
      else                           !verlet
         do j=1,np
          a=(c(j-1)*(x(j-1)-x(j))+c(j)*(x(j+1)-x(j)))/mass(j)
          xnew(j)=x(j)+v(j)*h+0.5d0*a*h**2 
         end do
         do j=1,np
          a=(c(j-1)*(x(j-1)-x(j))+c(j)*(x(j+1)-x(j)))/mass(j)
      anew=(c(j-1)*(xnew(j-1)-xnew(j))+c(j)*(xnew(j+1)-xnew(j)))/mass(j)
          vnew(j)=v(j)+0.5d0*h*(a+anew)
         end do
*----Actualizamos los vectores de velocidad y posicion de la particula j
         do j=1,np
          x(j)=xnew(j)
          v(j)=vnew(j)
         end do
      end if
      
      return
      end

*     feigenbaumlogistica-pjla.f
************************************************************************
*
*     Programacion del diagrama de Feigenbaum haciendo
*     uso de la ecuacion logistica
*
*
*     Pedro Jesus Lopez Abenza                     01-02-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer n,i,nr,k,m,lmax,bmax,bif,iseed
      parameter(n=1000)
      parameter(m=5000)
      parameter(k=4)
      dimension x(n),sol(n),bif(m)
      character iter*4,caso*1
      write(iter,'(i4)') n
      write(caso,'(i1)') k
      
      pi=4.0d0*datan(1.0d0)
      
*inicializacion del vector con los valores de x
      do i=1,n
          x(i)=0.0d0
      end do
      
*valores de tolerancia para la diferencia entre valores encontrados como solucion
*para afinar lo obtenido, tomamos dos tolerancias según el valor del parámetro r
*TODO ELLO PARA EL CASO ORIGINAL (PARA LOS OTROS HABRA QUE MODIFICAR LOS RANGOS
      tolinf=1.0d-2       !buena para separar r.le.0.75
      tolsup=0.5d-4       !buena para separar r.gt.0.75

*definicion de avance de la constante r entre 0 y 1      
      r0=0.0d0
      if (k.ne.4) then
      r1=1.0d0
      else
      r1=2.0d0*pi
      end if
      hr=(r1-r0)/real(m)

*definimos la semilla para hacer aleatorio el valor inicial
      iseed=-484545881
      
*inicializacion de las soluciones encontradas
      lmax=0

*bucle principal
      open(10,file='feigenbaum-'//caso//'-'//iter//'.dat')
      write(10,*) '# r                 ptos ciclicos  '
      do nr=0,m
       l=1
       r=r0+real(nr)*hr
*---valor inicial del x inicial en la ecuación logística
       x(1)=ran2(iseed)
       do i=1,n-1
*-------Realizamos el calculo iterativo de la ecuacion elegida
         if (k.eq.1) x(i+1)=4.0d0*r*x(i)*(1-x(i))
         if (k.eq.2) x(i+1)=x(i)*exp(4.0d0*r*(1-x(i)))
         if (k.eq.3) x(i+1)=r*(1-(2.0d0*x(i)-1.0d0)**4)
         if (k.eq.4) x(i+1)=r*sin(x(i))
*-------Grabamos las últimas iteraciones realizadas 
         if (i.gt.0.95*n) then            
            sol(l)=x(i+1)
*-------Guardamos las soluciones si cumplen el criterio establecido segun el valor de r
*-------Tomamos como criterio que la diferencia de una solución con respecto a cualquiera
*-------de las soluciones anteriores sea menor a una cierta tolerancia definida
           if (l.eq.1) then
                write(10,*) r,sol(l),l
                lmax=max(lmax,l)
                bif(nr)=lmax
           elseif (l.eq.2) then
                if (r.le.0.75 .and. abs(sol(l)-sol(1)).gt.tolinf) then !criterio:si una solucion difiere de otra en una cierta diferencia 'tol', la guardamos como solucion no-repetida
                       write(10,*) r,sol(l),l
                       lmax=max(lmax,l)
                       bif(nr)=lmax
                end if
                if (r.gt.0.75 .and. abs(sol(l)-sol(1)).gt.tolsup) then 
                       write(10,*) r,sol(l),l
                       lmax=max(lmax,l)
                       bif(nr)=lmax
                end if     
           elseif (l.gt.2) then
              do j=1,l-1              
                if (r.le.0.75) then
                   if (abs(sol(l)-sol(j)).gt.tolinf) then 
                     continue
                   else
                     go to 100
                   end if
                end if
                if (r.gt.0.75) then
                   if (abs(sol(l)-sol(j)).gt.tolsup) then 
                     continue
                   else
                     go to 100
                   end if
                end if
              end do 
*-------Si la nueva solucion no ha mostrado una diferencia menor a la tolerancia
*-------establecida según el valor de r, la guardamos como solucion no repetida
             write(10,*) r,sol(l),l
             lmax=max(lmax,l)
             bif(nr)=lmax
           end if              
100       l=l+1                
         end if
       end do
      end do
      close(10)

*bucle para obtener el valor donde se obtiene el maximo numero de soluciones no repetidas
      bmax=0
      do nr=0,m
          bmax=max(bmax,bif(nr))
      end do

*bucle que busca el primer valor de r que presenta el maximo de soluciones no repetidas
      do nr=0,m
        if (bif(nr).eq.bmax) then
           write(*,*) 'el valor de r donde se alcanza el comportamiento 
     &caotico es=',r0+real(nr)*hr
           go to 500
        end if
      end do
      
500   write(*,*) 'programa finalizado'
      stop
      end

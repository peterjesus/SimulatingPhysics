*     cadenavibratoria-pjla.f
************************************************************************
*
*     Programacion de la vibracion de una cadena lineal de atomos
*     de una cierta masa m con los metodos
*                        -metodo euler 
*                        -metodo euler-cromer
*                        -metodo euler-mejorado
*                        -metodo verlet
*
*
*     Pedro Jesus Lopez Abenza                     01-02-2017
************************************************************************

      
      include 'iterarvibra-pjla.f'
      include 'ran2-r8.f'
      integer ntime,np,k,i,j,l,iseed
      parameter(k=1,np=2)
      real*8 ecin,etot,epot
      real*8 t,t0,tf,h,mass,x,v,c,ran2
      dimension x(0:np+1),v(0:np+1),c(0:np),mass(0:np+1)
      character metodo*1
      write(metodo,'(i1)') k
      
*definimos la semilla para hacer aleatorias las elongaciones iniciales
      iseed=-484545881

*definimos las constantes del problema: masas y ctes amortiguamiento
      do l=0,np+1
         mass(l)=2.0d0
      end do
      
      c(0)=1.0d0
      c(1)=2.0d0
      c(2)=1.0d0
      
*inicializamos las energias
      ecin=0.0d0
      epot=0.0d0
      etot=0.0d0
      
*definimos los valores inicial y final de tiempo y a partir de ellos el paso
      t0=0.0
      tf=150.0
      ntime=10000
      h=(tf-t0)/real(ntime)
      t=t0

*abrimos el archivo
      open(10,file='posiciones-metodo'//metodo//'.dat')
      open(20,file='velocidades-metodo'//metodo//'.dat')
      open(30,file='energia-metodo'//metodo//'.dat')
      write(30,*) ' # tiempo                          total             
     &        cinetica                    potencial'
      open(40,file='fases-metodo'//metodo//'.dat')
      
*definimos las condiciones iniciales de los extremos fijos
      x(0)=0.0d0
      x(np+1)=0.0d0
      v(0)=0.0d0
      v(np+1)=0.0d0
      
*definimos las elongaciones y la velocidad iniciales, asi el valor de las
*energias potencial, cinetica y total de dichos instantes
      do j=1,np
       x(j)=ran2(iseed)/10.0d0   !elongacion inicial
       v(j)=0.0d0                 !velocidad
       epot=epot+0.5*c(j-1)*(x(j)-x(j-1))**2
       ecin=ecin+0.5*mass(j)*v(j)**2
       etot=epot+ecin
       if (j.eq.np) then
         epot=epot+0.5d0*c(j)*(x(np+1)-x(np))**2
         ecin=ecin+0.5d0*mass(np+1)*v(np+1)**2
         etot=epot+ecin 
       end if
      end do
      write(10,*) t,(x(i), i=1,np)
      write(20,*) t,(v(i), i=1,np)
      write(30,*) t,etot,ecin,epot
      write(40,*) (x(i), v(i), i=1,np)
      
*bucle principal
      do i=1,ntime
        t=t0+h*real(i)
*-----Reinicializamos la medida de las energías y el cmasas en cada instante
        epot=0.0d0
        ecin=0.0d0
*-----Llamamos a nuestra subrutina
        call iterarvibra(k,np,h,c,mass,x,v)
*-----Escribimos en los ficheros instante,posicion y velocidad para las particulas elegidas
        write(10,*) t,(x(j), j=1,np)
        write(20,*) t,(v(j), j=1,np)
        write(40,*) (x(j),v(j), j=1,np)
        do j=1,np
*-----Definimos los nuevos valores de la energia y los escribimos
           epot=epot+0.5d0*c(j-1)*(x(j)-x(j-1))**2
           ecin=ecin+0.5*mass(j)*v(j)**2
           etot=epot+ecin   
*-----Energia de la particula final
           if (j.eq.np) then
              epot=epot+0.5d0*c(np)*(x(np+1)-x(np))**2
              ecin=ecin+0.5d0*mass(np+1)*v(np+1)**2
              etot=epot+ecin 
           end if
        end do
        write(30,*) t,etot,ecin,epot
      end do
      close(10)
      close(20)
      close(30)
      close(40)
      
      write(*,*) 'programa finalizado'
      stop
      end      

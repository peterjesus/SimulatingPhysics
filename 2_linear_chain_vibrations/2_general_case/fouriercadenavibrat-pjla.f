*     fouriercadenavibrat-pjla.f
************************************************************************
*
*     Programacion del calculo numerico de la periodicidad en la
*     vibracion de una particula de una cadena lineal de particulas
*     haciendo uso de la transformada de Fourier*
*
*     Pedro Jesus Lopez Abenza                      13-02-2017
************************************************************************

      include 'dfour1.f'
      implicit real*8 (a-h,o-z)
      integer n,m,isign,k,ntime
      parameter(m=20)         !Definimos el número de datos de forma que sea una
      parameter(n=2**m)      !potencia de 2. Cogemos menos datos de los disponibles
      dimension fr(0:n-1),fi(0:n-1),data(1:2*n),gr(0:n-1),gi(0:n-1)
      
*Señalamos que buscamos realizar la FT directa y el numero de datos del fichero
      isign=1
      
*Definimos el paso de avance en tiempos de acuerdo al fichero y calculamos
*el paso de avance de frecuencias
      ntime=n
      dt=0.0025
      dv=1.0d0/(n*dt)
      
*Inicializamos la parte real e imaginaria de la señal muestreada
      do k=0,n-1
         fr(k)=0.0d0
         fi(k)=0.0d0
      end do

*Leemos los datos del archivo de posiciones o velocidades y los asociamos a la
*parte real de nuestra señal
      open(10,file='posiciones-metodo4.dat')
      !open(10,file='velocidades-metodo4.dat')
      do k=0,n-1
        read (10,*) t,x1,x2,x3,x4,x5,x6,x7,x8,x9
         fr(k)=x7  !Para particula i-esima
         fi(k)=0.0 
      end do
      close (10)
      
*Convertimos las parejas de las funciones real e imaginaria de la señal en
*la secuencia de datos a transformar
      do i=1,(2*n-1),2      !impares
         ind=(i-1)/2        !todos
         data(i)=fr(ind)
         data(i+1)=fi(ind)
      end do
      
*Llamamos a la subrutina encargada de realizar la transformada de fourier
      call dfour1(data,n,isign)
      
*Convertimos la secuencia de datos que nos ha devuelto la subrutina en las
*partes real e imaginaria de la transformada de fourier
      do i=1,(2*n-1),2      !impares
         ind=(i-1)/2        !todos
         gr(ind)=data(i)    
         gi(ind)=data(i+1)
      end do

*calculamos el espectro de potencia de las frecuencias positivas, siempre para
*valores por debajo de la frecuencia de Nyquist. Obtenemos frecuencia y potencia
      open(20,file='fourier-posiciones7.dat')
      !open(20,file='fourier-velocidades9.dat')   
      do j=0,n/2-1
         v=j*dv
         pot=gr(j)**2+gi(j)**2
         write(20,*) v,pot
      end do
      close(20)
      
      write(*,*) 'frecuencia de Nyquist =',1.0/(2.0*dt)
      write(*,*)
      write(*,*) 'programa finalizado'
      stop
      end

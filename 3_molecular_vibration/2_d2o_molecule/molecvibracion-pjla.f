*     molecvibracion-pjla.f
************************************************************************
*
*     Programacion de la vibracion de una molécula de agua pesada
*
*
*     Pedro Jesus Lopez Abenza                     14-02-2017
************************************************************************

      include 'iterarvibramol-pjla.f'
      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer i,nmol
      parameter(nmol=3)
      dimension x(nmol),y(nmol),vx(nmol),vy(nmol),xp(nmol),yp(nmol)
      dimension dmass(nmol)

************************************************************************
********     ETIQUETAS DE LOS ATOMOS DE LA MOLECULA DE AGUA     ********
*   D1-> Particula 1
*   O -> Particula 2
*   D2-> Particula 3
************************************************************************
     
      pi=4.0d0*datan(1.0d0)
      iseed=-484545885
     
*Comenzamos definiendo las constantes del problema en unidades atomicas,
*las cuales han sido obtenidas del libro Suhm & Watts, en su edicion de 1991
      dis0=1.809d0      
      alpha0=104.45d0*(pi/180.0d0)   
      ctel=0.5371d0               !cte longitudinal
      ctea=0.0477d0               !cte angular

*Definiendo las masas de los atomos de nuestra molecula en unidades atomicas
      dmass(1)=3692.21d0
      dmass(2)=29152.33d0
      dmass(3)=3692.21d0
      tmass=dmass(1)+dmass(2)+dmass(3)
            
      open(10,file='centromasas.dat')
      open(20,file='posicionesx.dat')
      open(30,file='posicionesy.dat')
      open(40,file='energias.dat')
      
*Definimos las posiciones de los atomos de nuestra molecula
*Lo hacemos para que el centro de masa inicial este en el origen
      x(1)=dis0*dsin(alpha0/2.0d0)
      y(1)=-dis0*dcos(alpha0/2.0d0)*(dmass(2)/tmass)
      x(2)=0.0d0       
      y(2)=dis0*dcos(alpha0/2.0d0)*(1.0d0-dmass(2)/tmass)
      x(3)=-dis0*dsin(alpha0/2.0d0) 
      y(3)=-dis0*dcos(alpha0/2.0d0)*(dmass(2)/tmass)
      xcmasas=(dmass(1)*x(1)+dmass(2)*x(2)+dmass(3)*x(3))/tmass
      ycmasas=(dmass(1)*y(1)+dmass(2)*y(2)+dmass(3)*y(3))/tmass
      cmasas=dsqrt(xcmasas**2+ycmasas**2)
      write(*,*) xcmasas,ycmasas,cmasas
      
*Definimos que las perturbaciones de las particulas y calculamos el nuevo centro de masas
      xp(1)=x(1)+ran2(iseed)*0.1d0
      yp(1)=y(1)+ran2(iseed)*0.1d0
      xp(3)=x(3)+ran2(iseed)*0.1d0
      xp(2)=-(dmass(1)*xp(1)+dmass(3)*xp(3))/dmass(2)     
      yp(3)=(x(2)-x(1))/(x(3)-x(2))*dmass(1)/dmass(3)*yp(1)+(y(1)-y(2))/
     & (x(3)-x(2))*dmass(1)/dmass(3)*xp(1)+xp(3)*(y(3)-y(2))/(x(3)-x(2))
      yp(2)=-(dmass(1)*yp(1)+dmass(3)*yp(3))/dmass(2)
      xcmasas=(dmass(1)*xp(1)+dmass(2)*xp(2)+dmass(3)*xp(3))/tmass
      ycmasas=(dmass(1)*yp(1)+dmass(2)*yp(2)+dmass(3)*yp(3))/tmass
      cmasas=dsqrt(xcmasas**2+ycmasas**2)
      write(*,*) xcmasas,ycmasas,cmasas

*Definimos que las velocidades de las particulas inicialmente son nulas
      do i=1,nmol
         vx(i)=0.0d0
         vy(i)=0.0d0
      end do 
      
*definimos los valores inicial y final de tiempo y a partir de ellos el paso
      t0=0.0d0
      nn=20
      ntime=2**nn
      h=0.05
      
*bucle principal
      do i=0,ntime
        t=t0+h*real(i)
*-----Reinicializamos la medida de las energías y el cmasas en cada instante
        epot=0.0d0
        ecin=0.0d0
        xcmasas=0.0d0  
        ycmasas=0.0d0  
        cmasas=0.0d0  
*----Llamamos a nuestra subrutina
        call iterarvibramol(nmol,h,dmass,xp,yp,vx,vy,alpha,alphan,d12,
     &                      d32,d12n,d32n)
        write(20,*) t, (xp(j), j=1,nmol)
        write(30,*) t, (yp(j), j=1,nmol)
*----Definimos los nuevos valores de la energia        
        xcmasas=(dmass(1)*xp(1)+dmass(2)*xp(2)+dmass(3)*xp(3))/tmass
        ycmasas=(dmass(1)*yp(1)+dmass(2)*yp(2)+dmass(3)*yp(3))/tmass
        cmasas=dsqrt(xcmasas**2+ycmasas**2)
        write(10,*) t,cmasas,xcmasas,ycmasas
*----Definimos los nuevos valores de la energia
        do j=1,nmol
           epot=epot+0.5d0*ctea*(alphan-alpha)**2+0.5*ctel*(d12n-d12)**2
     &         +0.5*ctel*(d32n-d32)**2
           ecin=ecin+0.5*dmass(j)*(vx(j)**2+vy(j)**2)
        end do
        etot=epot+ecin
        write(40,*) t,etot,ecin,epot  
      end do
      close(10)
      close(20)
      close(30)
      close(40)
      
      stop
      end

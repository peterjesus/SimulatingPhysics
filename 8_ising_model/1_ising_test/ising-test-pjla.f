*     ising-pjla.f
************************************************************************
* 
*   programa para simular el modelo de ising en una red bidimensional
*
*    Pedro Jesus Lopez Abenza                            14-03-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer si,sj,ndown,nup,energy,k,kmtc
      parameter(n=30)
      dimension m(0:n+1,0:n+1)
      character dimens*2
      write(dimens,'(i2)') n
                 
      iseed=-484545881

*Inicializacion de la energia y definicion de la J
      energy=0
      l=1
      x=1.0d2 !1.0d-2

*Definimos las exponenciales para la probabilidad     
      p=0.5d0
      pneg2=dexp(-4.0d0*dble(l)/x)    
      pneg4=dexp(-8.0d0*dble(l)/x)

*Inicializacion de la matriz con todos los espines en la misma direccion
*o en direcciones aleatorias, segun el valor de la temperatura
      
      if(x.gt.1.0d0) then
        open(10,file='ising-topspin-'//dimens//'-altatemp.dat')
        open(20,file='ising-downspin-'//dimens//'-altatemp.dat')
        do i=1,n
           do j=1,n
             aleat=ran2(iseed)
             if(aleat.ge.p) then
                m(i,j)=1
             else
                m(i,j)=-1
             endif
           end do
        end do
        do k=1,n
           m(0,k)=m(n,k)
           m(k,0)=m(k,n)
           m(n+1,k)=m(1,k)
           m(k,n+1)=m(k,1)
        end do
      elseif(x.lt.1.0d0) then
        open(10,file='ising-topspin-'//dimens//'-bajatemp.dat')
        open(20,file='ising-downspin-'//dimens//'-bajatemp.dat')
        do i=0,n+1
          do j=0,n+1
            m(i,j)=1
          end do
        end do
        m(0,0)=0
        m(n+1,0)=0
        m(0,n+1)=0
        m(n+1,n+1)=0
      endif
         
*Bucle:Paso de montecarlo
      do kmtc=1,n*n

*Aleatorio
         aleat=ran2(iseed)      
  
*Elegimos el sitio al azar
         si=int(ran2(iseed)*n+1)
         sj=int(ran2(iseed)*n+1)
            
*Analizamos si el cambio energetico es favorable mirando a los vecinos        
        energy=-l*m(si,sj)*(m(si-1,sj)+m(si+1,sj)+m(si,sj+1)+m(si,sj-1))

         if(energy.gt.0) then
             m(si,sj)=-m(si,sj)
         elseif(energy.eq.-2*l) then
           if(aleat.lt.pneg2) then
               m(si,sj)=-m(si,sj)
           else
               m(si,sj)=m(si,sj)
           endif
         elseif(energy.eq.-4*l) then
           if(aleat.lt.pneg4) then
               m(si,sj)=-m(si,sj)
           else
               m(si,sj)=m(si,sj)
           endif
         elseif(energy.eq.0) then
           if(aleat.lt.p) then
               m(si,sj)=-m(si,sj)
           else
               m(si,sj)=m(si,sj)
           endif
         endif

*Actualizamos las condiciones de contorno periodicas
         do k=1,n
            m(0,k)=m(n,k)
            m(k,0)=m(k,n)
            m(n+1,k)=m(1,k)
            m(k,n+1)=m(k,1)
         end do
        
      end do
            
      do i=1,n
         do j=1,n
             if(m(i,j).eq.1) then
                 write(10,*) i,j              
             elseif(m(i,j).eq.-1) then
                 write(20,*) i,j
             endif
         end do
      end do
      
      close(10)
      close(20)

*Calculamos la magnetizacion para ver la direccion de los espines
      ndown=0
      nup=0
      do i=1,n
         do j=1,n
             if (m(i,j).eq.-1) then
                 ndown=ndown+1
             elseif (m(i,j).eq.1) then
                 nup=nup+1
             endif
         end do
      end do
      dmag=abs(dble(nup-ndown)/dble(nup+ndown))
      write(*,*) nup,ndown,dmag
      
      stop
      end

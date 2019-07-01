*     ising-temp-pjla.f
************************************************************************
* 
*   programa para calcular la magnetizacion y energia media en una
*   red bidimensional que sigue el modelo de ising
*
*    Pedro Jesus Lopez Abenza                            14-03-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer si,sj,ndown,nup,energy,k,kmtc,ktemp,ntemp
      parameter(n=50,ntemp=100,nsim=10000)
      dimension m(0:n+1,0:n+1)
      character dimens*2
      write(dimens,'(i2)') n

      open(10,file='ising-magnet-'//dimens//'.dat')
      open(20,file='ising-energy-'//dimens//'.dat')
      
      iseed=-1579054411
      iseedi=-784818292
      iseedj=-284569433
      
*Definimos la temperatura inicial, la temperatura final y el paso del barrido
      t0=0.0d0
      tf=6.0d0
      htemp=(tf-t0)/dble(ntemp)

*Inicializacion de la energia y definicion de la J y la probabilidad del
*sorteo de sitios
      l=1
      p=0.5d0

*Inicializacion de la matriz con todos los espines en la misma direccion
         do i=0,n+1
           do j=0,n+1
              m(i,j)=1
           end do
         end do
         m(0,0)=0
         m(n+1,0)=0
         m(0,n+1)=0
         m(n+1,n+1)=0
      
*Bucle principal:Barrido en temperaturas
      do ktemp=0,ntemp
      
*---Definimos la temperatura considerada y el valor de la beta
        t=t0+ktemp*htemp

*---Definimos las exponenciales para la probabilidad     
        pneg2=dexp(-4.0d0*dble(l)/t)    
        pneg4=dexp(-8.0d0*dble(l)/t)  

*---Inicializamos los contadores valor medio y valor medio al cuadrado
*---para la magnetizacion y la energia para cada temperatura
        smag=0.0d0
        smag2=0.0d0        
        senerg=0.0d0
        senerg2=0.0d0

*---Realizamos un cierto numero de simulaciones para cada temperatura
        do ksim=1,nsim
          
*-----Paso de montecarlo
         do kmtc=1,n*n
            
*-------Aleatorio
              aleat=ran2(iseed)
      
*-------Elegimos el sitio al azar
              si=int(ran2(iseedi)*n+1)
              sj=int(ran2(iseedj)*n+1)
            
*-------Analizamos si el cambio energetico es favorable mirando a los vecinos       
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

*-------Actualizamos las condiciones de contorno periodicas
              do k=1,n
                 m(0,k)=m(n,k)
                 m(k,0)=m(k,n)
                 m(n+1,k)=m(1,k)
                 m(k,n+1)=m(k,1)
              end do            

         end do

*----Calculo de la magnetizacion y de la energia de la configuracion obtenida

          if(ksim.gt.100) then  
            ndown=0
            nup=0
            dmag=0.0d0
            dener=0.0d0
            do i=1,n
              do j=1,n
        dener=dener-dble(l*m(i,j)*(m(i-1,j)+m(i+1,j)+m(i,j+1)+m(i,j-1)))
                if (m(i,j).eq.-1) then
                    ndown=ndown+1
                elseif (m(i,j).eq.1) then
                    nup=nup+1
                endif
              end do
            end do
            dmag=abs(dble(nup-ndown)/dble(nup+ndown))
            smag=smag+dmag
            smag2=smag2+dmag**2
            senerg=senerg+dener
            senerg2=senerg2+dener**2
          end if
          
        end do
        
*---Obtenemos la magnetizacion media y su error
        smagg=smag/dble(nsim-100)
        smagg2=smag2/dble(nsim-100) 
        emagg=dsqrt(smagg2-smagg**2)/dsqrt(dble(nsim-100))

*---Obtenemos la energia media y su error
        senergg=senerg/dble(nsim-100)
        senergg2=senerg2/dble(nsim-100) 
      energg=dsqrt(senergg2-senergg**2)/dsqrt(dble(nsim-100))/dble(n**2)
        write(10,*) t,smagg,emagg
        write(20,*) t,senergg/n**2,energg
      
      end do
      
      close(10)
      close(20)
      
      call cpu_time(time)
      write(*,*) n,time
            
      stop
      end

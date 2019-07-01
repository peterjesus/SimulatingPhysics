*     percola-pjla.f
************************************************************************
* 
*   programa para estudiar la percolacion de un fuego en un bosque
*  
*
*    Pedro Jesus Lopez Abenza                            28-02-2017
************************************************************************

       include 'ran2-r8.f'
       implicit real*8 (a-h,o-z)
       integer iseed,n,nsim,npasos,l,i,j,k
       parameter(k=1,n=90,nsim=10000)
       dimension mtx(0:n+1,0:n+1)
       character dimens*2,caso*1

       write(dimens,'(i2)') n
       write(caso,'(i1)') k
       
       open(20,file='probperc-'//dimens//'-'//caso//'.dat')

*definimos nuestra semilla y el barrido de probabilidad de incendio
       iseed=-484545888
       if(k.eq.1) then
       p0=0.1d0
       pf=0.9d0
       npasos=50
       elseif(k.eq.2) then
       p0=0.585d0
       pf=0.60d0
       npasos=10
       endif
       h=(pf-p0)/real(npasos)

*bucle con el barrido
      do ipasos=1,npasos
         pocup=p0+real(ipasos)*h
*---inicializamos la percolacion
         perc=0.0d0   
*---bucle de simulaciones para un mismo valor de p
         do l=1,nsim
*---inicializamos la matriz
          do i=0,n+1
            do j=0,n+1
              mtx(i,j)=0
            end do
          end do
*---definimos la capa virtual y la capa final
          do i=0,n+1
             mtx(i,0)=1
             mtx(i,n+1)=4
          end do 
          iflag=1       !inicialmente como 1
*---bucle que barre los elementos de la matriz varias veces
          do while(iflag.eq.1) 
            iflag=0
            do j=0,n 
              do i=1,n
*----condiciones periodicas en el eje Y
                mtx(n+1,j)=mtx(1,j)
                mtx(0,j)=mtx(n,j)
*----analizamos las distintas situaciones
                if (mtx(i,j).eq.1) then
                  iflag=1
                  if(mtx(i,j+1).eq.4) then
                     perc=perc+1.0d0
                     go to 100
                  endif
                  if(mtx(i-1,j).eq.0) then
                         aleat=ran2(iseed)
                     if (aleat.ge.pocup) then
                         mtx(i-1,j)=2
                     else
                         mtx(i-1,j)=1
                     end if
                  endif
                  if(mtx(i+1,j).eq.0) then
                     aleat=ran2(iseed)
                     if (aleat.ge.pocup) then   
                          mtx(i+1,j)=2
                     else
                          mtx(i+1,j)=1
                     end if
                  endif
                  if(mtx(i,j-1).eq.0) then
                     aleat=ran2(iseed)
                     if (aleat.ge.pocup) then    
                          mtx(i,j-1)=2
                     else
                          mtx(i,j-1)=1
                     end if
                  endif
                  if(mtx(i,j+1).eq.0) then
                     aleat=ran2(iseed)
                     if (aleat.ge.pocup) then    
                          mtx(i,j+1)=2
                     else
                          mtx(i,j+1)=1
                     end if
                  endif
                 mtx(i,j)=3   !el frente activo pasa a ser 3
                endif
              end do 
            end do
          end do
          
100     end do
        pperc=perc/nsim        
        varp=dsqrt(pocup*(1-pocup)/nsim)
        write(20,*) pocup,pperc,varp
      end do
      close(20)
      
      stop
      end
    

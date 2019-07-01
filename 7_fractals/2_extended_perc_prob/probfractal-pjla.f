*     probfractal-pjla.f
************************************************************************
* 
*    programa para calcular la probabilidad critica mediante el
*    calculo de las trayectorias cerradas en nuestro fractal
*
*    Pedro Jesus Lopez Abenza                            07-03-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer n,iseed,nsim,nv,lcase
      integer cont,nsteps,sright,sleft,sextend,nrep,nlon
      parameter(n=300,nprob=200,nsim=10000,nmoves=10000000,lcase=1)
      dimension mtx(1:n,1:n),mv(1:4,1:2)
      character dimens*3,caso*1
      write(dimens,'(i3)') n
      write(caso,'(i1)') lcase
      
      open(10,file='fractal-probcritica-'//dimens//'-'//caso//'.dat')
      open(20,file='fractal-problong-'//dimens//'-'//caso//'.dat')

*definimos una semilla
      iseed=-484545882

*definimos el intervalo de probabilidades para realizar el barrido
      if(lcase.eq.1) then
         p0=0.2d0
         pf=0.9d0
         hprob=(pf-p0)/real(nprob)
      elseif(lcase.eq.2) then
         p0=0.585d0
         pf=0.6d0
         hprob=(pf-p0)/real(nprob)
      endif
       
*definicion del valor inicial
      if(mod(n,2).eq.0) then
        mstart=n/2
      else
        mstart=n/2+1
      endif
      
*definicion de la matriz del movimiento y la direccion inicial
                  !mv(direccioninicial, tipodesitio)=direccionresultante
      mv(1,1)=2
      mv(1,2)=3
      mv(2,1)=3
      mv(2,2)=4
      mv(3,1)=4
      mv(3,2)=1
      mv(4,1)=1
      mv(4,2)=2
      
*inicializamos el num de trayectorias extendidas contadas, para buscar el
*maximo, asi como la probabilidad critica para las extendidas y las cerradas
      extendpeak=0
      pcextend=0.0d0
      
*bucle principal:barrido probabilidades
      do kprob=1,nprob
      
       p=p0+kprob*hprob    
       
*---inicializamos los contadores de giro a derecha, de giro izquierda y
*---extendidas, asi como el numero de simulaciones validas y la longitud
       sright=0
       sleft=0
       sextend=0
       nrep=0
       nlon=0

*---realizamos un cierto numero de simulaciones
       do l=1,nsim
        
*-----inicializacion de la matriz
        do i=1,n
            do j=1,n
               mtx(i,j)=0
            end do
        end do       
       
*-----inicializamos la posicion y la velocidad inicial
        i=mstart   
        j=mstart 
        mtx(i,j)=1
        vstart=1
        v0=vstart
         
*-----inicializamos el contador de giro a derecha y de marcha atras y el
*-----del numero de pasos contados
        nsteps=0
        cont=0
        
        do k=1,nmoves
         
*-------definimos nuestro aleatorio
         aleat=ran2(iseed)
*-------guardamos la direccion utilizada para aplicar el cambio en la matriz de giro
         nv=int(vstart)
*-------condicion: Situacion inicial = Para
         if (k.ne.1 .and. nv.eq.v0) then
            if (i.eq.mstart .and. j.eq.mstart) then
              go to 200
            endif
          endif
*-------avance segun la direccion de movimiento
         if(vstart.eq.1) then                   !ARRIBA
          if(i.eq.1) then
           if(mtx(n,j).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                mtx(n,j)=2
                isol=2
                cont=cont-2
             elseif (aleat.lt.p) then    !GIRO
                isol=1
                mtx(n,j)=1
                cont=cont+1
             endif
           elseif(mtx(n,j).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(n,j).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=n
            j=j
          else
           if(mtx(i-1,j).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                mtx(i-1,j)=2
                isol=2
                cont=cont-2
             elseif (aleat.lt.p) then    !GIRO
                isol=1
                mtx(i-1,j)=1
                cont=cont+1
             endif
           elseif(mtx(i-1,j).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i-1,j).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i-1
            j=j
          endif
         endif
         if(vstart.eq.2) then                   !DCHA
          if(j.eq.n) then                 
           if(mtx(i,1).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,1)=2
                cont=cont-2
             else                   !GIRO
                isol=1
                mtx(i,1)=1
                cont=cont+1
             endif
           elseif(mtx(i,1).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i,1).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i
            j=1
          else
           if(mtx(i,j+1).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,j+1)=2
                cont=cont-2
             else                   !GIRO
                isol=1
                mtx(i,j+1)=1
                cont=cont+1
             endif
           elseif(mtx(i,j+1).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i,j+1).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i
            j=j+1
          endif
         endif
         if(vstart.eq.3) then                   !ABAJO
          if(i.eq.n) then
           if(mtx(1,j).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(1,j)=2
                cont=cont-2
             else                   !GIRO
                isol=1
                mtx(1,j)=1
                cont=cont+1
             endif
           elseif(mtx(1,j).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(1,j).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=1
            j=j
          else
           if(mtx(i+1,j).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i+1,j)=2
                cont=cont-2
             else                    !GIRO
                isol=1
                mtx(i+1,j)=1
                cont=cont+1
             endif
           elseif(mtx(i+1,j).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i+1,j).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i+1
            j=j
          endif 
         endif
         if(vstart.eq.4) then                   !IZQDA
          if(j.eq.1) then               
           if(mtx(i,n).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,n)=2
                cont=cont-2
             else                   !GIRO
                isol=1
                mtx(i,n)=1
                cont=cont+1
             endif
           elseif(mtx(i,n).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i,n).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i
            j=n
          else
           if(mtx(i,j-1).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,j-1)=2
                cont=cont-2
             else    !GIRO
                isol=1
                mtx(i,j-1)=1
                cont=cont+1
             endif
           elseif(mtx(i,j-1).eq.1) then
                isol=1
                cont=cont+1
           elseif(mtx(i,j-1).eq.2) then
                isol=2
                cont=cont-2
           endif
            i=i
            j=j-1
          endif
         endif

*-------Actualizamos el numero de pasos
         nsteps=nsteps+1
         nlon=nlon+1
*-------Definimos la nueva direccion
         vstart=mv(nv,isol)
        end do
        
200     if(nsteps.gt.100) then
           nrep=nrep+1
           if(cont.eq.4) then
             sright=sright+1           
           elseif(cont.eq.-4) then
             sleft=sleft+1
           elseif(cont.eq.0) then 
             sextend=sextend+1
           else
             write(*,*) 'warning'
           endif
         endif  
           
       end do
       
       tleft=real(sleft)/real(nrep)
       tright=real(sright)/real(nrep)
       textend=real(sextend)/real(nrep)

*---busqueda de la probabilidad critica
       if(lcase.eq.2) then
          if(sextend.gt.extendpeak) then
              extendpeak=sextend
              pcextend=p
          endif
       endif
       write(10,*) p,tleft,tright,textend
       write(20,*) p,real(nlon)/real(nsim)
       
      end do
 
      close(10)
      close(20)
      
      if(lcase.eq.2) then
           write(*,*)  'pc en base a tray extendidas = ',pcextend
      endif
            
      stop
      end

*     percfractal-pjla.f
************************************************************************
* 
*   programa para simular el camino de percolacion de un fractal
*
*    Pedro Jesus Lopez Abenza                            07-03-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer n,iseed,nmoves,nv
      parameter(n=30,nmoves=1000000)
      dimension mtx(1:n,1:n),mv(1:4,1:2)
      character dimens*2
      write(dimens,'(i2)') n
       
      open(10,file='percfractal1-prob07-'//dimens//'.dat')
      open(20,file='percfractal2-prob07-'//dimens//'.dat')
      
*inicializacion de la semilla y la probabilidad p
      iseed=-1853345832
      p=0.7
      
*inicializacion de la posicion central
      if(mod(n,2).eq.0) then
          mstart=n/2
      else
          mstart=n/2+1
      endif
       
*inicializacion de la matriz del movimiento y la direccion inicial
      mv(1,1)=2
      mv(1,2)=3
      mv(2,1)=3
      mv(2,2)=4
      mv(3,1)=4
      mv(3,2)=1
      mv(4,1)=1
      mv(4,2)=2
       
*inicializacion de la matriz
      do i=1,n
        do j=1,n
           mtx(i,j)=0
        end do
      end do

*inicializacion del valor inicial y de la direccion inicial
      i=mstart
      j=mstart      !Posiciones iniciales centrales
      mtx(i,j)=1    !Valor de la matriz en dicha posicion
      vstart=1      !Inicialmente hacia arriba
      v0=vstart

*bucle principal: camino percolacion
      do nrep=1,nmoves
      
         aleat=ran2(iseed)
         nv=int(vstart)         !guardamos la velocidad en nv
 
*----condicion: Situacion inicial = Para         
         if (nrep.ne.1 .and. nv.eq.v0) then
           if (i.eq.mstart .and. j.eq.mstart) then
              go to 100
           endif
         endif

*----posibles avances            
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
             elseif (aleat.lt.p) then    !GIRO
                isol=1
                mtx(i,n)=1
             endif
           elseif(mtx(i,n).eq.1) then
                isol=1
           elseif(mtx(i,n).eq.2) then
                isol=2
           endif
            i=i
            j=n
          else
           if(mtx(i,j-1).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,j-1)=2
             elseif (aleat.lt.p) then    !GIRO
                isol=1
                mtx(i,j-1)=1
             endif
           elseif(mtx(i,j-1).eq.1) then
                isol=1
           elseif(mtx(i,j-1).eq.2) then
                isol=2
           endif
            i=i
            j=j-1
          endif
         endif

         vstart=mv(nv,isol)        !actualizacion de la velocidad
      end do
      
*añadimos la localizacion de los puntos recorridos en la percolacion
      
100   do i=1,n
         do j=1,n
             if(mtx(i,j).eq.1) then
                 write(10,*) i,j                
             elseif(mtx(i,j).eq.2) then
                 write(20,*) i,j
             endif
         end do
      end do
    
      close(10)
      close(20)
      
      stop
      end
      

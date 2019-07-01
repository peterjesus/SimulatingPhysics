*     dimfractal-pjla.f
************************************************************************
* 
*   programa para calcular la dimension fractal
*
*    Pedro Jesus Lopez Abenza                            07-03-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer n,iseed,nsim,nv,cont,nsteps,nrep
      real*8 nstepstot,nstepstot2
      parameter(n=500,nsim=10000,nmoves=10000000)
      dimension mtx(1:n,1:n),mv(1:4,1:2)
      character dimens*3
      write(dimens,'(i3)') n
       
      open(10,file='fractal-extendways-'//dimens//'.dat')
      
*definimos nuestra semilla y la probabilidad critica en base a la practica anterior
      iseed=-484545882
      p=0.59274d0
       
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

*inicializamos el numero de pasos totales conseguidos y el de simulaciones validas
      nstepstot=0.0d0
      nstepstot2=0.0d0
      nrep=0
      
*bucle simulaciones
      do l=1,nsim
       
*---inicializacion de la matriz
        do i=1,n
           do j=1,n
              mtx(i,j)=0
           end do
        end do       
       
*---inicializamos la posicion y la velocidad inicial
        i=mstart   
        j=mstart      !Posiciones iniciales centrales
        mtx(i,j)=1    !Valor de la matriz en dicha posicion
        vstart=1
        v0=vstart
        
*---reiniciamos los contadores de giro a derecha y de marcha atras y del
*---numero de pasos contados
        nsteps=0
        cont=0
         
        do k=1,nmoves
         
*----definimos nuestro aleatorio
         aleat=ran2(iseed)
*----guardamos la direccion utilizada para aplicar el cambio en la matriz de giro
         nv=int(vstart)
*----condicion: Situacion inicial = Para
         if (k.ne.1 .and. nv.eq.v0) then
           if (i.eq.mstart .and. j.eq.mstart) then
             go to 200
           endif
         endif
         
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
          if(j.eq.1) then               !IZQDA
           if(mtx(i,n).eq.0) then
             if (aleat.ge.p) then   !ATRÁS
                isol=2
                mtx(i,n)=2
                cont=cont-2
             elseif (aleat.lt.p) then    !GIRO
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
             elseif (aleat.lt.p) then    !GIRO
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

*----actualizamos el numero de pasos realizados
         nsteps=nsteps+1
*----Definimos la nueva direccion
         vstart=mv(nv,isol)
        end do
              
200     write(10,*) l, cont, nsteps
        if (cont.eq.0 .and. nsteps.gt.100) then
            nrep=nrep+1                            !añadimos al numero de simulaciones validas
            nstepstot=nstepstot+real(nsteps)       !añadimos al total de pasos realizados
            nstepstot2=nstepstot2+real(nsteps)**2  !añadimos al total de pasos realizados
        end if       
      end do
       
*obtenemos el numero medio de pasos y lo sacamos a pantalla
       stepsmed=nstepstot/real(nrep)
       stepsmed2=nstepstot2/real(nrep)
       estepsmed=dsqrt(stepsmed2-stepsmed**2)/dsqrt(dble(nrep))
       write(*,*) n,stepsmed,estepsmed
      
      close(10)
      
      stop
      end
      


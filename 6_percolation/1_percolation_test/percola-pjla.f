*     percola-pjla.f
************************************************************************
* 
*   programa para calcular la propagacion de un fuego en un bosque
*
*    Pedro Jesus Lopez Abenza                            21-02-2017
************************************************************************

       include 'ran2-r8.f'
       implicit real*8 (a-h,o-z)
       integer iseed,n
       parameter(n=10)
       dimension mtx(0:n+1,0:n+1)
       character dimens*2
       write(dimens,'(i2)') n

       open(10,file='percolacion.dat')
       open(20,file='perc-matrix-'//dimens//'.dat')
       
*definimos nuestra semilla y nuestra probabilidad de incendio
       iseed=-45454542
       pocup=0.63d0
       
*---inicializamos la matriz y definimos la capa virtual y la capa final
      do i=0,n+1
        do j=0,n+1
           mtx(i,j)=0
        end do
      end do
      do i=1,n
          mtx(i,0)=1
          mtx(i,n+1)=4
      end do
*---bucle que barre los elementos de la matriz varias veces
      iflag=1       !inicialmente como 1
      do while(iflag.eq.1) 
        iflag=0
        do j=0,n+1
         do i=1,n
*----condiciones periodicas en eje Y
           mtx(n+1,j)=mtx(1,j)
           mtx(0,j)=mtx(n,j)
*----analizamos las distintas situaciones
           if (mtx(i,j).eq.1) then
               iflag=1
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
                  end if
               mtx(i,j)=3
           end if
         end do   
        end do
        
        do i=1,n
          write(10,*) (mtx(i,j), j=1,n)
        end do
        write(10,*)
       
      end do

       
      do i=1,n+1
        do j=1,n
           write(20,*) i,j,mtx(j,i)
        end do
      end do    
       
      close(10)
      close(20)
      
      stop
      end
      

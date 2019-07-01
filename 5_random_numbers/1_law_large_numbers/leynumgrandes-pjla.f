*     leynumerosgrandes-pjla.f
************************************************************************
* 
*   programa para comprobar numéricamente la ley de los números
*   grandes. Tomamos N cajas y tiramos M bolitas
*
*    Pedro Jesus Lopez Abenza                            21-02-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer nball,nbox,j,i
      parameter (n=200)
      parameter (nbox=100)   !numero de cajas que tenemos (fijas)
      dimension caja(nbox)
            
*tomamos nuestra semilla
      iseed=-484545881
      
      open(10,file='leynumgrandes-pjla.dat')
      
*bucle principal
      do k=0,n
       nball=100+250*k
*----inicializamos el numero de particulas por caja
       do i=1,nbox
         caja(i)=0.0d0
       end do
*----asociamos a la bola tirada a una cajita 
       do i=1,nball
*----calculamos el valor aleatorio asociado a la pelotita
         xball=ran2(iseed)
*----lo pasamos a entero según el número de cajas
         xent=xball*nbox+1
*----vemos a que caja está asociada segun su valor
         do j=1,nbox
           if (xent.gt.j .and. xent.le.(j+1)) then
              caja(j)=caja(j)+1
              go to 100
           end if
         end do
100    end do

*inicializamos los sumatorios de x y x**2
       s=0.0d0
       s2=0.0d0      
*añadimos valores para el calculo de la media de x y de x**2
       do i=1,nbox
          s=s+caja(i)
          s2=s2+caja(i)**2
       end do

       sm=s/dble(nbox)   
       s2m=s2/dble(nbox)       
       var=dsqrt(s2m-sm**2)
       
       write(10,*) nball,sm,var
      
      end do
      close(10)
             
      stop
      end

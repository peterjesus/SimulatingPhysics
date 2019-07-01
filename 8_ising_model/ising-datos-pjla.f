*     ising-datosutiles-pjla.f
************************************************************************
* 
*   programa para obtener la magnetizacion en las distintas regiones
*   de interes para el caso del modelo de ising
*
*    Pedro Jesus Lopez Abenza                            14-03-2017
************************************************************************

      implicit real*16 (a-h,o-z)
      character*2 dimens
      
      temp=0.0d0
      dmag=0.0d0
      emag=0.0d0
      
      t1=2.0d0
      t2=2.5d0
      tcrit=2.269d0
      
      do i=1,5
      
      n=i*10   
         
      write(dimens,'(i2)') n
      open(10,file='ising-magnet-'//dimens//'.dat')
      open(20,file='ising-linmagnet-'//dimens//'.dat')
      
        do k=0,100
          read (10,*) temp,dmag,emag
          if(temp.ge.t1 .and. temp.le.t2) then
            write(20,*) temp,dmag,emag
          endif
        end do
        
      close(10)
      close(20)
      close(30)
      close(40)

      end do
      

      open(50,file='ising-magnet-50.dat')
      open(70,file='ising-lnmagnet-50.dat')
      
      do k=0,100
           read (50,*) ttemp,ddmag,eemag
       if(ttemp.le.tcrit .and. log(1.0d0-(ttemp/tcrit)).le.-1.0d0) then
           write(70,*) log(1.0d0-(ttemp/tcrit)),log(ddmag),eemag
       endif
      end do
      
      close(50)   
      close(70)
      
      stop
      end

*     ajtccv-pjla.f
************************************************************************
* 
*   programa para escribir datos de ajustes lineales del modelo ising
*
*    Pedro Jesus Lopez Abenza                            28-05-2017
************************************************************************

       implicit real*8 (a-h,o-z)
       
       parameter(n=5)
       dimension tam(n),tcrit(n)
       
       open(10,file='ajtccv.dat')
       do i=1,5
          tam(i)=dble(i*10)
       end do
       
       tcrit(1)=2.3399999999999999
       tcrit(2)=2.3399999999999999
       tcrit(3)=2.2799999999999998
       tcrit(4)=2.2799999999999998
       tcrit(5)=2.2799999999999998
       
       do i=1,5
          write(10,*) tam(i),1.0d0/tam(i)**2,tcrit(i)
       end do
       
       close(10)
       
       stop
       end

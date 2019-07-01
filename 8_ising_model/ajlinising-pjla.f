*     ajlinising-pjla.f
************************************************************************
* 
*   programa para escribir datos de ajustes lineales del modelo ising
*
*    Pedro Jesus Lopez Abenza                            28-04-2017
************************************************************************


       implicit real*8 (a-h,o-z)
       parameter(n=6)
       dimension tam(n),tcrit(n),etcrit(n)
       dimension pend(n),epend(n),orig(n),eorig(n)
       
       open(10,file='ajlinising.dat')
       do i=1,5
          tam(i)=dble(i*10)
       end do
       
       pend(1)=-0.752058
       epend(1)=0.05931
       orig(1)=2.44016
       eorig(1)=0.1295
       pend(2)=-1.17329
       epend(2)=0.08954
       orig(2)=3.30904
       eorig(2)=0.1945
       pend(3)=-1.70807
       epend(3)=0.1734 
       orig(3)=4.43805
       eorig(3)=0.3794
       pend(4)=-1.63215
       epend(4)=0.2304
       orig(4)=4.29774
       eorig(4)=0.5047       
       pend(5)=-1.86199
       epend(5)=0.2224
       orig(5)=4.77561
       eorig(5)=0.4897
       
       do i=1,5
          tcrit(i)=-orig(i)/pend(i)
          etcrit(i)=abs(1.0d0/pend(i))*eorig(i)+abs(orig(i)/pend(i)**2)*
     &epend(i)
          write(10,*) tam(i),1.0d0/tam(i)**2,tcrit(i),etcrit(i)
       end do
       
       close(10)
       
       stop
       end
       

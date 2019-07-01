*     ajlinperc-pjla.f
************************************************************************
* 
*   programa para realizar ajustes lineales de percolacion
*
*    Pedro Jesus Lopez Abenza                            28-04-2017
************************************************************************
       
       implicit real*8 (a-h,o-z)
       dimension tam(8),pend(8),epend(8),orig(8),eorig(8)
       dimension ptam(8),top(8),etop(8),bot(8),ebot(8)

       open(10,file='ajlinperc.dat')
       
       do i=1,8
          tam(i)=dble(i*10)
       end do
       
       pend(1)=4.00281
       epend(1)=0.4031
       orig(1)=-1.84437
       eorig(1)=0.2392
       pend(2)=7.67949
       epend(2)=0.3568
       orig(2)=-4.03941
       eorig(2)=0.2117
       pend(3)=10.4571
       epend(3)=0.3413
       orig(3)=-5.68641
       eorig(3)=0.2025
       pend(4)=11.9964
       epend(4)=0.3102
       orig(4)=-6.60544
       eorig(4)=0.184
       pend(5)=14.8839
       epend(5)=0.2667
       orig(5)=-8.31634
       eorig(5)=0.1582
       pend(6)=16.08
       epend(6)=0.2486
       orig(6)=-9.027
       eorig(6)=0.1475       
       pend(7)=17.7968
       epend(7)=0.2542
       orig(7)=-10.0442
       eorig(7)=0.1508
       pend(8)=20.5002
       epend(8)=0.2059
       orig(8)=-11.6506
       eorig(8)=0.1221
       
       do i=1,8
          write(10,*) tam(i),log(tam(i)),pend(i),log(pend(i)),epend(i),
     &  orig(i),eorig(i)
       end do
       
       close(10)
       
       open(20,file='ajlinprobtop.dat')
       open(30,file='ajlinprobbot.dat')
       
       do i=1,8
       ptam(i)=tam(i)**(-0.75)
       top(i)=(0.56-orig(i))/pend(i)
       etop(i)=abs((orig(i)-0.56)/pend(i)**2)*epend(i)+abs(-1/pend(i))*e
     & pend(i)
       bot(i)=(0.48-orig(i))/pend(i)
       ebot(i)=abs((orig(i)-0.48)/pend(i)**2)*epend(i)+abs(-1/pend(i))*e
     & pend(i)
       enddo
       do i=1,8
          write(20,*) ptam(i),top(i),etop(i)
          write(30,*) ptam(i),bot(i),ebot(i)    
       end do
       close(20)
       close(30)
       stop 
       end

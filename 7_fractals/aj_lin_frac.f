*     ajlinfrac-pjla.f
************************************************************************
* 
*   programa para realizar ajustes lineales de fractales
*
*    Pedro Jesus Lopez Abenza                            28-04-2017
************************************************************************
       
       include 'linfit.f'
       implicit real*8 (a-h,o-z)
       parameter(n=6)
       dimension tam(6),ptam(6),pleft(6),epleft(6),pright(6),epright(6)
       dimension oleft(6),eoleft(6),oright(6),eoright(6),pc(6),epc(6)
       
       open(10,file='ajlinfrac.dat')
       
       do i=1,6
          tam(i)=dble(i*50)
       end do
       
       pleft(1)=-16.1853
       epleft(1)=0.1814
       oleft(1)=9.96176
       eoleft(1)=0.1075
       pright(1)=16.2921
       epright(1)=0.1976
       oright(1)=-9.28662
       eoright(1)=0.1171
       
       pleft(2)=-24.9435
       epleft(2)=0.2715
       oleft(2)=15.1777
       eoleft(2)=0.1609
       pright(2)=25.1955
       epright(2)=0.2849
       oright(2)=-14.5387
       eoright(2)=0.1688
       
       pleft(3)=-31.2309
       epleft(3)=0.3751
       oleft(3)=18.9182
       eoleft(3)=0.2223
       pright(3)=30.8935
       epright(3)=0.3249
       oright(3)=-17.9007
       eoright(3)=0.1926
       
       pleft(4)=-36.0285
       epleft(4)=0.4192
       oleft(4)=21.7724
       eoleft(4)=0.2484
       pright(4)=35.8336
       epright(4)=0.444
       oright(4)=-20.8203
       eoright(4)=0.2631
       
       pleft(5)=-39.9756
       epleft(5)=0.527
       oleft(5)=24.1206
       eoleft(5)=0.3123
       pright(5)=39.5891
       epright(5)=0.5482
       oright(5)=-23.0361
       eoright(5)=0.3249
       
       pleft(6)=-42.751
       epleft(6)=0.6312
       oleft(6)=25.7724
       eoleft(6)=0.3741
       pright(6)=42.5081
       epright(6)=0.6638
       oright(6)=-24.7602
       eoright(6)=0.3934
       
       do i=1,6
          ptam(i)=tam(i)**(-0.75)
          pc(i)=(oleft(i)-oright(i))/(pright(i)-pleft(i))
          epc(i)= eoleft(i)*abs(1.0d0/(pright(i)-pleft(i))) + 
     & eoright(i)*abs(-1.0d0/(pright(i)-pleft(i))) + 
     & epleft(i)*abs((oright(i)-oleft(i))/(pright(i)-pleft(i))**2) + 
     & epright(i)*abs((oleft(i)-oright(i))/(pright(i)-pleft(i))**2)     
       end do
       
       do i=1,6
          write(10,*) ptam(i),pc(i),epc(i)
       end do
       
       close(10)

       stop
       end
       

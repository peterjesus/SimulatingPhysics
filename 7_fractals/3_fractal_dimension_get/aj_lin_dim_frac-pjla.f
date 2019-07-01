*     ajlindimfrac-pjla.f
************************************************************************
* 
*   programa para realizar ajustes lineales de percolacion
*
*    Pedro Jesus Lopez Abenza                            28-04-2017
************************************************************************
       
       implicit real*8 (a-h,o-z)
       real*8 numpasmed
       dimension tam(8),numpasmed(8),enumpasmed(8)
       
       do i=1,6
          tam(i)=dble(50*i)
       end do
       tam(7)=dble(400)
       tam(8)=dble(500)
       
       open(10,file='ajlindimfrac.dat')
       
       numpasmed(1)=2060.5330739299611
       enumpasmed(1)=18.191704816821257       
       numpasmed(2)=6925.1312977099233
       enumpasmed(2)=67.660492018501984
       numpasmed(3)=13866.337683523654
       enumpasmed(3)=136.56912833706082    
       numpasmed(4)=22987.942257217848
       enumpasmed(4)=241.66259520409201       
       numpasmed(5)=33995.663663663661
       enumpasmed(5)=361.55070743142727
       numpasmed(6)=46867.065989847717
       enumpasmed(6)=521.98211649410348
       numpasmed(7)=77579.607973421924
       enumpasmed(7)=881.89021092800601
       numpasmed(8)=116183.26153846153
       enumpasmed(8)=1337.9076106679568
       
       do i=1,8
         write(10,*) log(tam(i)),log(numpasmed(i)),enumpasmed(i),
     & numpasmed(i),tam(i)
       end do
       
       close(10)
       
       stop
       end
       
       

*     transpart-pjla.f
************************************************************************
* 
*    programa para simular el movimiento de neutrones con una energia
*    de 100 KeV en un material biologico 
*
*    Pedro Jesus Lopez Abenza                            27-03-2017
************************************************************************
        
        include 'ran2-r8.f'
        implicit real*8 (a-h,o-z)
        integer iseedcol,iseedchi,iseedeta,iseedpart
        integer nproy,nsteps,npart
        parameter(nproy=10000,nsteps=10000000,ndat=15)
        dimension pmu(4),pmass(4),thetalab(0:nsteps),philab(0:nsteps)
        character*2 num
        
        pi=4.0d0*datan(1.0d0)
      
*Definimos las semillas necesarias
        iseedcol=-25844621
        iseedpart=-118964723
        iseedchi=-484545883
        iseedeta=-58964723

*Definimos masas (en unidades atomicas) y la probabilidad acumulada de
*cada tipo de particulas
        pmu(1)=0.842d0        !Hidrogeno
        pmass(1)=1.00794d0
        pmu(2)=0.95d0         !Oxigeno
        pmass(2)=15.9994d0
        pmu(3)=0.994d0        !Carbono
        pmass(3)=12.0111d0
        pmu(4)=1.0d0          !Nitrogeno
        pmass(4)=14.0067d0
        
*Definimos la mu total, la masa y la energia inicial del neutron
        pmutot=0.92315d0
        emass=1.0086649156d0 
        e0=100.0d3 !eV       

*Valor limite de la energia de los proyectiles
        elim=100.0d0 !eV
        
*Bucle principal
        do i=1,nproy
        
*---Creamos los archivos con los datos de cada proyectil
          if(i.le.ndat) then
          write(num,'(i2)') i
          open(10,file='transpart-'//num//'-pjla.dat')
          endif
          
       
*---Definimos las coordenadas y angulos iniciales del proyectil
          x=0.0d0
          y=0.0d0
          z=0.0d0
          thetalab(0)=0 !pi/2.0d0
          philab(0)=0   !pi/2.0d0
          e=e0
          
          if(i.le.ndat) then
          write(10,*) x,y,z,e,e0-e
          endif
          
*---Bucle con los pasos de avance del proyectil
          do j=1,nsteps

*-----Distancia aleatoria limite para el recorrido de la particula.
*-----Obtenemos así el tipo de particula con la que colisiona
            pcol=ran2(iseedcol)
            dist=-log(pcol)/pmutot
            
            ranpart=ran2(iseedpart)      
            if(ranpart.lt.pmu(1)) then
              npart=1
            elseif(ranpart.ge.pmu(1) .and. ranpart.lt.pmu(2)) then
              npart=2
            elseif(ranpart.ge.pmu(2) .and. ranpart.lt.pmu(3)) then
              npart=3
            else
              npart=4
            endif

*-----Definimos los angulos polar y acimutal en el sistema centro de masas
            chi=dacos(1.0d0-2.0d0*ran2(iseedchi))
            eta=2.0d0*pi*ran2(iseedeta)
            chir=pi-chi
            etar=pi+eta

*-----Los pasamos al sistema solidario de cada colision de acuerdo a las
*-----relaciones geometricas consideradas
            thetaf1=(emass/pmass(npart))/dsin(chi)
            thetaf2=dcos(chi)/dsin(chi)    
            theta=datan(1.0d0/(thetaf1+thetaf2))
            thetar=chir/2.0d0
            phi=eta
            phir=etar

*-----Calculamos la nueva energia y vemos donde se ha depositado
            divener=(emass+pmass(npart))**2
            er=4.0d0*e*emass*pmass(npart)*dcos(thetar)**2/divener
            e=e-er
            
*-----Controlamos cuando tiene que parar el sistema
            if(e.lt.elim) then
              go to 100             
            end if
            
*-----Definimos los angulos en el sistema laboratorio de acuerdo a los
*-----anteriores y las relaciones geometricas que los relacionan

           theta1=-dsin(theta)*dcos(phi)*dsin(thetalab(j-1))
           theta2=dcos(theta)*dcos(thetalab(j-1)) 
           thetadat=theta1+theta2   
           thetalab(j)=dacos(thetadat)
           
        phi1=dsin(theta)*dcos(phi)*dcos(philab(j-1))*dcos(thetalab(j-1))
        phi2=-dsin(theta)*dsin(phi)*dsin(philab(j-1))
        phi3=dcos(theta)*dsin(thetalab(j-1))*dcos(philab(j-1))
           phidat=(phi1+phi2+phi3)/dsin(thetalab(j))
           philab(j)=dacos(phidat)
                  
           x=x+dist*dcos(thetalab(j))
           y=y+dist*dsin(thetalab(j))*dcos(philab(j))
           z=z+dist*dsin(thetalab(j))*dsin(philab(j))
           
           if(i.le.ndat) then
              write(10,*) x,y,z,e,e0-e
           endif
           
          end do
          
100       if(i.le.ndat) then
             close(10)  
          endif
            
        end do
        
        stop
        end        

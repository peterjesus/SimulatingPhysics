*     transprops-elec-pjla.f
************************************************************************
* 
*    programa para simular el movimiento de electrones con una energia
*    de keV en un material monoatomico 
*
*    Pedro Jesus Lopez Abenza                            27-03-2017
************************************************************************
        
        include 'ran2-r8.f'
        implicit real*8 (a-h,o-z)
        integer zcase
        parameter(zcase=2)
        parameter(nsteps=10000000,nproy=100000,ndz=1000)
        dimension zmax(nproy),absorbel(ndz)

        open(20,file='transelect-transm.dat')
        open(30,file='transelect-absorc.dat')
        
        pi=4.0d0*datan(1.0d0)
        iseedthet=-884545481
        iseedphi=-484545881
        iseedlib=-148454588
        
*definimos la distancia maxima a recorrer por nuestro (en micrometros)
        z0=1.0d-3
        zf=15.0d0
        hz=(zf-z0)/dble(ndz)
        
*inicializamos los vectores a utilizar en el programa        
        do k=0,nproy
          zmax(k)=0.0d0
        end do
        do k=1,ndz
          absorbel(k)=0.0d0
        end do
        
*Definimos a0, hbar y masa del electron en unidades del SI, pues las utilizamos
*para definir la constante adimensional beta
        a0=5.2917720859d-11  !m
        hbar=6.62606957d-34/(2.0d0*pi) !Js
        emass=9.10938356d-31 !kg

*Valores inicial y limite de la energia de los proyectiles
        e0=30.0d0  !keV        
        elim=1.0d0 !keV
        
*Elegimos el tipo de material a tratar segun lo indicado en zcase (1=oro,2=alum)
*Definimos num.atomico, masa atomica y densidad en las unidades adecuadas
        if(zcase.eq.1) then
           zz=79.0d0
           atommass=196.96657d0       !g (por mol)
           rho=19.3d0 !g/cm3
        elseif(zcase.eq.2) then
           zz=13.0d0
           atommass=26.981539d0       !g (por mol)
           rho=2.6984d0 !g/cm3                                       
        endif
        rec0=(zz**(1.0d0/3.0d0))/(a0*0.855d0)
        
*Bucle principal
        do i=1,nproy
         
*-Definimos las coordenadas y angulos iniciales del proyectil
         x=0.0d0
         y=0.0d0
         z=0.0d0
         thetan=0.0d0                   !Avance en la direccion del eje Z,
         phin=0.0d0                     !perpendicular al plano YX
         e=e0

*-Bucle con los pasos de avance del proyectil
         do ks=1,nsteps

*---Obtenemos los numeros aleatorios que nos permiten estudiar nuestro problema
           rantheta=ran2(iseedtheta)
           ranphi=ran2(iseedphi) 
           ranlibre=ran2(iseedlib)
                      
*---Definimos el factor beta
           t=e*1.602176565d-16   !J
           bet=0.25d0*(1.12d0*rec0*hbar)**2/(2.0d0*emass*t)      !adimensional
           
*---Distancia aleatoria limite para el recorrido de la particula          
           rlib=1.02d0*bet*atommass*(1.0d0+bet)*e**2/(zz*(zz+1.0d0)*rho)  !um
           dist=-dlog(ranlibre)*rlib  !um
           
*---Calculamos la perdida de energia cinetica en dicho trayecto libre
           dtds=-7.83d0*(rho*zz/e/atommass)*dlog(174.0d0*e/zz)  !keV/um
           ecloss=dist*abs(dtds)  !KeV
           e=e-ecloss             !KeV
                              
           if(e.lt.elim) then
               zmax(i)=z
               do k=0,ndz-1       
                   zant=dble(k)*hz
                   zpos=dble(k+1)*hz
                   if(z.gt.zant .and. z.le.zpos) then
                       absorbel(k+1) = absorbel(k+1) + 1.0d0
                   endif
               end do 
               go to 100             
           end if
                      
*---Definimos los angulos polar y azimutal del scattering aleatorios
          theta=dacos(1.0d0-(2.0d0*bet*rantheta/(1.0d0+bet-rantheta)))
          phi=2.0d0*pi*ranphi

*---Angulos de scattering en el sistema laboratorio
          t1=dcos(thetan)*dcos(theta)
          t2=dsin(thetan)*dsin(theta)*dcos(phi)
          thetann=dacos(t1+t2)           
          p1=dcos(theta)-dcos(thetann)*dcos(thetan)
          if(ks.eq.1) then
          phinn=phin
          else
          phinn=phin+datan((dsin(thetan)*dsin(theta)*dsin(phi))/p1)     !con tangente
          endif
          !phinn=phin+dacos(p1/(dsin(thetan)*dsin(thetann)))             !con coseno
          !phinn=phin+dasin((dsin(theta)*dsin(phi))/dsin(thetann))       !con seno
          
*---En caso de que la energía sea válida calculamos las nuevas coordenadas
           x=x+dist*dsin(thetann)*dcos(phinn)   !um
           y=y+dist*dsin(thetann)*dsin(phinn)   !um
           z=z+dist*dcos(thetann)               !um
           
*---Si el electron sale de la muestra paramos
           if(z.lt.0.0d0) then
              go to 100
           endif

*---Actualizamos angulos para la siguiente iteracion           
           thetan=thetann
           phin=phinn
        
         end do
         
100     end do

        do l=1,ndz
           dz=z0+dble(l)*hz
           transm=0.0d0
           do k=1,nproy
               if(zmax(k).gt.dz) then
                  transm=transm+1.0d0
               endif
           end do
           write(20,*) dz,transm/dble(nproy)
        end do
        do k=0,ndz-1
            zant=dble(k)*hz
            zpos=dble(k+1)*hz
            zzz=(zant+zpos)/2.0d0   !Promediado
           write(30,*) zzz,absorbel(k+1)/dble(nproy)
        end do
        
        stop
        end        

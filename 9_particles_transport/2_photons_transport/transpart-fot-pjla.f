*     transpart-fot-pjla.f
************************************************************************
* 
*    programa para simular el movimiento de fotones con una energia
*    de 1 MeV en un material monoatomico 
*
*    Pedro Jesus Lopez Abenza                            27-03-2017
************************************************************************
        
        include 'ran2-r8.f'
        implicit real*8 (a-h,o-z)
        integer nsteps,ntiradas,ns,zat
        parameter(zcase=1)
        parameter(nsteps=1000000,ntiradas=10000,ns=13,nproy=1,ndat=25)
        dimension q(0:ns),b0(0:ns),b1(0:ns),b2(0:ns),f(0:ns),fds(0:1000)
        character*2 num
        
        open(20,file='datosphotocross.dat')
        
*Definicion de las constantes del problema (Sistema Internacional)
        pi=4.0d0*datan(1.0d0)
        re=2.82d-15 !m
        c=3.0d8     !m/s
        alpha=1.0d0/137.03599911d0
        emass=9.10938291d-31    !kg
        
*Elegimos el tipo de material a tratar segun lo indicado en zcase (1=oro,2=alum)
*Definimos num.atomico, masa atomica y densidad
        if(zcase.eq.1) then
           zat=79
           atommass=3.2707065d-25 !kg
           rho=19300.0d0 !kg/m3
        elseif(zcase.eq.2) then
           zat=13
           atommass=4.4803895d-26 !kg
           rho=2698.4d0 !kg/m3                   
        endif
        densat=rho/atommass      !m**(-3)
        zz=real(zat)
        coefz=alpha*zz
        
*Lectura de datos del fichero para luego calcular la sec.ef.fotoelectrica
        do j=0,ns
            read(20,*) q(j),b0(j),b1(j),b2(j),f(j)
        enddo
        close(20)
        
        iseedtheta=-884545481
        iseedphi=-484545881
        iseedlib=-148454588
        iseedpro=-188454548
        
*Valores inicial y limite de la energia de los proyectiles
        e0ev=1.0d6 !eV
        e0=e0ev*1.602176565d-19 !J       
        elim=1.0d4 !eV

*Bucle principal
        do i=1,nproy
        
*-Creamos los archivos con los datos de cada proyectil
         if(i.le.ndat) then
         write(num,'(i2)') i
         open(10,file='transelect-'//num//'.dat')
         endif
         
*-Definimos las coordenadas y angulos iniciales del proyectil
         x=0.0d0
         y=0.0d0
         z=0.0d0
         thetan=0.0d0
         phin=0.0d0
         e=e0        !J
         eev=e0ev

         if(i.le.ndat) then
               write(10,*) x,y,z,eev
         endif

*-Bucle con los pasos de avance del proyectil
         do ks=1,nsteps

*---Obtenemos los numeros aleatorios que nos permiten estudiar nuestro problema
          ranphi=ran2(iseedphi)
          ranlibre=ran2(iseedlib)
          ranprocc=ran2(iseedpro)

*---Definimos el valor del la constante gamma en funcion de la energia
          gamma=e/(emass*c**2)     !adimensional
          egev=e/1.602176565d-19   !eV
          
*---Definimos las secciones eficaces asociadas a cada proceso fisico y la total
          akn=(1.0d0+gamma)/gamma**2*(2.0d0*(1.0d0+gamma)/(1.0d0+2.0d0*g
     &amma)-(dlog(1.0d0+2.0d0*gamma))/gamma)
          bkn=dlog(1.0d0+2.0d0*gamma)/(2.0d0*gamma)
          ckn=(1.0d0+3.0d0*gamma)/(1.0d0+2.0d0*gamma)**2
          sigkn=2.0d0*pi*(akn+bkn-ckn)*re**2
          cskn=zz*sigkn
          
          ape=4.0d0*pi*re**2*(coefz)**4*(zz/gamma)
          do j=0,ns-1
             if(coefz.ge.q(j) .and. coefz.lt.q(j+1)) then
                  b00=b0(j)+(b0(j+1)-b0(j))*(coefz-q(j))/(q(j+1)-q(j))
                  b11=b1(j)+(b1(j+1)-b1(j))*(coefz-q(j))/(q(j+1)-q(j))
                  b22=b2(j)+(b2(j+1)-b2(j))*(coefz-q(j))/(q(j+1)-q(j))
                  ff=f(j)+(f(j+1)-f(j))*(coefz-q(j))/(q(j+1)-q(j))
             endif
          enddo
          sigpe=ape*(b00+b11/gamma+b22/gamma**2)*ff
          cspe=sigpe
        
          write(*,*) cspe,cskn
          write(*,*)
          cstot=cspe+cskn     !m**2
          
*---Distancia aleatoria limite para el recorrido de la particula
          rlambda=1.0d0/(cstot*densat)   !m
          dist=-log(ranlibre)*rlambda

*---Cocientes para sortear que proceso tiene lugar y sorteo del mismo
          rlkn=cskn/cstot
          rlpe=cspe/cstot          
          
          if(ranprocc.ge.0.0d0 .and. ranprocc.lt.rlpe) then             !Efecto Fotoelectrico
              
              eev=0.0d0
              write(*,*) 'Efecto fotoelectrico foton en iteracion',ks
              go to 200
              
          else                                                          !Efecto Compton
          
          
          
          
          
          
*---Definimos el angulo polar del scattering aleatorio con el metodo de acept-reject
*---Valores limite de theta=[0,pi] y f(theta)=[0,fmax] donde fmax es conocida para
*---E=1MEv y para E=0.01MEv. Haremos una simple interpolacion en funcion del valor de E
          tmx=0.5+(0.9-0.5)*((egev-1.0d6))/(1.0d4-1.0d6)          
          fc=1.0d0+gamma*(1.0d0-dcos(tmx))
          fn=gamma*(1.0d0-dcos(tmx))
          fthemax=re**2*pi*dsin(tmx)*(1.0d0+dcos(tmx)**2+fn**2/fc)/fc**2
          h=fthemax
          


          hfd=pi/real(1000)
          do ifd=0,1000
               thf=theta0+hfd*i
               fc=1.0d0+gamma*(1.0d0-dcos(thf))
               fn=gamma*(1.0d0-dcos(thf))
          hf(ifd)=re**2*pi*dsin(thf)*(1.0d0+dcos(thf)**2+fn**2/fc)/fc**2
          end do
          
          
          
*-----Definimos nuestro cuadrado y los valores aleatorios x e y de la distrib
*-----uniforme entre 0 y pi y 0 y fmax, respectivamente
             do nfd=0,1000
                nin=0
                do ktir=1,ntiradas
                    ranthetax=ran2(iseedtheta)
                    ranthetay=ran2(iseedtheta)
                    rx=pi*ranthetax
                    ry=h*ranthetay
                    fcx=1.0d0+gamma*(1.0d0-dcos(rx))
                    fnx=gamma*(1.0d0-dcos(rx))
              fx=re**2*pi*dsin(rx)*(1.0d0+dcos(rx)**2+fnx**2/fcx)/fcx**2
                    if(ry.lt.fx) then
                       nin=nin+1
                    endif
                 end do
                 fds=h*pi*nin/ntiradas
             end do
             
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
             
             
*---Definimos el angulo azimutal del scattering aleatorio
              phi=2.0d0*pi*ranphi

*-----Calculamos la nueva energia y comprobamos si es menor al limite establecido
              e=emass*c**2*gamma/(1.0d0+gamma*(1.0d0-dcos(theta))) !J
              eev=e/1.602176565d-19  !eV
              if(eev.lt.elim) then
                  go to 200
              endif
              
*-----Angulos de scattering en el sistema laboratorio
              t1=dcos(thetan)*dcos(theta)
              t2=dsin(thetan)*dsin(theta)*dcos(phi)
              thetann=dacos(t1+t2)           
              p1=dsin(thetan)*dsin(thetann)
              if(ks.eq.1) then
                  phinn=phin
              else
              phinn=phin+datan((dsin(thetan)*dsin(theta)*dsin(phi))/p1)     !con tangente
              endif
              !phinn=phin+dacos(p1/(dsin(thetan)*dsin(thetann)))             !con coseno
              !phinn=phin+dasin((dsin(theta)*dsin(phi))/dsin(thetann))       !con seno

*---En caso de que la energía sea válida calculamos las nuevas coordenadas
              x=x+dist*dsin(thetann)*dcos(phinn)
              y=y+dist*dsin(thetann)*dsin(phinn)
              z=z+dist*dcos(thetann)

*---Si el foton sale de la muestra paramos
              if(z.lt.0.0d0) then
                   go to 200
              endif

*---Escribimos resultados y actualizamos angulos para la siguiente iteracion           
              if(i.le.ndat) then
                  write(10,*) x,y,z,e
              endif
              thetan=thetann
              phin=phinn
          
          endif

         end do
         
200      if(i.le.ndat) then
             close(10)  
         endif

      enddo
        
      stop
      end        

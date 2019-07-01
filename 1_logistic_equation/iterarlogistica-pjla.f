*     iterarlogistica-pjla.f
************************************************************************
*
*     Programacion del avance de la ecuacion logistica a
*     medida que aumentan las iteraciones
*
*
*     Pedro Jesus Lopez Abenza                     01-02-2017
************************************************************************

      integer n,i,k
      parameter(n=1000,k=1,nr=95) 
      real*8 r,x,tol,aer
      dimension x(n)
      character caso*1,rvalue*3

*definimos la variable del número de iteraciones
      write(caso,'(i1)') k
      write(rvalue,'(i3)') nr
      
*inicializacion del vector con los valores de x      
      do i=1,n
          x(i)=0.0d0
      end do

*valor del parámetro r y del x inicial en la ecuación logística
      r=0.01d0*real(nr)
      x(1)=0.55d0
      
      open(10,file='logistica-r'//rvalue//'-x055-'//caso//'-pjla.dat')   
      write(10,*) '#  i      x(i+1)'
      write(10,*)  0,x(1)
*bucle principal: Realizado para obtener las iteraciones de la ecuacion logistica
      do i=1,n-1
*-----Realizamos el calculo iterativo de la ecuacion elegida
        if (k.eq.1) x(i+1)=4.0d0*r*x(i)*(1-x(i))
        if (k.eq.2) x(i+1)=x(i)*exp(4.0d0*r*(1-x(i)))
        if (k.eq.3) x(i+1)=r*(1-(2.0d0*x(i)-1.0d0)**4)
        if (k.eq.4) x(i+1)=r*sin(x(i))
*-----Escribimos en pantalla
          write(10,*) i,x(i+1)
      end do
      close(10)
      
*valor de la tolerancia entre soluciones consecutivas e inicializamos el error absoluto
      tol=0.01d0
      aer=0.0d0

*bucle principal: Repetido para calcular la iteracion en la que converge
      do i=1,n-1
        aer=abs((x(i+1)-x(i))/x(i))
        write(*,*) x(i),x(i+1),aer
        if (aer.le.tol) then
          write(*,*) 'se ha alcanzado la convergencia en la iteracion',i
          go to 100
        end if
      end do
      
100   write(*,*) 'programa finalizado'
      stop
      end

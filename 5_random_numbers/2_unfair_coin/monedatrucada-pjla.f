*     monedatruncada-pjla.f
************************************************************************
* 
*   programa para calcular la probabilidad de obtener cara con
*   una moneda truncada
*
*    Pedro Jesus Lopez Abenza                            21-02-2017
************************************************************************

      include 'ran2-r8.f'
      implicit real*8 (a-h,o-z)
      integer ntiradas,n,s
      parameter (n=5*10**6,npasos=100)
            
*tomamos nuestra semilla
      iseed=-184545888

      open(10,file='mtruc-paseoborr.dat')
      open(20,file='mtruc-vmedtruc.dat')
      
*inicializamos los resultados obtenidos para cada moneda
      gmoney=0.0d0
      tmoney=0.0d0
      gs=0.0d0
      ts=0.0d0
      gs2=0.0d0
      ts2=0.0d0
      
      ntiradas=100
      s=0
      ss=0

*bucle principal
      do i=1,n
      
*----calculamos el valor aleatorio asociado a la tirada
        xmoney=ran2(iseed)
        
*----establecemos si es cara o cruz segun la probabilidad de la moneda
        if (xmoney.lt.0.5d0) then         !MONEDA CON MISMA PROBABILIDAD
            gmoney=gmoney+1.0d0
            gs=gs+1.0d0
            gs2=gs2+1.0d0**2
        else
            gmoney=gmoney-1.0d0
            gs=gs-0.0d0
            gs2=gs2+0.0d0**2
        end if
        
        if (xmoney.lt.0.501d0) then       !MONEDA CON PROBABILIDAD TRUCADA
            tmoney=tmoney+1.0d0
            ts=ts+1.0d0
            ts2=ts2+1.0d0**2
        else
            tmoney=tmoney-1.0d0
            ts=ts-0.0d0
            ts2=ts2+0.0d0**2
        end if
        
        if(i.lt.10**5) then
            write(10,*) i,gmoney,tmoney
        endif

*---Una vez llegamos a la ultima tirada, calculamos la estadistica del problema
        if(i.eq.ntiradas) then
            tmean=ts/dble(ntiradas)    
            t2mean=ts2/dble(ntiradas)
            desvt=dsqrt(t2mean-tmean**2)
            vart=desvt/dsqrt(dble(ntiradas))
            sigmat=3.0d0*vart
            sigmamenos=tmean-sigmat
            sigmamas=tmean+sigmat
            write(20,*) ntiradas,tmean,sigmamas,sigmamenos
           if((tmean-sigmat).ge.0.5d0 .and. ss.ne.1) then
                s=s+1
           else
                s=0
           endif
            if(s.eq.10000) then
               write(*,*) ntiradas-10000*npasos
               ss=1
            endif
            ntiradas=ntiradas+npasos
         end if
      end do
      
      if(ss.ne.1) then
         write(*,*)  'Warning: Aumentar numero tiradas'
      endif
      
      close(10)
      close(20)
      
      stop
      end

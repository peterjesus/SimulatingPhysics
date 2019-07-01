#CADENA LINEAL

##########CASOSIMPLE
###POSICIONES
#EULERSIMPLE
eusimpx=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo1.dat")
eusimpxt=eusimpx[1:nrow(eusimpx),1] #t
eusimpx1=eusimpx[1:nrow(eusimpx),2] #x1
eusimpx2=eusimpx[1:nrow(eusimpx),3] #x2
#EULERCROMER
eucromx=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo2.dat")
eucromxt=eucromx[1:nrow(eucromx),1] #t
eucromx1=eucromx[1:nrow(eucromx),2] #x1
eucromx2=eucromx[1:nrow(eucromx),3] #x2
#EULERMEJORADO
eumejx=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo3.dat")
eumejxt=eumejx[1:nrow(eumejx),1] #t
eumejx1=eumejx[1:nrow(eumejx),2] #x1
eumejx2=eumejx[1:nrow(eumejx),3] #x2
#VERLET
verletx=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo4.dat")
verletxt=verletx[1:nrow(verletx),1] #t
verletx1=verletx[1:nrow(verletx),2] #x1
verletx2=verletx[1:nrow(verletx),3] #x2
plot(eusimpxt,eusimpx1,type="l",ylim=c(-0.3,0.6),lty=c(3),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Posición",main=paste("Posición frente al Tiempo. Partícula 1"),col="green")
lines(eumejxt,eumejx1,type="l",lty=c(2),col="blue")
lines(eucromxt,eucromx1,type="l",lty=c(5),col="yellow")
lines(verletxt,verletx1,type="l",lty=c(1),col="red")
grid()
legend(1,.575,lty=c(c(3),c(2),c(5),c(1)),c("Euler Simple","Euler-Cromer","Euler Mejorado","Verlet"),col=c("green","blue","yellow","red"))
plot(eusimpxt,eusimpx2,type="l",ylim=c(-0.3,0.6),lty=c(3),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Posición",main=paste("Posición frente al Tiempo. Partícula 2"),col="green")
lines(eumejxt,eumejx2,type="l",lty=c(2),col="blue")
lines(eucromxt,eucromx2,type="l",lty=c(5),col="yellow")
lines(verletxt,verletx2,type="l",lty=c(1),col="red")
grid()
legend(1,.575,lty=c(c(3),c(2),c(5),c(1)),c("Euler Simple","Euler-Cromer","Euler Mejorado","Verlet"),col=c("green","blue","yellow","red"))

###VELOCIDADES
#EULERSIMPLE
eusimpv=read.table("/home/pj/Escritorio/simulacion2/casosimple/velocidades-metodo1.dat")
eusimpvt=eusimpv[1:nrow(eusimpv),1] #t
eusimpv1=eusimpv[1:nrow(eusimpv),2] #x1
eusimpv2=eusimpv[1:nrow(eusimpv),3] #x2
#EULERCROMER
eucromv=read.table("/home/pj/Escritorio/simulacion2/casosimple/velocidades-metodo2.dat")
eucromvt=eucromv[1:nrow(eucromv),1] #t
eucromv1=eucromv[1:nrow(eucromv),2] #x1
eucromv2=eucromv[1:nrow(eucromv),3] #x2
#EULERMEJORADO
eumejv=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo3.dat")
eumejvt=eumejv[1:nrow(eumejv),1] #t
eumejv1=eumejv[1:nrow(eumejv),2] #x1
eumejv2=eumejv[1:nrow(eumejv),3] #x2
#VERLET
verletv=read.table("/home/pj/Escritorio/simulacion2/casosimple/posiciones-metodo4.dat")
verletvt=verletv[1:nrow(verletv),1] #t
verletv1=verletv[1:nrow(verletv),2] #x1
verletv2=verletv[1:nrow(verletv),3] #x2
plot(eusimpvt,eusimpv1,type="l",ylim=c(-0.5,1),lty=c(3),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Velocidad",main=paste("Velocidad frente al Tiempo. Partícula 1"),col="green")
lines(eumejvt,eumejv1,type="l",lty=c(2),col="blue")
lines(eucromvt,eucromv1,type="l",lty=c(5),col="yellow")
lines(verletvt,verletv1,type="l",lty=c(1),col="red")
grid()
legend(1,.95,lty=c(c(3),c(2),c(5),c(1)),c("Euler Simple","Euler-Cromer","Euler Mejorado","Verlet"),col=c("green","blue","yellow","red"))
plot(eusimpvt,eusimpv2,type="l",ylim=c(-0.5,1),lty=c(3),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Velocidad",main=paste("Velocidad frente al Tiempo. Partícula 2"),col="green")
lines(eumejvt,eumejv2,type="l",lty=c(2),col="blue")
lines(eucromvt,eucromv2,type="l",lty=c(5),col="yellow")
lines(verletvt,verletv2,type="l",lty=c(1),col="red")
grid()
legend(1,.95,lty=c(c(3),c(2),c(5),c(1)),c("Euler Simple","Euler-Cromer","Euler Mejorado","Verlet"),col=c("green","blue","yellow","red"))


###ENERGIAS
#EULERSIMPLE
eusimpe=read.table("/home/pj/Escritorio/simulacion2/casosimple/energia-metodo1.dat")
eusimpt=eusimpe[1:nrow(eusimpe),1] #t
eusimpetot=eusimpe[1:nrow(eusimpe),2] #etot
#EULERCROMER
eucrome=read.table("/home/pj/Escritorio/simulacion2/casosimple/energia-metodo2.dat")
eucromt=eucrome[1:nrow(eucrome),1] #t
eucrometot=eucrome[1:nrow(eucrome),2] #etot
#EULERMEJORADO
eumeje=read.table("/home/pj/Escritorio/simulacion2/casosimple/energia-metodo3.dat")
eumejt=eumeje[1:nrow(eumeje),1] #t
eumejetot=eumeje[1:nrow(eumeje),2] #etot
#VERLET
verlete=read.table("/home/pj/Escritorio/simulacion2/casosimple/energia-metodo4.dat")
verlett=verlete[1:nrow(verlete),1] #t
verletetot=verlete[1:nrow(verlete),2] #etot

plot(eusimpt,eusimpetot,type="l",ylim=c(0.0025,0.0075),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Energia total"),col="red")
lines(eucromt,eucrometot,type="l",col="blue")
lines(eumejt,eumejetot,type="l",col="green")
lines(verlett,verletetot,type="l",col="orange")
grid()
legend(80,.007,lty=c(1),c("Euler Simple","Euler-Cromer","Euler Mejorado","Verlet"),col=c("red","blue","green","orange"))

plot(eucromt,eucrometot,type="l",ylim=c(0.0029,0.003),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Energia total"),col="purple")
lines(verlett,verletetot,type="l",col="orange")
grid()
legend(80,.002995,lty=c(1),c("Euler-Cromer","Verlet"),col=c("purple","orange"))


###FASES
#EULERSIMPLE
eusimpf=read.table("/home/pj/Escritorio/simulacion2/casosimple/fases-metodo1.dat")
eusimpfx=eusimpf[1:nrow(eusimpf),1] #x
eusimpfv=eusimpf[1:nrow(eusimpf),2] #v
#EULERCROMER
eucromf=read.table("/home/pj/Escritorio/simulacion2/casosimple/fases-metodo2.dat")
eucromfx=eucromf[1:nrow(eucromf),1] #x
eucromfv=eucromf[1:nrow(eucromf),2] #v
#EULERMEJORADO
eumejf=read.table("/home/pj/Escritorio/simulacion2/casosimple/fases-metodo3.dat")
eumejfx=eumejf[1:nrow(eumejf),1] #x
eumejfv=eumejf[1:nrow(eumejf),2] #v
#VERLET
verletf=read.table("/home/pj/Escritorio/simulacion2/casosimple/fases-metodo4.dat")
verletfx=verletf[1:nrow(verletf),1] #t
verletfv=verletf[1:nrow(verletf),2] #etot

plot(eusimpfv,eusimpfx,type="l",xaxs="i",yaxs="i",xlab="Posición",ylab="Velocidad",main=paste("Espacio Fases Euler Simple"),col="red")
grid()
plot(eucromfv,eucromfx,type="l",xaxs="i",yaxs="i",xlab="Posición",ylab="Velocidad",main=paste("Espacio Fases Euler Cromer"),col="blue")
grid()
plot(eumejfv,eumejfx,type="l",xaxs="i",yaxs="i",xlab="Posición",ylab="Velocidad",main=paste("Espacio Fases Euler Mejorado"),col="orange")
grid()
plot(verletfv,verletfx,type="l",xaxs="i",yaxs="i",xlab="Posición",ylab="Velocidad",main=paste("Espacio Fases Verlet"),col="purple")
grid()


###ERIODICIDADES
fft1=read.table("/home/pj/Escritorio/simulacion2/casosimple/fourier-pos-p1.dat")
fftf1=fft1[1:nrow(fft1),1] #f
fftp1=fft1[1:nrow(fft1),2] #p
fft2=read.table("/home/pj/Escritorio/simulacion2/casosimple/fourier-pos-p2.dat")
fftf2=fft2[1:nrow(fft2),1] #f
fftp2=fft2[1:nrow(fft2),2] #p

plot(fftf1,fftp1,xlim=c(0,0.5),ylim=c(0,25000),type="l",xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Transformada Fourier Posiciones Partícula 1"),col="red")
plot(fftf2,fftp2,xlim=c(0,0.5),ylim=c(0,25000),type="l",xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Transformada Fourier Posiciones Partícula 2"),col="blue")








##################CASO GENERAL
###POSICIONES
#import.dataset
t=posiciones.metodo4[1:nrow(posiciones.metodo4),1] #t
x4=posiciones.metodo4[1:nrow(posiciones.metodo4),5]
x7=posiciones.metodo4[1:nrow(posiciones.metodo4),8]
plot(t,x4,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Posición",main=paste("Partícula 4: Posición frente al Tiempo"),col="blue")
plot(t,x7,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Posición",main=paste("Partícula 7: Posición frente al Tiempo"),col="red")

#import.dataset
t=velocidades.metodo4[1:nrow(velocidades.metodo4),1] #t
v2=velocidades.metodo4[1:nrow(velocidades.metodo4),3]
v9=velocidades.metodo4[1:nrow(velocidades.metodo4),10]
plot(t,v2,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Velocidad",main=paste("Partícula 2: Velocidad frente al Tiempo"),col="green")
plot(t,v9,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Velocidad",main=paste("Partícula 9: Velocidad frente al Tiempo"),col="purple")


####ENERGIA
den=read.table("/home/pj/Escritorio/simulacion/simulacion2/casogeneral/energia-metodo4.dat")
tt=den[1:nrow(den),1]
energy=den[1:nrow(den),2]
plot(tt,energy,type="l",ylim=c(0.0035451,0.0035452),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Energia total"),col="red")


####PERIODICIDADES
#import.dataset
frec4=fourier.posiciones4[1:nrow(fourier.posiciones4),1]
potf4=fourier.posiciones4[1:nrow(fourier.posiciones4),2]
plot(frec4,potf4,type="l",col="red",xaxs="i",main=paste("Particula 4 FT Posicion: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,1e7))
locator(5)
plot(frec4,potf4,type="l",col="red",xaxs="i",main=paste("Particula 4 FT Posicion: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,5e3))
locator(4)
#import.dataset
frec7=fourier.posiciones7[1:nrow(fourier.posiciones7),1]
potf7=fourier.posiciones7[1:nrow(fourier.posiciones7),2]
plot(frec7,potf7,type="l",col="blue",xaxs="i",main=paste("Particula 7 FT Posicion: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,5e6))
locator(5)
plot(frec7,potf7,type="l",col="blue",xaxs="i",main=paste("Particula 7 FT Posicion: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,2.5e3))
locator(4)
#import.dataset
frec2=fourier.velocidades2[1:nrow(fourier.velocidades2),1]
potf2=fourier.velocidades2[1:nrow(fourier.velocidades2),2]
plot(frec2,potf2,type="l",col="green",xaxs="i",main=paste("Particula 2 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,5e7))
locator(6)
plot(frec2,potf2,type="l",col="green",xaxs="i",main=paste("Particula 2 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,5e3))
locator(3)
#import.dataset
frec9=fourier.velocidades9[1:nrow(fourier.velocidades9),1]
potf9=fourier.velocidades9[1:nrow(fourier.velocidades9),2]
plot(frec9,potf9,type="l",col="purple",xaxs="i",main=paste("Particula 9 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,1e7))
locator(6)
plot(frec9,potf9,type="l",col="purple",xaxs="i",main=paste("Particula 9 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.6),ylim=c(0,1.5e4))
locator(3)







#######CASO DIATOMICO

###ENERGIA
#import.dataset
tt=energia.metodo4[1:nrow(energia.metodo4),1]
energy=energia.metodo4[1:nrow(energia.metodo4),2]
plot(tt,energy,type="l",ylim=c(0.00662025,0.00662035),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Energía Total",main=paste("Energia total"),col="red")

###POSICIONES
#import.dataset
t=posd[1:nrow(posd),1] #t
x3=posd[1:nrow(posd),4]
plot(t,x3,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Posición",main=paste("Partícula 3: Posición frente al Tiempo"),col="blue")

###VELOCIDADES
#import.dataset
t=veld[1:nrow(veld),1] #t
v5=veld[1:nrow(veld),6]
plot(t,v5,type="l",lty=c(1),xaxs="i",yaxs="i",xlab="Tiempo",ylab="Velocidad",main=paste("Partícula 5: Velocidad frente al Tiempo"),col="red")


###PERIODICIDADES
#import.dataset
frcp3=fpp3[1:nrow(fpp3),1]
potp3=fpp3[1:nrow(fpp3),2]
plot(frcp3,potp3,type="l",col="blue",xaxs="i",main=paste("Particula 3 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.3),ylim=c(0,7e6))
locator(4)
plot(frcp3,potp3,type="l",col="blue",xaxs="i",main=paste("Particula 3 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.3),ylim=c(0,3e4))
locator(2)

#import.dataset
frcp5=fpv5[1:nrow(fpv5),1]
potp5=fpv5[1:nrow(fpv5),2]
plot(frcp5,potp5,type="l",col="red",xaxs="i",main=paste("Particula 5 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.3),ylim=c(0,1e7))
locator(4)
plot(frcp5,potp5,type="l",col="red",xaxs="i",main=paste("Particula 5 FT Velocidad: Espectro de Frecuencias"),xlab="Frecuencia",ylab="Potencia",xlim=c(0,0.3),ylim=c(0,1e5))
locator(2)

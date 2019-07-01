##################MOLECULA H20

###Centro de Masas
#cmm=read.table("/home/pj/Escritorio/simulacion/simulacion3/centromasas.dat")
t=cmm[1:nrow(cmm),1]
cm=cmm[1:nrow(cmm),2]
xcm=cmm[1:nrow(cmm),3]
ycm=cmm[1:nrow(cmm),4]
plot(t,cm,type="l",lwd=2,ylim=c(-0.75e-13,2e-13),xaxs="i",yaxs="i",col="red",xlab="Tiempo",ylab="Separación",main=paste("Evolución temporal del Centro de Masas"))
lines(t,xcm,lwd=2,type="l",col="blue")
lines(t,ycm,lwd=2,type="l",col="purple")
legend(50,1.85e-13,lty=c(1),c("Centro Masas","X Centro Masas","Y Centro Masas"),lwd=2,col=c("red","blue","purple"))

###Energy
#enn=read.table("/home/pj/Escritorio/simulacion/simulacion3/energias.dat")
t=enn[1:nrow(enn),1]
etot=enn[1:nrow(enn),2]
ecin=enn[1:nrow(enn),3]
epot=enn[1:nrow(enn),4]
plot(t,etot,type="l",xlab="Tiempo",ylab="Energía Total",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal de la Energía Total del sistema"))

###Posiciones X
#xx=read.table("/home/pj/Escritorio/simulacion/simulacion3/posicionesx.dat")
t=xx[1:nrow(xx),1]
x1=xx[1:nrow(xx),2]
x2=xx[1:nrow(xx),3]
x3=xx[1:nrow(xx),4]
plot(t,x1,type="l",xlab="Tiempo",ylab="X1",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal X Hidrógeno 1"))
plot(t,x2,type="l",xlab="Tiempo",ylab="X2",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal X Oxígeno"))
plot(t,x3,type="l",xlab="Tiempo",ylab="X3",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal X Hidrógeno 2"))


###Posiciones Y
#yy=read.table("/home/pj/Escritorio/simulacion/simulacion3/posicionesx.dat")
t=yy[1:nrow(yy),1]
y1=yy[1:nrow(yy),2]
y2=yy[1:nrow(yy),3]
y3=yy[1:nrow(yy),4]
plot(t,y1,type="l",xlab="Tiempo",ylab="Y1",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal Y Hidrógeno 1"))
plot(t,y2,type="l",xlab="Tiempo",ylab="Y2",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal Y Oxígeno"))
plot(t,y3,type="l",xlab="Tiempo",ylab="Y3",xaxs="i",yaxs="i",col="red",main=paste("Evolución temporal Y Hidrógeno 2"))


#Periodicidades
#px1=read.table("")
fpx1=px1[1:nrow(px1),1]
ppx1=px1[1:nrow(px1),2]
plot(fpx1,ppx1,type="l",xlim=c(0.0005,0.003),ylim=c(0,7e8),xlab="Frecuencias",ylab="Amplitud",main=paste("Espectro de frecuencias X1"),xaxs="i",yaxs="i",col="red")

#py3=read.table("")
fpy3=py3[1:nrow(py3),1]
ppy3=py3[1:nrow(py3),2]
plot(fpy3,ppy3,type="l",xlim=c(0.0005,0.003),ylim=c(0,5e8),xlab="Frecuencias",ylab="Amplitud",main=paste("Espectro de frecuencias Y3"),xaxs="i",yaxs="i",col="purple")





##################MOLECULA D20

###Centro de Masas
#cmm=read.table("/home/pj/Escritorio/simulacion/simulacion3/centromasas.dat")
t=cmm[1:nrow(cmm),1]
cm=cmm[1:nrow(cmm),2]
xcm=cmm[1:nrow(cmm),3]
ycm=cmm[1:nrow(cmm),4]
plot(t,cm,type="l",lwd=2,ylim=c(-0.8e-13,1.6e-13),xaxs="i",yaxs="i",col="red",xlab="Tiempo",ylab="Separación",main=paste("Evolución temporal del Centro de Masas"))
lines(t,xcm,lwd=2,type="l",col="blue")
lines(t,ycm,lwd=2,type="l",col="purple")
legend(250,1.45e-13,lty=c(1),c("Centro Masas","X Centro Masas","Y Centro Masas"),lwd=2,col=c("red","blue","purple"))
#

###Energy
#enn=read.table("/home/pj/Escritorio/simulacion/simulacion3/energias.dat")
t=enn[1:nrow(enn),1]
etot=enn[1:nrow(enn),2]
ecin=enn[1:nrow(enn),3]
epot=enn[1:nrow(enn),4]
plot(t,etot,type="l",xlab="Tiempo",ylab="Energía Total",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal de la Energía Total del sistema"))

###Posiciones X
#xx=read.table("/home/pj/Escritorio/simulacion/simulacion3/posicionesx.dat")
t=xx[1:nrow(xx),1]
x1=xx[1:nrow(xx),2]
x2=xx[1:nrow(xx),3]
x3=xx[1:nrow(xx),4]
plot(t,x1,type="l",xlab="Tiempo",ylab="X1",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal X Deuterio 1"))
plot(t,x2,type="l",xlab="Tiempo",ylab="X2",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal X Oxígeno"))
plot(t,x3,type="l",xlab="Tiempo",ylab="X3",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal X Deuterio 2"))


###Posiciones Y
#yy=read.table("/home/pj/Escritorio/simulacion/simulacion3/posicionesx.dat")
t=yy[1:nrow(yy),1]
y1=yy[1:nrow(yy),2]
y2=yy[1:nrow(yy),3]
y3=yy[1:nrow(yy),4]
plot(t,y1,type="l",xlab="Tiempo",ylab="Y1",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal Y Deuterio 1"))
plot(t,y2,type="l",xlab="Tiempo",ylab="Y2",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal Y Oxígeno"))
plot(t,y3,type="l",xlab="Tiempo",ylab="Y3",xaxs="i",yaxs="i",col="blue",main=paste("Evolución temporal Y Deuterio 2"))


#Periodicidades

#px3=read.table("")
fpx3=px3[1:nrow(px3),1]
ppx3=px3[1:nrow(px3),2]
plot(fpx3,ppx3,type="l",xlim=c(0.0005,0.003),ylim=c(0,7e8),xlab="Frecuencias",ylab="Amplitud",main=paste("Espectro de frecuencias X3"),xaxs="i",yaxs="i",col="blue")
locator(3)

#py1=read.table("")
fpy1=py1[1:nrow(py1),1]
ppy1=py1[1:nrow(py1),2]
plot(fpy1,ppy1,type="l",xlim=c(0.0005,0.003),ylim=c(0,4e8),xlab="Frecuencias",ylab="Amplitud",main=paste("Espectro de frecuencias Y1"),xaxs="i",yaxs="i",col="red")
locator(3)
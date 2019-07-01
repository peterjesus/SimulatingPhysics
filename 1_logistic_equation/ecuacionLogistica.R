
################## EC LOGISTICA

##########r=0.15
###x0=0.20        #Converge en la iteracion 72 con tolerancia 0.4 y criterio de error relativo
logistable1520=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 15-x020-1-pjla.dat")
i1520=logistable1520[1:nrow(logistable1520),1] #j
x1520=logistable1520[1:nrow(logistable1520),2] #x
###x0=0.55        #Converge en la iteracion 72 con tolerancia 0.4 y criterio de error relativo
logistable1555=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 15-x055-1-pjla.dat")
i1555=logistable1555[1:nrow(logistable1555),1] #j
x1555=logistable1555[1:nrow(logistable1555),2] #x
###x0=0.90        #Converge en la iteracion 69 con tolerancia 0.4 y criterio de error relativo
logistable1590=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 15-x090-1-pjla.dat")
i1590=logistable1590[1:nrow(logistable1590),1] #j
x1590=logistable1590[1:nrow(logistable1590),2] #x

plot(i1520,x1520,type="p",xaxs="i",yaxs="i",ylim=c(0,1),xlim=c(0,10),xlab="j",ylab="X(j)",main=paste("Órbita Ecuación Logística para r=0.15"),col="blue",pch=19,cex=0.5)
lines(i1555,x1555,type="p",col="red",pch=0,cex=0.5)
lines(i1590,x1590,type="p",col="green",pch=2,cex=0.5)
grid()
legend(7,0.95,c("x0=0.20 ","x0=0.55 ","x0=0.90 "),pch=c(c(19),c(0),c(2)),col=c("blue","red","green"))

##########r=0.50
###x0=0.20        #Converge en la iteracion 3 con tolerancia 0.0001 y criterio de error relativo
logistable5020=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 50-x020-1-pjla.dat")
i5020=logistable5020[1:nrow(logistable5020),1] #j
x5020=logistable5020[1:nrow(logistable5020),2] #x
###x0=0.55        #Converge en la iteracion 3 con tolerancia 0.0001 y criterio de error relativo
logistable5055=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 50-x055-1-pjla.dat")
i5055=logistable5055[1:nrow(logistable5055),1] #j
x5055=logistable5055[1:nrow(logistable5055),2] #x
###x0=0.90        #Converge en la iteracion 3 con tolerancia 0.0001 y criterio de error relativo
logistable5090=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 50-x090-1-pjla.dat")
i5090=logistable5090[1:nrow(logistable5090),1] #j
x5090=logistable5090[1:nrow(logistable5090),2] #x

plot(i5020,x5020,type="p",xaxs="i",yaxs="i",ylim=c(0,1),xlim=c(0,10),xlab="j",ylab="X(j)",main=paste("Órbita Ecuación Logística para r=0.50"),col="blue",pch=19,cex=0.5)
lines(i5055,x5055,type="p",col="red",pch=0,cex=0.5)
lines(i5090,x5090,type="p",col="green",pch=2,cex=0.5)
grid()
legend(7,0.95,c("x0=0.20","x0=0.55","x0=0.90"),pch=c(c(19),c(0),c(2)),col=c("blue","red","green"))

##########r=0.85
###x0=0.20        #Converge en la iteracion 3 con tolerancia 0.5 y criterio de error relativo
logistable8520=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 85-x020-1-pjla.dat")
i8520=logistable8520[1:nrow(logistable8520),1] #j
x8520=logistable8520[1:nrow(logistable8520),2] #x
###x0=0.55        #Converge en la iteracion 2 con tolerancia 0.5 y criterio de error relativo
logistable8555=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 85-x055-1-pjla.dat")
i8555=logistable8555[1:nrow(logistable8555),1] #j
x8555=logistable8555[1:nrow(logistable8555),2] #x
###x0=0.90        #Converge en la iteracion 3 con tolerancia 0.08 y criterio de error relativo
logistable8590=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 85-x090-1-pjla.dat")
i8590=logistable8590[1:nrow(logistable8590),1] #j
x8590=logistable8590[1:nrow(logistable8590),2] #x

plot(i8520,x8520,type="p",xaxs="i",yaxs="i",ylim=c(0,1),xlim=c(0,50),xlab="j",ylab="X(j)",main=paste("Órbita Ecuación Logística para r=0.85"),col="blue",pch=19,cex=0.5)
lines(i8555,x8555,type="p",col="red",pch=0,cex=0.5)
lines(i8590,x8590,type="p",col="green",pch=2,cex=0.5)
grid()
legend(30,0.35,c("x0=0.20 ","x0=0.55 ","x0=0.90 "),pch=c(c(19),c(0),c(2)),col=c("blue","red","green"))

##########r=0.95
###x0=0.20        #Converge en la iteracion 636 con tolerancia 0.01 y criterio de error relativo
logistable9520=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 95-x020-1-pjla.dat")
i9520=logistable9520[1:nrow(logistable9520),1] #j
x9520=logistable9520[1:nrow(logistable9520),2] #x
###x0=0.55        #Converge en la iteracion 70 con tolerancia 0.01 y criterio de error relativo
logistable9555=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 95-x055-1-pjla.dat")
i9555=logistable9555[1:nrow(logistable9555),1] #j
x9555=logistable9555[1:nrow(logistable9555),2] #x
###x0=0.90        #Converge en la iteracion 562 con tolerancia 0.01 y criterio de error relativo
logistable9590=read.table("/home/pj/Escritorio/simulacion1/iterativo/logistica-r 95-x090-1-pjla.dat")
i9590=logistable9590[1:nrow(logistable9590),1] #j
x9590=logistable9590[1:nrow(logistable9590),2] #x
plot(i9520,x9520,type="p",xaxs="i",yaxs="i",ylim=c(0,1),xlim=c(0,200),xlab="j",ylab="X(j)",main=paste("Órbita Ecuación Logística para r=0.95"),col="blue",pch=19,cex=0.5)
lines(i9555,x9555,type="p",col="red",pch=0,cex=0.5)
lines(i9590,x9590,type="p",col="green",pch=2,cex=0.5)
grid()
legend(150,0.35,c("x0=0.20 ","x0=0.55 ","x0=0.90 "),pch=c(c(19),c(0),c(2)),col=c("blue","red","green"))


##################FEIGENBAUM EC LOGISTICA
#ORIGINAL
feigentable=read.table("/home/pj/Escritorio/simulacion1/feigenbaum/feigenbaum-1-1000.dat")
r=feigentable[1:nrow(feigentable),1]
cicl=feigentable[1:nrow(feigentable),2]
sol=feigentable[1:nrow(feigentable),3]
plot(r,cicl,type="p",xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="blue",pch=19,cex=0.0000000000001)
grid()
legend(0.025,0.9,"Puntos cíclicos",pch=c(19),col="blue")
plot(r,cicl,type="p",xlim=c(0.85,1),xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="blue",pch=19,cex=0.0000000000001)
grid()
legend(0.855,0.2,"Puntos cíclicos",pch=c(19),col="blue")
plot(r,sol,ylim=c(0,50),type="h",xaxs="i",yaxs="i",xlab="r",ylab="Número de Soluciones",main=paste("Búsqueda aparación del Caos"),col="green")
abline(h=-1,v=0.8926,col="red",lwd=2.5)
grid()
legend(0.05,47.5,lty=c(c(1),c(1)),c("Soluciones encontradas","Valor inicio del caos"),col=c("green","red"))


#2
feigentable2=read.table("/home/pj/Escritorio/simulacion1/feigenbaum/feigenbaum-2-1000.dat")
r2=feigentable2[1:nrow(feigentable2),1]
cicl2=feigentable2[1:nrow(feigentable2),2]
sol2=feigentable2[1:nrow(feigentable2),3]
plot(r2,cicl2,type="p",xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="red",pch=19,cex=0.0000000000001)
grid()
legend(0.025,4.5,"Puntos cíclicos",pch=c(19),col="red")
plot(r2,cicl2,type="p",xlim=c(0.6,1),xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="red",pch=19,cex=0.0000000000001)
grid()
legend(0.6,4.75,"Puntos cíclicos",pch=c(19),col="red")


#3
feigentable3=read.table("/home/pj/Escritorio/simulacion1/feigenbaum/feigenbaum-3-1000.dat")
r3=feigentable3[1:nrow(feigentable3),1]
cicl3=feigentable3[1:nrow(feigentable3),2]
sol3=feigentable3[1:nrow(feigentable3),3]
plot(r3,cicl3,type="p",xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="purple",pch=19,cex=0.0000000000001)
grid()
legend(0.025,0.9,"Puntos cíclicos",pch=c(19),col="purple")
plot(r3,cicl3,type="p",xlim=c(0.9,1),xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="purple",pch=19,cex=0.0000000000001)
grid()
legend(0.905,0.3,"Puntos cíclicos",pch=c(19),col="purple")


#4
feigentable4=read.table("/home/pj/Escritorio/simulacion1/feigenbaum/feigenbaum-4-1000.dat")
r4=feigentable4[1:nrow(feigentable4),1]
cicl4=feigentable4[1:nrow(feigentable4),2]
sol4=feigentable4[1:nrow(feigentable4),3]
plot(r4,cicl4,type="p",xaxs="i",yaxs="i",xlab="r",ylab="Puntos cíclicos",main=paste("Diagrama de Feigenbaum"),col="orange",pch=19,cex=0.0000000000001)
grid()
legend(0.025,5.5,"Puntos cíclicos",pch=c(19),col="orange")

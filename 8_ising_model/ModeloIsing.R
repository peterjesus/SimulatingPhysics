######ISING

#Ejemplillos
neg30100=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-topspin-30-bajatemp.dat")
ineg30100=neg30100[1:nrow(neg30100),1]
jneg30100=neg30100[1:nrow(neg30100),2]
plot(ineg30100,jneg30100,xlab="i",ylab="j",xlim=c(1,30),ylim=c(1,30),yaxs="i",xaxs="i",col="blue",pch=18,main=paste("Magnetización para T=10e-2"))
grid()

neg30001=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-downspin-30-altatemp.dat")
ineg30001=neg30001[1:nrow(neg30001),1]
jneg30001=neg30001[1:nrow(neg30001),2]
pos30001=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-topspin-30-altatemp.dat")
ipos30001=pos30001[1:nrow(pos30001),1]
jpos30001=pos30001[1:nrow(pos30001),2]
plot(ineg30001,jneg30001,xlab="i",ylab="j",xlim=c(1,30),ylim=c(1,30),yaxs="i",xaxs="i",col="blue",pch=18,main=paste("Magnetización para T=10e2"))
points(ipos30001,jpos30001,col="red",pch=18)
grid()


#x=seq(0,10,length.out = 100000)
#m=(1-(1/(sinh(2/x)))^4)^(1/8)
#plot(x,m,type="l")




######Magnetizacion


magnet10=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-magnet-10.dat")
t10=magnet10[1:nrow(magnet10),1]
m10=magnet10[1:nrow(magnet10),2]
e10=magnet10[1:nrow(magnet10),3]
plot(t10,m10,type="l",col="red")
epsilon=0.05
for (i in 1:nrow(magnet10)){
  up=m10[i]+e10[i]
  low=m10[i]-e10[i]
  segments(t10[i],low,t10[i],up,col="red")
  segments(t10[i]-epsilon,low,t10[i]+epsilon,low,col="red")
  segments(t10[i]-epsilon,up,t10[i]+epsilon,up,col="red")
}

magnet20=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-magnet-20.dat")
t20=magnet20[1:nrow(magnet20),1]
m20=magnet20[1:nrow(magnet20),2]
e20=magnet20[1:nrow(magnet20),3]
plot(t20,m20,type="l",col="red",xlab="Temperatura",ylab="Magnetización",xaxs="i",yaxs="i",main=paste("Temperatura vs Magnetización (L=20)"))
epsilon=0.05
for (i in 1:nrow(magnet20)){
  up=m20[i]+e20[i]
  low=m20[i]-e20[i]
  segments(t20[i],low,t20[i],up,col="red")
  segments(t20[i]-epsilon,low,t20[i]+epsilon,low,col="red")
  segments(t20[i]-epsilon,up,t20[i]+epsilon,up,col="red")
}

magnet30=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-magnet-30.dat")
t30=magnet30[1:nrow(magnet30),1]
m30=magnet30[1:nrow(magnet30),2]
e30=magnet30[1:nrow(magnet30),3]
plot(t30,m30,type="l",col="blue",xlab="Temperatura",ylab="Magnetización",xaxs="i",yaxs="i",main=paste("Temperatura vs Magnetización (L=30)"))
epsilon=0.05
for (i in 1:nrow(magnet30)){
  up=m30[i]+e30[i]
  low=m30[i]-e30[i]
  segments(t30[i],low,t30[i],up,col="blue")
  segments(t30[i]-epsilon,low,t30[i]+epsilon,low,col="blue")
  segments(t30[i]-epsilon,up,t30[i]+epsilon,up,col="blue")
}

magnet40=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-magnet-40.dat")
t40=magnet40[1:nrow(magnet40),1]
m40=magnet40[1:nrow(magnet40),2]
e40=magnet40[1:nrow(magnet40),3]
plot(t40,m40,type="l",col="limegreen",xlab="Temperatura",ylab="Magnetización",xaxs="i",yaxs="i",main=paste("Temperatura vs Magnetización (L=40)"))

epsilon=0.05
for (i in 1:nrow(magnet40)){
  up=m40[i]+e40[i]
  low=m40[i]-e40[i]
  segments(t40[i],low,t10[i],up,col="limegreen")
  segments(t40[i]-epsilon,low,t40[i]+epsilon,low,col="limegreen")
  segments(t40[i]-epsilon,up,t40[i]+epsilon,up,col="limegreen")
}

magnet50=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-magnet-50.dat")
t50=magnet50[1:nrow(magnet50),1]
m50=magnet50[1:nrow(magnet50),2]
e50=magnet50[1:nrow(magnet50),3]
plot(t50,m50,type="l",col="purple",xlab="Temperatura",ylab="Magnetización",xaxs="i",yaxs="i",main=paste("Temperatura vs Magnetización (L=50)"))
epsilon=0.05
for (i in 1:nrow(magnet50)){
  up=m50[i]+e50[i]
  low=m50[i]-e50[i]
  segments(t50[i],low,t50[i],up,col="purple")
  segments(t50[i]-epsilon,low,t50[i]+epsilon,low,col="purple")
  segments(t50[i]-epsilon,up,t50[i]+epsilon,up,col="purple")
}

#####Energia

energ10=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-energy-10.dat")
t10=energ10[1:nrow(energ10),1]
n10=energ10[1:nrow(energ10),2]
e10=energ10[1:nrow(energ10),3]
plot(t10,n10,type="l",col="red")
epsilon=0.05
for (i in 1:nrow(energ10)){
  up=n10[i]+e10[i]
  low=n10[i]-e10[i]
  segments(t10[i],low,t10[i],up,col="red")
  segments(t10[i]-epsilon,low,t10[i]+epsilon,low,col="red")
  segments(t10[i]-epsilon,up,t10[i]+epsilon,up,col="red")
}

energ20=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-energy-20.dat")
t20=energ20[1:nrow(energ20),1]
n20=energ20[1:nrow(energ20),2]
e20=energ20[1:nrow(energ20),3]
plot(t20,n20,type="l",col="red",xlab="Temperatura",ylab="Energía/Espín",xaxs="i",yaxs="i",main=paste("Temperatura vs Energía Media (L=20)"))
epsilon=0.05
for (i in 1:nrow(energ20)){
  up=n20[i]+e20[i]
  low=n20[i]-e20[i]
  segments(t20[i],low,t20[i],up,col="red")
  segments(t20[i]-epsilon,low,t20[i]+epsilon,low,col="red")
  segments(t20[i]-epsilon,up,t20[i]+epsilon,up,col="red")
}

energ30=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-energy-30.dat")
t30=energ30[1:nrow(energ30),1]
n30=energ30[1:nrow(energ30),2]
e30=energ30[1:nrow(energ30),3]
plot(t30,n30,type="l",col="blue",xlab="Temperatura",ylab="Energía/Espín",xaxs="i",yaxs="i",main=paste("Temperatura vs Energía Media  (L=30)"))
epsilon=0.05
for (i in 1:nrow(energ30)){
  up=n30[i]+e30[i]
  low=n30[i]-e30[i]
  segments(t30[i],low,t30[i],up,col="blue")
  segments(t30[i]-epsilon,low,t30[i]+epsilon,low,col="blue")
  segments(t30[i]-epsilon,up,t30[i]+epsilon,up,col="blue")
}

energ40=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-energy-40.dat")
t40=energ40[1:nrow(energ40),1]
n40=energ40[1:nrow(energ40),2]
e40=energ40[1:nrow(energ40),3]
plot(t40,n40,type="l",col="limegreen",xlab="Temperatura",ylab="Energía/Espín",xaxs="i",yaxs="i",main=paste("Temperatura vs Energía Media  (L=40)"))
epsilon=0.05
for (i in 1:nrow(energ40)){
  up=n40[i]+e40[i]
  low=n40[i]-e40[i]
  segments(t40[i],low,t10[i],up,col="limegreen")
  segments(t40[i]-epsilon,low,t40[i]+epsilon,low,col="limegreen")
  segments(t40[i]-epsilon,up,t40[i]+epsilon,up,col="limegreen")
}

energ50=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-energy-50.dat")
t50=energ50[1:nrow(energ50),1]
n50=energ50[1:nrow(energ50),2]
e50=energ50[1:nrow(energ50),3]
plot(t50,n50,type="l",col="purple",xlab="Temperatura",ylab="Energía/Espín",xaxs="i",yaxs="i",main=paste("Temperatura vs Energía Media (L=50)"))
epsilon=0.05
for (i in 1:nrow(energ50)){
  up=n50[i]+e50[i]
  low=n50[i]-e50[i]
  segments(t50[i],low,t50[i],up,col="purple")
  segments(t50[i]-epsilon,low,t50[i]+epsilon,low,col="purple")
  segments(t50[i]-epsilon,up,t50[i]+epsilon,up,col="purple")
}

#Magnetizacion para todas
plot(t10,m10,type="l",xaxs="i",yaxs="i",col="cyan",ylim=c(0,1),main=paste("Magnetización"),ylab="Magnetización",xlab="Temperatura")
epsilon=0.005
for (i in 1:nrow(magnet10)){
  up=m10[i]+e10[i]
  low=m10[i]-e10[i]
  segments(t10[i],low,t10[i],up,col="cyan")
  segments(t10[i]-epsilon,low,t10[i]+epsilon,low,col="cyan")
  segments(t10[i]-epsilon,up,t10[i]+epsilon,up,col="cyan")
}
lines(t20,m20,type="l",col="red",xlab="Temperatura",ylab="Magnetización",xaxs="i",yaxs="i",main=paste("Temperatura vs Magnetización (L=20)"))
epsilon=0.005
for (i in 1:nrow(magnet20)){
  up=m20[i]+e20[i]
  low=m20[i]-e20[i]
  segments(t20[i],low,t20[i],up,col="red")
  segments(t20[i]-epsilon,low,t20[i]+epsilon,low,col="red")
  segments(t20[i]-epsilon,up,t20[i]+epsilon,up,col="red")
}
lines(t30,m30,type="l",col="blue")
epsilon=0.005
for (i in 1:nrow(magnet30)){
  up=m30[i]+e30[i]
  low=m30[i]-e30[i]
  segments(t30[i],low,t30[i],up,col="blue")
  segments(t30[i]-epsilon,low,t30[i]+epsilon,low,col="blue")
  segments(t30[i]-epsilon,up,t30[i]+epsilon,up,col="blue")
}
lines(t40,m40,type="l",col="limegreen")
epsilon=0.005
for (i in 1:nrow(magnet40)){
  up=m40[i]+e40[i]
  low=m40[i]-e40[i]
  segments(t40[i],low,t10[i],up,col="limegreen")
  segments(t40[i]-epsilon,low,t40[i]+epsilon,low,col="limegreen")
  segments(t40[i]-epsilon,up,t40[i]+epsilon,up,col="limegreen")
}
lines(t50,m50,type="l",col="purple")
epsilon=0.005
for (i in 1:nrow(magnet50)){
  up=m50[i]+e50[i]
  low=m50[i]-e50[i]
  segments(t50[i],low,t50[i],up,col="purple")
  segments(t50[i]-epsilon,low,t50[i]+epsilon,low,col="purple")
  segments(t50[i]-epsilon,up,t50[i]+epsilon,up,col="purple")
}
legend(4.04,0.93,cex=1,lty=c(1),c("L=10","L=20","L=30","L=40","L=50"),col=c("cyan","red","blue","limegreen","purple"))



#####Zona lineal
magnet10=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-linmagnet-10.dat")
t10=magnet10[1:nrow(magnet10),1]
m10=magnet10[1:nrow(magnet10),2]
e10=magnet10[1:nrow(magnet10),3]

magnet20=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-linmagnet-20.dat")
t20=magnet20[1:nrow(magnet20),1]
m20=magnet20[1:nrow(magnet20),2]
e20=magnet20[1:nrow(magnet20),3]

magnet30=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-linmagnet-30.dat")
t30=magnet30[1:nrow(magnet30),1]
m30=magnet30[1:nrow(magnet30),2]
e30=magnet30[1:nrow(magnet30),3]

magnet40=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-linmagnet-40.dat")
t40=magnet40[1:nrow(magnet40),1]
m40=magnet40[1:nrow(magnet40),2]
e40=magnet40[1:nrow(magnet40),3]

magnet50=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-linmagnet-50.dat")
t50=magnet50[1:nrow(magnet50),1]
m50=magnet50[1:nrow(magnet50),2]
e50=magnet50[1:nrow(magnet50),3]




plot(t10,m10,type="l",xaxs="i",yaxs="i",col="cyan",ylim=c(0,1),main=paste("Zona Lineal Magnetización"),ylab="Magnetización",xlab="Temperatura")
epsilon=0.005
for (i in 1:nrow(magnet10)){
  up=m10[i]+e10[i]
  low=m10[i]-e10[i]
  segments(t10[i],low,t10[i],up,col="cyan")
  segments(t10[i]-epsilon,low,t10[i]+epsilon,low,col="cyan")
  segments(t10[i]-epsilon,up,t10[i]+epsilon,up,col="cyan")
}

lines(t20,m20,type="l",xaxs="i",yaxs="i",col="red",ylim=c(0,1),main=paste("Zona Lineal Magnetización"),ylab="Magnetización",xlab="Temperatura")
epsilon=0.005
for (i in 1:nrow(magnet20)){
  up=m20[i]+e20[i]
  low=m20[i]-e20[i]
  segments(t20[i],low,t20[i],up,col="red")
  segments(t20[i]-epsilon,low,t20[i]+epsilon,low,col="red")
  segments(t20[i]-epsilon,up,t20[i]+epsilon,up,col="red")
}
lines(t30,m30,type="l",col="blue")
epsilon=0.005
for (i in 1:nrow(magnet30)){
  up=m30[i]+e30[i]
  low=m30[i]-e30[i]
  segments(t30[i],low,t30[i],up,col="blue")
  segments(t30[i]-epsilon,low,t30[i]+epsilon,low,col="blue")
  segments(t30[i]-epsilon,up,t30[i]+epsilon,up,col="blue")
}
lines(t40,m40,type="l",col="limegreen")
epsilon=0.005
for (i in 1:nrow(magnet40)){
  up=m40[i]+e40[i]
  low=m40[i]-e40[i]
  segments(t40[i],low,t10[i],up,col="limegreen")
  segments(t40[i]-epsilon,low,t40[i]+epsilon,low,col="limegreen")
  segments(t40[i]-epsilon,up,t40[i]+epsilon,up,col="limegreen")
}
lines(t50,m50,type="l",col="purple")
epsilon=0.005
for (i in 1:nrow(magnet50)){
  up=m50[i]+e50[i]
  low=m50[i]-e50[i]
  segments(t50[i],low,t50[i],up,col="purple")
  segments(t50[i]-epsilon,low,t50[i]+epsilon,low,col="purple")
  segments(t50[i]-epsilon,up,t50[i]+epsilon,up,col="purple")
}
legend(2.055,0.625,cex=1.1,lty=c(1),c("L=10","L=20","L=30","L=40","L=50"),col=c("cyan","red","blue","limegreen","purple"))



##Calculo Tcritica


dising=read.table("/home/pj/Escritorio/simulacion/simulacion7/ajlinising.dat")
size=dising[1:nrow(dising),1]
invsize=dising[1:nrow(dising),2]
tc=dising[1:nrow(dising),3]
etc=dising[1:nrow(dising),4]

plot(invsize,tc,ylim=c(2,3.7),xlim=c(0,0.012),xlab="1/N^2",ylab="Temp. Crítica",main=paste("Tamaño muestra vs Temperatura crítica"),pch=20,col="red")
epsilon=0.0002
for (i in 1:length(etc)){
  up=tc[i]+etc[i]
  low=tc[i]-etc[i]
  segments(invsize[i],low,invsize[i],up,col="black")
  segments(invsize[i]-epsilon,low,invsize[i]+epsilon,low,col="black")
  segments(invsize[i]-epsilon,up,invsize[i]+epsilon,up,col="black")
}
abline(v=0,col="blue")
#gnuplot
q=seq(0,0.015,length.out = 10000)
lines(q,q*67.6232+2.58366,col="purple")
legend(0.005,2.625,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("Temperatura Crítica    ","Ajuste Tc vs 1/N**2     "))



###Calculo exponente

qq=seq(-4,-0.5,length.out = 100000)
ln50=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-lnmagnet-50.dat")
lnt=ln50[1:nrow(ln50),1]
lnm=ln50[1:nrow(ln50),2]
elnm=ln50[1:nrow(ln50),3]
#gnuplot
plot(lnt,lnm,pch=20,col="red",xlab="ln(1-T/Tc)",ylab="ln(M)",main=paste("Temperatura vs Magnetizacion"))
lines(qq,qq*0.0929824+0.0868675,col="purple")
legend(-2.9,-0.2,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("Ln Magnetizacion    ","Ajuste ln(M) vs ln(1-T/Tc)      "))


###Representacion calor especifico

calor10=read.table("/home/pj/Dropbox/simulacion/ising-calor-10.dat")
t10=calor10[1:nrow(calor10),1]
c10=calor10[1:nrow(calor10),2]

calor20=read.table("/home/pj/Dropbox/simulacion/ising-calor-20.dat")
t20=calor20[1:nrow(calor20),1]
c20=calor20[1:nrow(calor20),2]

calor30=read.table("/home/pj/Dropbox/simulacion/ising-calor-30.dat")
t30=calor30[1:nrow(calor30),1]
c30=calor30[1:nrow(calor30),2]

calor40=read.table("/home/pj/Dropbox/simulacion/ising-calor-40.dat")
t40=calor40[1:nrow(calor40),1]
c40=calor40[1:nrow(calor40),2]

calor50=read.table("/home/pj/Dropbox/simulacion/ising-calor-50.dat")
t50=calor50[1:nrow(calor50),1]
c50=calor50[1:nrow(calor50),2]

plot(t10,c10,type="l",xaxs="i",yaxs="i",col="cyan",ylim=c(0,0.6),main=paste("Calor específico vs la temperatura (L=10)"),ylab="Calor específico",xlab="Temperatura")
plot(t20,c20,type="l",xaxs="i",yaxs="i",col="red",ylim=c(0,0.35),main=paste("Calor específico vs la temperatura (L=20)"),ylab="Calor específico",xlab="Temperatura")
plot(t30,c30,type="l",xaxs="i",yaxs="i",col="blue",ylim=c(0,0.3),main=paste("Calor específico vs la temperatura (L=30)"),ylab="Calor específico",xlab="Temperatura")
plot(t40,c40,type="l",xaxs="i",yaxs="i",col="limegreen",ylim=c(0,0.2),main=paste("Calor específico vs la temperatura (L=40)"),ylab="Calor específico",xlab="Temperatura")
plot(t50,c50,type="l",xaxs="i",yaxs="i",col="purple",ylim=c(0,0.2),main=paste("Calor específico vs la temperatura (L=50)"),ylab="Calor específico",xlab="Temperatura")


tmred=c(10,20,30,40,50)
ired=1/tmred**2
tccc=c(2.3399999999999999,2.3399999999999999,2.2799999999999998,2.2799999999999998,2.2799999999999998)
plot(ired,tccc,xlim=c(0,0.01),ylim=c(2.25,2.4),pch=20,type="p",col="red",xlab="1/N^2",ylab="Temp. Crítica",main=paste("Tamaño muestra vs Temperatura crítica"))
abline(v=0,col="blue")
qqq=seq(0,0.015,length.out = 10000)
lines(qqq,qqq*6.11628+2.2861,col="purple")
legend(0.004,2.4,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("Temperatura Crítica    ","Ajuste Tc vs 1/N**2     "))
epsilon=0.0001
for (i in 1:length(tccc)){
  up=tccc[i]+0.03
  low=tccc[i]-0.03
  segments(ired[i],low,ired[i],up,col="black")
  segments(ired[i]-epsilon,low,ired[i]+epsilon,low,col="black")
  segments(ired[i]-epsilon,up,ired[i]+epsilon,up,col="black")
}


#xls=seq(0,2.7,length.out = 10000)
#tcc50=read.table("/home/pj/Escritorio/simulacion/simulacion7/ising-tcmagnet-50.dat")
#ttc=tcc50[1:nrow(tcc50),1]
#mtc=tcc50[1:nrow(tcc50),2]
#plot(ttc,mtc)
#lines(xls,((2.269-xls)/2.269)**(0.03))

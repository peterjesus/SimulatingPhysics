##################FRACTALES

###Movimiento Fractal
frac1110=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal1-prob1-10.dat")
ifrac1110=frac1110[1:nrow(frac1110),1]
jfrac1110=frac1110[1:nrow(frac1110),2]
plot(ifrac1110,jfrac1110,xlab="i",ylab="j",xlim=c(0,10),ylim=c(0,10),col="blue",pch=18,main=paste("Percolación para p=1.0"))
legend(0,10,c(" 1  "," 2  "),pch=c(c(18),c(18)),col=c("red","blue"))
grid()

frac1010=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal1-prob0-10.dat")
ifrac1010=frac1010[1:nrow(frac1010),1]
jfrac1010=frac1010[1:nrow(frac1010),2]
frac2010=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal2-prob0-10.dat")
ifrac2010=frac2010[1:nrow(frac2010),1]
jfrac2010=frac2010[1:nrow(frac2010),2]
plot(ifrac2010,jfrac2010,xlab="i",ylab="j",xlim=c(0,10),ylim=c(0,10),col="blue",pch=18,main=paste("Percolación para p=0.0"))
lines(ifrac1010,jfrac1010,type="p",col="red",pch=18)
grid()
legend(0,10,c(" 1  "," 2  "),pch=c(c(18),c(18)),col=c("red","blue"))


frac105530=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal1-prob055-30.dat")
ifrac105530=frac105530[1:nrow(frac105530),1]
jfrac105530=frac105530[1:nrow(frac105530),2]
frac205530=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal2-prob055-30.dat")
ifrac205530=frac205530[1:nrow(frac205530),1]
jfrac205530=frac205530[1:nrow(frac205530),2]
plot(ifrac205530,jfrac205530,main=paste("Percolación para p=0.55"),xlab="i",ylab="j",xaxs="i",yaxs="i",type="p",xlim=c(1,30),ylim=c(1,30),col="blue",pch=18)
lines(ifrac105530,jfrac105530,type="p",col="red",pch=18)
grid()
legend(9,30,c(" 1 "," 2 "),pch=c(c(18),c(18)),col=c("red","blue"))


frac107030=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal1-prob07-30.dat")
ifrac107030=frac107030[1:nrow(frac107030),1]
jfrac107030=frac107030[1:nrow(frac107030),2]
frac207030=read.table("/home/pj/Escritorio/simulacion/simulacion6/percfractal/percfractal2-prob07-30.dat")
ifrac207030=frac207030[1:nrow(frac207030),1]
jfrac207030=frac207030[1:nrow(frac207030),2]
plot(ifrac207030,jfrac207030,main=paste("Percolación para p=0.70"),xlab="i",ylab="j",xaxs="i",yaxs="i",type="p",xlim=c(1,30),ylim=c(1,30),col="blue",pch=18)
lines(ifrac107030,jfrac107030,type="p",col="red",pch=18)
grid()
legend(2,29,c(" 1 "," 2 "),pch=c(c(18),c(18)),col=c("red","blue"))


###Longitudes medias total
dates30=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-300-1.dat")
probs30=dates30[1:nrow(dates30),1]
longs30=dates30[1:nrow(dates30),2]
dates20=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-200-1.dat")
probs20=dates20[1:nrow(dates20),1]
longs20=dates20[1:nrow(dates20),2]
dates10=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-100-1.dat")
probs10=dates10[1:nrow(dates10),1]
longs10=dates10[1:nrow(dates10),2]
dates5=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong- 50-1.dat")
probs5=dates5[1:nrow(dates5),1]
longs5=dates5[1:nrow(dates5),2]
plot(probs30,longs30,type="l",col="blue",pch=4,xlab="Probabilidad",ylab="Longitudes medias",main=paste("Longitud media trayectorias vs Probabilidad"))
lines(probs5,longs5,type="l",col="purple")
lines(probs20,longs20,type="l",col="orange")
lines(probs10,longs10,type="l",col="red")
grid()
legend(0.2,18000,c("L= 50","L= 100","L= 200","L= 300"),lty=c(1),col=c("purple","red","orange","blue"))

datess30=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-300-2.dat")
probss30=datess30[1:nrow(datess30),1]
longss30=datess30[1:nrow(datess30),2]
datess20=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-200-2.dat")
probss20=datess20[1:nrow(datess20),1]
longss20=datess20[1:nrow(datess20),2]
datess10=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong-100-2.dat")
probss10=datess10[1:nrow(datess10),1]
longss10=datess10[1:nrow(datess10),2]
datess5=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-problong- 50-2.dat")
probss5=datess5[1:nrow(datess5),1]
longss5=datess5[1:nrow(datess5),2]
plot(probss30,longss30,type="p",col="blue",ylim=c(0,20000),pch=19,xlab="Probabilidad",ylab="Longitudes medias",main=paste("Longitud media trayectorias vs Probabilidad"))
lines(probss5,longss5,type="p",pch=20,col="purple")
lines(probss20,longss20,type="p",pch=17,col="orange")
lines(probss10,longss10,type="p",pch=18,col="red")
grid()





###Probabilidad critica

dat50=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica- 50-1.dat")
prob50=dat50[1:nrow(dat50),1]
left50=dat50[1:nrow(dat50),2]
rght50=dat50[1:nrow(dat50),3]
extd50=dat50[1:nrow(dat50),4]
dat100=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica-100-1.dat")
prob100=dat100[1:nrow(dat100),1]
left100=dat100[1:nrow(dat100),2]
rght100=dat100[1:nrow(dat100),3]
extd100=dat100[1:nrow(dat100),4]
dat150=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica-150-1.dat")
prob150=dat150[1:nrow(dat150),1]
left150=dat150[1:nrow(dat150),2]
rght150=dat150[1:nrow(dat150),3]
extd150=dat150[1:nrow(dat150),4]
dat200=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica-200-1.dat")
prob200=dat200[1:nrow(dat200),1]
left200=dat200[1:nrow(dat200),2]
rght200=dat200[1:nrow(dat200),3]
extd200=dat200[1:nrow(dat200),4]
dat250=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica-250-1.dat")
prob250=dat250[1:nrow(dat250),1]
left250=dat250[1:nrow(dat250),2]
rght250=dat250[1:nrow(dat250),3]
extd250=dat250[1:nrow(dat250),4]
dat300=read.table("/home/pj/Escritorio/simulacion/simulacion6/fractal-probcritica-300-1.dat")
prob300=dat300[1:nrow(dat300),1]
left300=dat300[1:nrow(dat300),2]
rght300=dat300[1:nrow(dat300),3]
extd300=dat300[1:nrow(dat300),4]


#Grafica T.Extendidas
plot(prob50,extd50,type="l",col="blue",xlab="Probabilidad",ylab="Num. Trayectorias Extendidas",main=paste("Número de trayectorias extendidas vs Probabilidad"))
lines(prob100,extd100,col="red")
#lines(prob150,extd150)
lines(prob200,extd200,col="orange")
#lines(prob250,extd250)
lines(prob300,extd300,col="purple")
grid()
legend(0.2,0.27,c("L= 50","L= 100","L= 200","L= 300"),lty=c(1),col=c("blue","red","orange","purple"))

xxx=seq(0,0.06,length.out=1000)
size=c(50,100,150,200,250,300)
potsize=size**-0.75
dat=c(0.59160000000000001,0.59199999999999998,0.59309999999999996,0.59279999999999999,0.59399999999999997,0.59279999999999999)
plot(potsize,dat,pch=19,xlim=c(0,0.06),col="red",xlab=("L^(-1/nu)"),ylab="Prob. Crítica",main=paste("Tamaño muestra vs Probabilidad crítica"))
abline(v=0,col="blue")
grid()
pa=lm(dat~potsize)
summary(pa)
lines(xxx,xxx*-0.0463419+0.5939271,col="purple")
legend(0.03,0.594,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("p( L^(-1/nu) )    ","Ajuste Lineal    "))



#Grafica T.Cerradas

plot(prob50,left50,type="p",col="blue",pch=20,xlab="Probabilidad",ylab="Num. Trayectorias Cerradas",main=paste("Núm. Trayectorias Cerradas vs Probabilidad. L=50"))
lines(prob50,rght50,type="p",col="red",pch=20)
grid()
legend(0.2,0.7,c("N Trayect. Levóg.   ","N Trayect. Destróg.   "),pch=20,col=c("blue","red"))

plot(prob100,left100,type="p",pch=20,col="blue",xlab="Probabilidad",ylab="Num. Trayectorias Cerradas",main=paste("Núm. Trayectorias Cerradas vs Probabilidad. L=100"))
lines(prob100,rght100,type="p",pch=20,col="red")
grid()
legend(0.2,0.7,c("N Trayect. Levóg.   ","N Trayect. Destróg.   "),pch=20,col=c("blue","red"))

plot(prob200,left200,type="p",pch=20,col="blue",xlab="Probabilidad",ylab="Num. Trayectorias Cerradas",main=paste("Núm. Trayectorias Cerradas vs Probabilidad. L=200"))
lines(prob200,rght200,type="p",pch=20,col="red")
grid()
legend(0.2,0.7,c("N Trayect. Levóg.   ","N Trayect. Destróg.   "),pch=20,col=c("blue","red"))

plot(prob300,left300,type="p",pch=20,col="blue",xlab="Probabilidad",ylab="Num. Trayectorias Cerradas",main=paste("Núm. Trayectorias Cerradas vs Probabilidad. L=300"))
lines(prob300,rght300,pch=20,type="p",col="red")
grid()
legend(0.2,0.7,c("N Trayect. Levóg.   ","N Trayect. Destróg.   "),pch=20,col=c("blue","red"))


#Ajuste T.Cerrad

dat=read.table("/home/pj/Escritorio/simulacion/simulacion6/ajlinfrac.dat")
potsiz=dat[1:nrow(dat),1]
pc=dat[1:nrow(dat),2]
epc=dat[1:nrow(dat),3]

plot(potsiz,pc,xlim=c(0,0.06),col="red",ylab="Probabilidad",xlab=("L^(-1/nu)"),pch=20,main=paste("Tamaño muestra vs Probabilidad"))
abline(v=0,col="blue")


ext=seq(0,0.1,length.out = 10000)
lines(ext,-ext*0.000514018+0.592695,col="purple")
legend(0.033,0.5927,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("p( L^(-1/nu) )    ","Ajuste Lineal    "))










###Dimension Fractal

datt=read.table("/home/pj/Escritorio/simulacion/simulacion6/dimfractal/ajlindimfrac.dat")
lnsize=datt[1:nrow(datt),1]
lnsteps=datt[1:nrow(datt),2]
esteps=datt[1:nrow(datt),3]
steps=datt[1:nrow(datt),4]
size=datt[1:nrow(datt),5]


x=seq(0,7,length.out=10000)
plot(size,steps,type="p",col="blue",xlab="L",ylab="N",main=paste("Tamaño muestra vs Num. pasos trayect. extendidas"),pch=19)
grid()
legend(50,1e5,"Num. medio pasos N      ",pch=c(19),col="blue")

plot(lnsize,lnsteps,type="p",col="red",xlab="ln(L)",ylab="ln(N)",main=paste("Tamaño muestra vs Num. pasos trayect. extendidas"),pch=19)
lines(x,x*1.74303+0.812067,col="purple")
grid()
legend(3.85,11.7,c("ln Num.Med.Pasos ln(N)       ","Ajuste ln(N)=dimF*ln(L)       "),pch=c(c(19),c(26)),lty=c(c(0),c(1)),col=c("red","purple"))


##################PERCOLACION

###Ejemplo

x=read.table("/home/pj/Escritorio/simulacion/simulacion5/perc-matrix-10.dat")
a=x[1:nrow(x),1]
b=x[1:nrow(x),2]
c=x[1:nrow(x),3]

s=length(c)
p=c[1]+13
plot(a[1],b[1],main=paste("Ejemplo Percolación p=0.65"),xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(1,11),ylim=c(0,11),col="red",pch=p)
for (i in 2:s){
    q=c[i]+13
    if(c[i] > 2){
      points(a[i],b[i],pch=q,col="red")
    }else{
      points(a[i],b[i],pch=q)
    }
}

####Graficas

######CASO 10X10
malla10=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-10-1.dat")
pocup10=malla10[1:nrow(malla10),1]
pperc10=malla10[1:nrow(malla10),2]
error10=malla10[1:nrow(malla10),3]
plot(pocup10,pperc10,xaxs="i",yaxs="i",type="l",col="blue",xlab="Prob.Ocupación",ylab="Prob.Percolación",main=paste("Prob.Percolación vs Prob.Ocupación"))
grid()
epsilon=0.003
for (i in 1:nrow(malla10)){
  up=pperc10[i]+error10[i]
  low=pperc10[i]-error10[i]
  segments(pocup10[i],low,pocup10[i],up)
  segments(pocup10[i]-epsilon,low,pocup10[i]+epsilon,low)
  segments(pocup10[i]-epsilon,up,pocup10[i]+epsilon,up)
}
legend(0.15,0.9,"L=10",lty=c(1),col="blue")

#CASO 20X20
malla20=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-20-1.dat")
pocup20=malla20[1:nrow(malla20),1]
pperc20=malla20[1:nrow(malla20),2]
error20=malla20[1:nrow(malla20),3]
#CASO 30X30
malla30=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-30-1.dat")
pocup30=malla30[1:nrow(malla30),1]
pperc30=malla30[1:nrow(malla30),2]
error30=malla30[1:nrow(malla30),3]
#CASO 40X40
malla40=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-40-1.dat")
pocup40=malla40[1:nrow(malla40),1]
pperc40=malla40[1:nrow(malla40),2]
error40=malla40[1:nrow(malla40),3]
#CASO 50X50
malla50=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-50-1.dat")
pocup50=malla50[1:nrow(malla50),1]
pperc50=malla50[1:nrow(malla50),2]
error50=malla50[1:nrow(malla50),3]
#CASO 60X60
malla60=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-60-1.dat")
pocup60=malla60[1:nrow(malla60),1]
pperc60=malla60[1:nrow(malla60),2]
error60=malla60[1:nrow(malla60),3]
#CASO 70X70
malla70=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-70-1.dat")
pocup70=malla70[1:nrow(malla70),1]
pperc70=malla70[1:nrow(malla70),2]
error70=malla70[1:nrow(malla70),3]
#CASO 80X80
malla80=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-80-1.dat")
pocup80=malla80[1:nrow(malla80),1]
pperc80=malla80[1:nrow(malla80),2]
error80=malla80[1:nrow(malla80),3]

plot(pocup20,pperc20,ylim=c(-0.05,1.05),xlim=c(0.35,0.8),xaxs="i",yaxs="i",type="l",col="blue",xlab="Prob.Ocupación",ylab="Prob.Percolación",main=paste("Prob.Percolación vs Prob.Ocupación"),lwd=1.5)
lines(pocup30,pperc30,type="l",col="red",lwd=1.5)
lines(pocup40,pperc40,type="l",col="green",lwd=1.5)
lines(pocup50,pperc50,type="l",col="magenta",lwd=1.5)
lines(pocup60,pperc60,type="l",col="cyan",lwd=1.5)
lines(pocup70,pperc70,type="l",col="orange",lwd=1.5)
lines(pocup70,pperc80,type="l",col="purple",lwd=1.5)
grid()
epsilon=0.003
for (i in 1:nrow(malla20)){
  up=pperc20[i]+error20[i]
  low=pperc20[i]-error20[i]
  segments(pocup20[i],low,pocup20[i],up,col="blue")
  segments(pocup20[i]-epsilon,low,pocup20[i]+epsilon,low,col="blue")
  segments(pocup20[i]-epsilon,up,pocup20[i]+epsilon,up,col="blue")
}
for (i in 1:nrow(malla30)){
  up=pperc30[i]+error30[i]
  low=pperc30[i]-error30[i]
  segments(pocup30[i],low,pocup30[i],up,col="red")
  segments(pocup30[i]-epsilon,low,pocup30[i]+epsilon,low,col="red")
  segments(pocup30[i]-epsilon,up,pocup30[i]+epsilon,up,col="red")
}
for (i in 1:nrow(malla40)){
  up=pperc40[i]+error40[i]
  low=pperc40[i]-error40[i]
  segments(pocup40[i],low,pocup40[i],up,col="green")
  segments(pocup40[i]-epsilon,low,pocup40[i]+epsilon,low,col="green")
  segments(pocup40[i]-epsilon,up,pocup40[i]+epsilon,up,col="green")
}
for (i in 1:nrow(malla50)){
  up=pperc50[i]+error50[i]
  low=pperc50[i]-error50[i]
  segments(pocup50[i],low,pocup50[i],up,col="magenta")
  segments(pocup50[i]-epsilon,low,pocup50[i]+epsilon,low,col="magenta")
  segments(pocup50[i]-epsilon,up,pocup50[i]+epsilon,up,col="magenta")
}
for (i in 1:nrow(malla60)){
  up=pperc60[i]+error60[i]
  low=pperc60[i]-error60[i]
  segments(pocup60[i],low,pocup60[i],up,col="cyan")
  segments(pocup60[i]-epsilon,low,pocup60[i]+epsilon,low,col="cyan")
  segments(pocup60[i]-epsilon,up,pocup60[i]+epsilon,up,col="cyan")
}
for (i in 1:nrow(malla70)){
  up=pperc70[i]+error70[i]
  low=pperc70[i]-error70[i]
  segments(pocup70[i],low,pocup70[i],up,col="orange")
  segments(pocup70[i]-epsilon,low,pocup70[i]+epsilon,low,col="orange")
  segments(pocup70[i]-epsilon,up,pocup70[i]+epsilon,up,col="orange")
}
for (i in 1:nrow(malla80)){
  up=pperc80[i]+error80[i]
  low=pperc80[i]-error80[i]
  segments(pocup80[i],low,pocup80[i],up,col="purple")
  segments(pocup80[i]-epsilon,low,pocup80[i]+epsilon,low,col="purple")
  segments(pocup80[i]-epsilon,up,pocup80[i]+epsilon,up,col="purple")
}
legend(0.375,0.95,lty=c(1),lwd=1.5,c("L=20","L=30","L=40","L=50","L=60","L=70","L=80"),col=c("blue","red","green","magenta","cyan","orange","purple"))



#CASO 10X10
malla10=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-10-2.dat")
pocup10=malla10[1:nrow(malla10),1]
pperc10=malla10[1:nrow(malla10),2]
error10=malla10[1:nrow(malla10),3]
#CASO 20X20
malla20=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-20-2.dat")
pocup20=malla20[1:nrow(malla20),1]
pperc20=malla20[1:nrow(malla20),2]
error20=malla20[1:nrow(malla20),3]
#CASO 30X30
malla30=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-30-2.dat")
pocup30=malla30[1:nrow(malla30),1]
pperc30=malla30[1:nrow(malla30),2]
error30=malla30[1:nrow(malla30),3]
#CASO 40X40
malla40=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-40-2.dat")
pocup40=malla40[1:nrow(malla40),1]
pperc40=malla40[1:nrow(malla40),2]
error40=malla40[1:nrow(malla40),3]
#CASO 50X50
malla50=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-50-2.dat")
pocup50=malla50[1:nrow(malla50),1]
pperc50=malla50[1:nrow(malla50),2]
error50=malla50[1:nrow(malla50),3]
#CASO 60X60
malla60=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-60-2.dat")
pocup60=malla60[1:nrow(malla60),1]
pperc60=malla60[1:nrow(malla60),2]
error60=malla60[1:nrow(malla60),3]
#CASO 70X70
malla70=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-70-2.dat")
pocup70=malla70[1:nrow(malla70),1]
pperc70=malla70[1:nrow(malla70),2]
error70=malla70[1:nrow(malla70),3]
#CASO 80X80
malla80=read.table("/home/pj/Escritorio/simulacion/simulacion5/probperc-80-2.dat")
pocup80=malla80[1:nrow(malla80),1]
pperc80=malla80[1:nrow(malla80),2]
error80=malla80[1:nrow(malla80),3]

plot(pocup30,pperc30,xaxs="i",yaxs="i",type="l",col="blue",xlab="Prob.Ocupación",ylab="Prob.Percolación",main=paste("Prob.Percolación vs Prob.Ocupación"))
lines(pocup10,pperc10,type="l",col="gold")
lines(pocup20,pperc20,type="l",col="magenta")
lines(pocup40,pperc40,type="l",col="red")
lines(pocup50,pperc50,type="l",col="green")
lines(pocup60,pperc60,type="l",col="purple")
lines(pocup70,pperc70,type="l",col="darkorange2")
lines(pocup80,pperc80,type="l",col="navy")
grid()

epsilon=0.0001
for (i in 1:nrow(malla10)){
  up=pperc10[i]+error10[i]
  low=pperc10[i]-error10[i]
  segments(pocup10[i],low,pocup10[i],up,col="gold")
  segments(pocup10[i]-epsilon,low,pocup10[i]+epsilon,low,col="gold")
  segments(pocup10[i]-epsilon,up,pocup10[i]+epsilon,up,col="gold")
}
for (i in 1:nrow(malla20)){
  up=pperc20[i]+error20[i]
  low=pperc20[i]-error20[i]
  segments(pocup20[i],low,pocup20[i],up,col="magenta")
  segments(pocup20[i]-epsilon,low,pocup20[i]+epsilon,low,col="magenta")
  segments(pocup20[i]-epsilon,up,pocup20[i]+epsilon,up,col="magenta")
}
for (i in 1:nrow(malla30)){
  up=pperc30[i]+error30[i]
  low=pperc30[i]-error30[i]
  segments(pocup30[i],low,pocup30[i],up,col="blue")
  segments(pocup30[i]-epsilon,low,pocup30[i]+epsilon,low,col="blue")
  segments(pocup30[i]-epsilon,up,pocup30[i]+epsilon,up,col="blue")
}
for (i in 1:nrow(malla40)){
  up=pperc40[i]+error40[i]
  low=pperc40[i]-error40[i]
  segments(pocup40[i],low,pocup40[i],up,col="red")
  segments(pocup40[i]-epsilon,low,pocup40[i]+epsilon,low,col="red")
  segments(pocup40[i]-epsilon,up,pocup40[i]+epsilon,up,col="red")
}
for (i in 1:nrow(malla50)){
  up=pperc50[i]+error50[i]
  low=pperc50[i]-error50[i]
  segments(pocup50[i],low,pocup50[i],up,col="green")
  segments(pocup50[i]-epsilon,low,pocup50[i]+epsilon,low,col="green")
  segments(pocup50[i]-epsilon,up,pocup50[i]+epsilon,up,col="green")
}
for (i in 1:nrow(malla60)){
  up=pperc60[i]+error60[i]
  low=pperc60[i]-error60[i]
  segments(pocup60[i],low,pocup60[i],up,col="purple")
  segments(pocup60[i]-epsilon,low,pocup60[i]+epsilon,low,col="purple")
  segments(pocup60[i]-epsilon,up,pocup60[i]+epsilon,up,col="purple")
}
for (i in 1:nrow(malla70)){
  up=pperc70[i]+error70[i]
  low=pperc70[i]-error70[i]
  segments(pocup70[i],low,pocup70[i],up,col="darkorange2")
  segments(pocup70[i]-epsilon,low,pocup70[i]+epsilon,low,col="darkorange2")
  segments(pocup70[i]-epsilon,up,pocup70[i]+epsilon,up,col="darkorange2")
}
for (i in 1:nrow(malla80)){
  up=pperc80[i]+error80[i]
  low=pperc80[i]-error80[i]
  segments(pocup80[i],low,pocup80[i],up,col="navy")
  segments(pocup80[i]-epsilon,low,pocup80[i]+epsilon,low,col="navy")
  segments(pocup80[i]-epsilon,up,pocup80[i]+epsilon,up,col="navy")
}
legend(0.5965,0.534,cex=0.875,lty=c(1),c("L=10","L=20","L=30","L=40","L=50","L=60","L=70","L=80"),col=c("gold","magenta","blue","red","green","purple","darkorange2","navy"))


#AJUSTE LINEAL HECHO CON GNUPLOT...
ajustelineal=read.table("/home/pj/Escritorio/simulacion/simulacion5/ajlinperc.dat")
size=ajustelineal[1:nrow(ajustelineal),1]
lnsize=ajustelineal[1:nrow(ajustelineal),2]
pendiente=ajustelineal[1:nrow(ajustelineal),3]
lnpendiente=ajustelineal[1:nrow(ajustelineal),4]
sdpend=ajustelineal[1:nrow(ajustelineal),5]
interc=ajustelineal[1:nrow(ajustelineal),6]
sdinterc=ajustelineal[1:nrow(ajustelineal),7]




###Exp Critico
x=seq(2,5,length.out=10000)
plot(size,pendiente,main=paste("Tamaño muestra vs Pendiente Ajuste Lineal"),xlab="L",ylab="b",type="l",col="blue")
epsilon=0.75
for (i in 1:length(sdpend)){
  up=pendiente[i]+sdpend[i]
  low=pendiente[i]-sdpend[i]
  segments(size[i],low,size[i],up,col="black")
  segments(size[i]-epsilon,low,size[i]+epsilon,low,col="black")
  segments(size[i]-epsilon,up,size[i]+epsilon,up,col="black")
}
grid()
legend(15,19,lty=c(1),col="blue","b(L)")
plot(lnsize,lnpendiente,pch=19,col="red",main=paste("Tamaño muestra vs Pendiente Ajuste Lineal"),xlab="ln(L)",ylab="ln(b)")
lines(x,x*0.743012-0.241525,col="purple")
grid()
legend(2.3,3.035,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("ln Pendiente ln(b)","Ajuste ln(b)=1/nu*ln(L)"))

###Prob Critico
ajlintop=read.table("/home/pj/Escritorio/simulacion/simulacion5/ajlinprobtop.dat")
potsize=ajlintop[1:nrow(ajlintop),1]
top=ajlintop[1:nrow(ajlintop),2]
deltatop=ajlintop[1:nrow(ajlintop),3]
ajlinbot=read.table("/home/pj/Escritorio/simulacion/simulacion5/ajlinprobbot.dat")
bot=ajlinbot[1:nrow(ajlinbot),2]
deltabot=ajlinbot[1:nrow(ajlinbot),3]


plot(potsize,top,main=paste("Tamaño muestra vs Probabilidad Ocupación"),xlab=("L^(-1/nu)"),ylab="Prob.Ocupación",col="red",xlim=c(0,0.07),ylim=c(0.585,0.6),pch=19)
points(potsize,bot,pch=19,col="red")
abline(v=0,col="blue")
ext=seq(0,0.2,length.out = 10000)
#epsilon=0.001
#for (i in 1:length(potsize)){
#  up=top[i]+deltatop[i]
#  low=top[i]-deltatop[i]
#  segments(potsize[i],low,potsize[i],up,col="black")
#  segments(potsize[i]-epsilon,low,potsize[i]+epsilon,low,col="black")
#  segments(potsize[i]-epsilon,up,potsize[i]+epsilon,up,col="black")
#}

lines(ext,ext*0.0446338+0.594032,col="purple")
lines(ext,-ext*0.0565569+0.593866,col="purple")
legend(0.005,0.59,lty=c(c(0),c(1)),pch=c(19,27),col=c("red","purple"),c("p( L^(-1/nu) )  ","Ajuste Lineal  "))

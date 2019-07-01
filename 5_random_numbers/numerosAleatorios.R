##################ALEATORIOS

#######CAJAS Y BOLAS
bignum=read.table("/home/pj/Escritorio/simulacion4/cajitasybolitas/leynumgrandes.dat")
nlanzam=bignum[1:nrow(bignum),1]
mean=bignum[1:nrow(bignum),2]
var=bignum[1:nrow(bignum),3]

p=mean/nlanzam
func=sqrt(nlanzam*p*(1-p))

lnlan=log(nlanzam)
lvar=log(var)
aj=lm(lvar~lnlan)
aj

plot(nlanzam,var,xaxs="i",yaxs="i",type="p",pch=20,cex=0.75,col="2",xlab="Número de bolas lanzadas",ylab="Varianza",main=paste("Ley de los números grandes: Lineal"))
lines(nlanzam,func,type="l",col="blue")

plot(lnlan,lvar,xaxs="i",yaxs="i",type="p",pch=20,cex=0.75,col="orange",xlab="Número de bolas lanzadas",ylab="Varianza",main=paste("Ley de los números grandes: Logarítmica"))
lines(lnlan,0.505*lnlan-2.35,col="purple")


#######MONEDA TRUCADA
pasborr=read.table("/home/pj/Escritorio/simulacion/simulacion4/monedatrucada/mtruc-paseoborr.dat")
nlanzam=pasborr[1:nrow(pasborr),1]
good=pasborr[1:nrow(pasborr),2]
bad=pasborr[1:nrow(pasborr),3]

plot(nlanzam,bad,ylim=c(-300,200),xaxs="i",yaxs="i",type="p",main=paste("Paseo del borracho"),xlab="Núm Lanzamientos",ylab="Posición",pch=20,cex=0.5,col="red")
lines(nlanzam,good,type="p",pch=20,cex=0.5,col="blue")
grid()
legend(180,1000,pch=c(19,19),col=c("red","blue"),c("Trucada(0.501)  ","Justa(0.5)  "))


truncam=read.table("/home/pj/Escritorio/simulacion/simulacion4/monedatrucada/mtruc-vmedtruc.dat")
lanz=truncam[1:nrow(truncam),1]
med=truncam[1:nrow(truncam),2]
medp=truncam[1:nrow(truncam),3]
medm=truncam[1:nrow(truncam),4]
x=seq(0,10**10,length.out=10*10)

plot(lanz,med,xlim=c(0,5e6),ylim=c(0.49,0.515),xaxs="i",yaxs="i",type="l",main=paste("Estudio de la moneda trucada"),xlab="Núm Lanzamientos",ylab="Valor promedio",col="blue")
lines(lanz,medp,type="l",col="red")
lines(lanz,medm,type="l",col="red")
lines(x,x*0+0.5,col="purple")
abline(v=3600200,col="green")
grid()
legend(3e6,0.513,lty=c(1),col=c("blue","red","purple"),c("<x>","<x>+-3sigma  ","0.5"))





###TRANSP PARTICULAS

#Rep datos

a=read.table("//home/pj/Escritorio/electrones/transelect- 1.dat")
ax=a[1:nrow(a),1]
ay=a[1:nrow(a),2]
az=a[1:nrow(a),3]

b=read.table("//home/pj/Escritorio/electrones/transelect- 2.dat")
bx=b[1:nrow(b),1]
by=b[1:nrow(b),2]
bz=b[1:nrow(b),3]

c=read.table("//home/pj/Escritorio/electrones/transelect- 3.dat")
cx=c[1:nrow(c),1]
cy=c[1:nrow(c),2]
cz=c[1:nrow(c),3]

d=read.table("//home/pj/Escritorio/electrones/transelect- 4.dat")
dx=d[1:nrow(d),1]
dy=d[1:nrow(d),2]
dz=d[1:nrow(d),3]

e=read.table("//home/pj/Escritorio/electrones/transelect- 5.dat")
ex=e[1:nrow(e),1]
ey=e[1:nrow(e),2]
ez=e[1:nrow(e),3]

f=read.table("//home/pj/Escritorio/electrones/transelect- 6.dat")
fx=f[1:nrow(f),1]
fy=f[1:nrow(f),2]
fz=f[1:nrow(f),3]

g=read.table("//home/pj/Escritorio/electrones/transelect- 7.dat")
gx=g[1:nrow(g),1]
gy=g[1:nrow(g),2]
gz=g[1:nrow(g),3]

h=read.table("//home/pj/Escritorio/electrones/transelect- 8.dat")
hx=h[1:nrow(h),1]
hy=h[1:nrow(h),2]
hz=h[1:nrow(h),3]

i=read.table("//home/pj/Escritorio/electrones/transelect- 9.dat")
ix=i[1:nrow(i),1]
iy=i[1:nrow(i),2]
iz=i[1:nrow(i),3]

j=read.table("//home/pj/Escritorio/electrones/transelect-10.dat")
jx=j[1:nrow(j),1]
jy=j[1:nrow(j),2]
jz=j[1:nrow(j),3]

k=read.table("//home/pj/Escritorio/electrones/transelect-11.dat")
kx=k[1:nrow(k),1]
ky=k[1:nrow(k),2]
kz=k[1:nrow(k),3]

l=read.table("//home/pj/Escritorio/electrones/transelect-12.dat")
lx=l[1:nrow(l),1]
ly=l[1:nrow(l),2]
lz=l[1:nrow(l),3]

m=read.table("//home/pj/Escritorio/electrones/transelect-13.dat")
mx=m[1:nrow(m),1]
my=m[1:nrow(m),2]
mz=m[1:nrow(m),3]

n=read.table("//home/pj/Escritorio/electrones/transelect-14.dat")
nx=n[1:nrow(n),1]
ny=n[1:nrow(n),2]
nz=n[1:nrow(n),3]

o=read.table("//home/pj/Escritorio/electrones/transelect-15.dat")
ox=o[1:nrow(o),1]
oy=o[1:nrow(o),2]
oz=o[1:nrow(o),3]


plot3d(ax,ay,az,type="l",lwd=1.5,box=F,xlab="X",ylab="Y",zlab="",col="darkslateblue",xlim=c(-4,4),ylim=c(-4,4),zlim=c(0,4))
lines3d(bx,by,bz,col="red",lwd=1.5)
lines3d(cx,cy,cz,col="pink",lwd=1.5)
lines3d(dx,dy,dz,col="violet",lwd=1.5)
lines3d(ex,ey,ez,col="darkorange",lwd=1.5)
lines3d(fx,fy,fz,col="gold",lwd=1.5)
lines3d(gx,gy,gz,col="green",lwd=1.5)
lines3d(hx,hy,hz,col="cyan",lwd=1.5)
lines3d(ix,iy,iz,col="brown",lwd=1.5)
lines3d(jx,jy,jz,col="seagreen",lwd=1.5)
lines3d(kx,jy,kz,col="darkgoldenrod",lwd=1.5)
lines3d(lx,ly,lz,col="darkcyan",lwd=1.5)
lines3d(mx,my,mz,col="tan1",lwd=1.5)
lines3d(nx,ny,nz,col="mediumseagreen",lwd=1.5)
lines3d(ox,oy,oz,col="lightcyan4",lwd=1.5)
rgl.snapshot(filename=paste("Trayect.png"))


plot(ax,ay,type="l",main=paste("Proyección XY"),xlab="X(um)",ylab="Y(um)",col="darkslateblue",lwd=2.5,xlim=c(-4,4),ylim=c(-4,4))
lines(bx,by,col="red",lwd=2.5)
lines(cx,cy,col="pink",lwd=2.5)
lines(dx,dy,col="violet",lwd=2.5)
lines(ex,ey,col="darkorange",lwd=2.5)
lines(fx,fy,col="gold",lwd=2.5)
lines(gx,gy,col="green",lwd=2.5)
lines(hx,hy,col="cyan",lwd=2.5)
lines(ix,iy,col="brown",lwd=2.5)
lines(jx,jy,col="seagreen",lwd=2.5)
lines(kx,ky,col="darkgoldenrod",lwd=2.5)
lines(lx,ly,col="darkcyan",lwd=2.5)
lines(mx,my,col="tan1",lwd=2.5)
lines(nx,ny,col="mediumseagreen",lwd=2.5)
lines(ox,oy,col="lightcyan4",lwd=2.5)

plot(ax,az,type="l",main=paste("Proyección XZ"),xlab="X(um)",ylab="Z(um)",col="darkslateblue",lwd=2.5,xlim=c(-4,4),ylim=c(0,4))
lines(bx,bz,col="red",lwd=2.5)
lines(cx,cz,col="pink",lwd=2.5)
lines(dx,dz,col="violet",lwd=2.5)
lines(ex,ez,col="darkorange",lwd=2.5)
lines(fx,fz,col="gold",lwd=2.5)
lines(gx,gz,col="green",lwd=2.5)
lines(hx,hz,col="cyan",lwd=2.5)
lines(ix,iz,col="brown",lwd=2.5)
lines(jx,jz,col="seagreen",lwd=2.5)
lines(kx,kz,col="darkgoldenrod",lwd=2.5)
lines(lx,lz,col="darkcyan",lwd=2.5)
lines(mx,mz,col="tan1",lwd=2.5)
lines(nx,nz,col="mediumseagreen",lwd=2.5)
lines(ox,oz,col="lightcyan4",lwd=2.5)

plot(ay,az,type="l",main=paste("Proyección YZ"),xlab="X(um)",ylab="Z(um)",col="darkslateblue",lwd=2.5,xlim=c(-1,1),ylim=c(0,1))
lines(by,bz,col="red",lwd=2.5)
lines(cy,cz,col="pink",lwd=2.5)
lines(dy,dz,col="violet",lwd=2.5)
lines(ey,ez,col="darkorange",lwd=2.5)
lines(fy,fz,col="gold",lwd=2.5)
lines(gy,gz,col="green",lwd=2.5)
lines(hy,hz,col="cyan",lwd=2.5)
lines(iy,iz,col="brown",lwd=2.5)
lines(jy,jz,col="seagreen",lwd=2.5)
lines(ky,kz,col="darkgoldenrod",lwd=2.5)
lines(ly,lz,col="darkcyan",lwd=2.5)
lines(my,mz,col="tan1",lwd=2.5)
lines(ny,nz,col="mediumseagreen",lwd=2.5)
lines(oy,oz,col="lightcyan4",lwd=2.5)

###TRANSMISION

dattra=read.table("/home/pj/Escritorio/electrones/props/transelect-transm.dat")
z=dattra[1:nrow(dattra),1]
tr=dattra[1:nrow(dattra),2]
plot(z,tr,xaxs="i",yaxs="i",xlim=c(0,10),type="p",pch=20,col="blue",xlab="Profundidad(um)",ylab="Prob.Transmisión",main=paste("Probabilidad de Transmisión"))
legend(5,0.75,pch=20,col="blue","Prob.Transmisión     ")


###ABSORCION

databs=read.table("/home/pj/Escritorio/electrones/props/transelect-absorc.dat")
zz=databs[1:nrow(databs),1]
abso=databs[1:nrow(databs),2]
plot(zz,abso,xlim=c(0,10),xaxs="i",yaxs="i",type="p",pch=20,col="purple",xlab="Profundidad(um)",ylab="Prob.Absorción",main=paste("Probabilidad de Absorción"))
legend(5.5,0.003,pch=20,col="purple","Prob.Absorción    ")

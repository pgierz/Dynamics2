print("STRANGE ATTRACTORS-LORENZ SYSTEM")
r=24
r=26
s=10
b=8/3
dt=0.01
x=-0.6
y=0.1
z=0.1
vx<-c(0)
vy<-c(0)
vz<-c(0)
for(i in 1:10000){
x1=x+s*(y-x)*dt
y1=y+(r*x-y-x*z)*dt
z1=z+(x*y-b*z)*dt
vx[i]=x1
vy[i]=y1
vz[i]=z1
x=x1
y=y1
z=z1
}
plot(vx,vz,type="l",xlab="x",ylab="y",main="LORENZ ATTRACTOR")

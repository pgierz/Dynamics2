ni<-200 #number of grid cells
nt<-20000 #number of time steps

ia.0<-1:ni
ia.m1<-c(ni,1:(ni-1))
ia.p1<-c(2:ni,1)

g<-0.1 #9.81 m/s^2
dx<-1e5 #gridcell 10km 
dt<-100 #timstep 1 second
H<-1e3 #1km depth


u<-rep(0,ni) #speed at each point
h<-rep(0,ni) #pertubation at each point
u.new<-vector()
h.new<-vector()

#h[31:50,1]<--0.5  #one pertubation in the middle
#h[51:70,1]<-0.5   #one pertubation in the middle
h[50:90]<-sin(0:40/2*pi/20)


#1st step euler forward
u.new[ia.0]<-u[ia.0]-g*dt/2/dx*(h[ia.p1]-h[ia.m1])     #momentum equation
h.new[ia.0]<-h[ia.0]-H*dt/2*((u[ia.p1]-u[ia.m1])/dx)   #Continuity eq. horizontal divergence

#from step 3 on use Leapfrog
for (n in 2:(nt-1))
{
	u.old<-u
	h.old<-h
	h<-h.new
	u<-u.new


	u.new[ia.0]<-u.old[ia.0]-g*dt/dx*(h[ia.p1]-h[ia.m1]) 
	h.new[ia.0]<-h.old[ia.0]-H*dt*((u[ia.p1]-u[ia.m1])/dx)  


	if ((n%%10)==0)
	{	u.new[ia.0]<-(u.new[ia.0]+u[ia.0])/2
            h.new[ia.0]<-(h.new[ia.0]+h[ia.0])/2

      }


	if ((n%%101)==0) plot(h,type="l",lwd=2,ylim=c(-1,1))

	
}


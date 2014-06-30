#Dec 2006, 1D diffusion equation, explicit scheme 



#Constants
L.X<-50 #width of lattice
L.T<-5  #length of time
dx <- 1    #space step
dt <- 0.1   #time step
D<-1             #Diffusion coefficent




N.x<-L.X/dx + 2   #number of space boxes + 2 boundary boxes
N.t<-L.T/dt       #number of time boxes

u<-matrix(0,N.t,N.x)   #grid
u.temp<-rep(0,N.x)     #temporary vector which stores the state of of one timestep

u[1,N.x/2]<-1          #Set the starting and boundary condition, here one value in the middle


for (n in 1:(N.t-1))
{
	for (j in 2:(N.x-1))
	{
		u.temp[j]<-u[n,j]+D*dt/(dx^2)*(u[n,j+1]-2*u[n,j]+u[n,j-1])
	}
	u[n+1,]<-u.temp  
}
		

filled.contour((1:N.t)*dt,(1:N.x)*dx,u,color.palette=rainbow,xlab="time",ylab="space")

#This is just a definition of a function to plot vectorplots, you do not have to understand it...
par.uin<-function() 
  # determine scale of inches/userunits in x and y
  # from http://tolstoy.newcastle.edu.au/R/help/01c/2714.html
  # Brian Ripley Tue 20 Nov 2001 - 20:13:52 EST
 {
    u <- par("usr") 
    p <- par("pin") 
    c(p[1]/(u[2] - u[1]), p[2]/(u[4] - u[3]))
 }

quiver<-function(lon,lat,u,v,scale=1,length=0.05,maxspeed=200, ...) 
 # first stab at matlab's quiver in R
 # from http://tolstoy.newcastle.edu.au/R/help/01c/2711.html
 # Robin Hankin Tue 20 Nov 2001 - 13:10:28 EST
  {
    ypos <- lat[col(u)] 
    xpos <- lon[row(u)]

    speed <- sqrt(u*u+v*v) 

    u <- u*scale/maxspeed 
    v <- v*scale/maxspeed 
    

    matplot(xpos,ypos,type="p",cex=0,xlab="lon",ylab="lat", ...) 
    arrows(xpos,ypos,xpos+u,ypos+v,length=length*min(par.uin())) 
  }



#Program starts here

#Shallow water 2D,cyclic boundary conditions + Coriolis term

ni<-101  #number of gridcells in one direction
nt<-5000 #number of timesteps



#The physical constants
g<-0.1 #low gravity, 0.1 m/s^2
dx<-1e5 #gridcell 10km 
dy<-1e5
dt<-1000 #timstep 1000 second
H<-1e3 #1km depth
Omega<-1e-4




#define three index vectors.. the middle one , one shifted one cell to the left, and one to the right 
#(including the periodic boundary conditions)
ia.0<-1:ni
ia.m1<-c(ni,1:(ni-1))
ia.p1<-c(2:ni,1)
u<-matrix(0,ni,ni) #speed at each point
v<-matrix(0,ni,ni) #speed at each point
h<-matrix(0,ni,ni) #pertubation at each point
f<-matrix(0,ni,ni) #pertubation at each point

lat<-c(-50:50)*90/50
weight<-sin(lat*pi/180)

f<-rep(weight*2*Omega,each=ni)
dim(f)<-c(ni,ni)
filled.contour(f)

u.new<-u
h.new<-h
v.new<-v


#Inital condition: One smooth blobs at each side of the "equator"(sin)
h[60:80,60:80]<-sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))
h[20:40,20:40]<-sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))



#1st step euler forward
u.new[ia.0,ia.0]<-u[ia.0,ia.0]-g*dt/2/dx*(h[ia.p1,ia.0]-h[ia.m1,ia.0]) 
v.new[ia.0,ia.0]<-v[ia.0,ia.0]-g*dt/2/dy*(h[ia.0,ia.p1]-h[ia.0,ia.m1]) 
h.new[ia.0,ia.0]<-h[ia.0,ia.0]-H*dt/2*((u[ia.p1,ia.0]-u[ia.m1,ia.0])/dx + (v[ia.0,ia.p1]-v[ia.0,ia.m1])/dy)  #(du/dx + dv/dy)

#Divide the screen in two parts
par(mfcol=c(1,2))

#Leapfrog from the third step on
for (n in 3:(nt-1))
{
	
	u.old<-u
	v.old<-v
	h.old<-h
	h<-h.new
	u<-u.new
	v<-v.new

	u.new[ia.0,ia.0]<-u.old[ia.0,ia.0]-g*dt/dx*(h[ia.p1,ia.0]-h[ia.m1,ia.0])+dt*f*v
	v.new[ia.0,ia.0]<-v.old[ia.0,ia.0]-g*dt/dy*(h[ia.0,ia.p1]-h[ia.0,ia.m1])-dt*f*u
	h.new[ia.0,ia.0]<-h.old[ia.0,ia.0]-H*dt*((u[ia.p1,ia.0]-u[ia.m1,ia.0])/dx + (v[ia.0,ia.p1]-v[ia.0,ia.m1])/dy)  #(du/dx + dv/dy)
	
	
	#plot every 10th image
	if ((n %% 10) == 0) {
		image(h,zlim=c(-1,1),col=rainbow(200)) #this time as color coated 
		#persp(h/3, theta = 0, phi = 40, scale = FALSE, ltheta = -120, shade = 0.6, border = NA, box = FALSE,zlim=c(-0.3,0.3)) 
	 	quiver(-10:10,(-10:10)*9,u[(1:20)*5,(1:20)*5],v[(1:20)*5,(1:20)*5],scale=200,maxspeed=1,length=0.5)
	}

}




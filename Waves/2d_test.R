
ni <- 101 # number of gridcells in 1 direction
nt <- 1000 # number of timestesp
## Physical Constants
g <- 0.1
dx <- 1e5 #gridcell length (10 km)
dy <- 1e5
dt <- 1000 # timestep 1000 seconds
H <- 1000
Omega <- 1e-4
## Gridre
## Define three index vectors, the middle, one shifted to left and one shifted to right
ia.0 <- 1:ni
ia.m1 <- c(ni,1:(ni-1))
ia.p1 <- c(2:ni,1)
u <- matrix(0,ni,ni) # speed at each point
v <- matrix(0,ni,ni) # speed at each point
h <- matrix(0,ni,ni) # height at each point
f <- matrix(0,ni,ni) # pertubation at each point
lat <- c(-50:50)*90/50
weight <- sin(lat*pi/180) # sin of lat in radians (since R uses rad not deg)
f <- rep(weight*2*Omega, each=ni)
print(f)
dim(f) <- c(ni,ni)

## TODO: Make h.store for plotting via anim slider
u.new <- u
h.new <- h
v.new <- v

## Initial Conditions
## Two smooth blobs at each side of the equator
h[60:80,60:80] <- sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))
h[20:40,20:40] <- sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))

## 1st step euler forward
u.new[ia.0,ia.0]<-u[ia.0,ia.0]-g*dt/2/dx*(h[ia.p1,ia.0]-h[ia.m1,ia.0]) 
v.new[ia.0,ia.0]<-v[ia.0,ia.0]-g*dt/2/dy*(h[ia.0,ia.p1]-h[ia.0,ia.m1]) 
h.new[ia.0,ia.0]<-h[ia.0,ia.0]-H*dt/2*((u[ia.p1,ia.0]-u[ia.m1,ia.0])/dx + (v[ia.0,ia.p1]-v[ia.0,ia.m1])/dy)  #(du/dx + dv/dy)

## Divide the plot into two parts
                                        #par(mfcol=c(1,2))
for (n in 3:(nt-1)){
    print(n)
    u.old<-u
    v.old<-v
    h.old<-h
    h<-h.new
    u<-u.new
    v<-v.new

    u.new[ia.0,ia.0]<-u.old[ia.0,ia.0]-g*dt/dx*(h[ia.p1,ia.0]-h[ia.m1,ia.0])+dt*f*v
    v.new[ia.0,ia.0]<-v.old[ia.0,ia.0]-g*dt/dy*(h[ia.0,ia.p1]-h[ia.0,ia.m1])-dt*f*u
    h.new[ia.0,ia.0]<-h.old[ia.0,ia.0]-H*dt*((u[ia.p1,ia.0]-u[ia.m1,ia.0])/dx + (v[ia.0,ia.p1]-v[ia.0,ia.m1])/dy)  #(du/dx + dv/dy)                        

    ## Plot every 10th image
    ##            if ((n %% 10) == 0) {
image(h,zlim=c(-1,1),col=rainbow(200)) #this time as color coated
    ##                filled.contour(h,zlim=c(-1,1))
    ##persp(h/3, theta = 0, phi = 40, scale = FALSE, ltheta = -120, shade = 0.6, border = NA, box = FALSE,zlim=c(-0.3,0.3)) 
    ##quiver(-10:10,(-10:10)*9,u[(1:20)*5,(1:20)*5],v[(1:20)*5,(1:20)*5],scale=200,maxspeed=1,length=0.5)
    ##            }

}


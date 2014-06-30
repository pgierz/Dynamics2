library(shiny)
library(shinyIncubator)

## Define the server logic required for shallow water waves
shinyServer(function(input, output, session) {
    output$plot1 = renderPlot({        
        ni<-200 #number of grid cells
        nt<-1000 #number of time steps
        ia.0<-1:ni
        ia.m1<-c(ni,1:(ni-1))
        ia.p1<-c(2:ni,1)
        ## Constants
        dx<-1e5 #gridcell 10km 
        dt<-100 #timstep 1 second
        ## Depth from input
        H=input$depth#H<-1e3 #1km depth
        ## Gravity from input
        g=input$gravity
        u<-rep(0,ni) #speed at each point
        h<-rep(0,ni) #pertubation at each point
        u.new<-vector()
        h.new<-vector()
        h.store <- array()
        ## Make a perturbation
        h[50:90]<-sin(0:40/2*pi/20)
        h.store <- h
        u.new[ia.0]<-u[ia.0]-g*dt/2/dx*(h[ia.p1]-h[ia.m1])     #momentum equation
        h.new[ia.0]<-h[ia.0]-H*dt/2*((u[ia.p1]-u[ia.m1])/dx)   #Continuity eq. horizontal divergences
        h.store <- c(h.store, h.new)
        ##    withProgress(session, min = 1, max = (nt-1), {
        ##        setProgress(message = "Calculating, please wait.",
        ##                    detail = "This will take a few moments...")
        for (n in 2:(nt-1))
            {
                u.old<-u
                h.old<-h
                h<-h.new
                u<-u.new
                u.new[ia.0]<-u.old[ia.0]-g*dt/dx*(h[ia.p1]-h[ia.m1]) 
                h.new[ia.0]<-h.old[ia.0]-H*dt*((u[ia.p1]-u[ia.m1])/dx)          
                if ((n%%10)==0)
                    {
                        u.new[ia.0]<-(u.new[ia.0]+u[ia.0])/2
                        h.new[ia.0]<-(h.new[ia.0]+h[ia.0])/2
                    }
                h.store <- c(h.store, h.new)
                ##                setProgress(value = n)
                ##                if ( (nt-1)/2 <= n & n < (nt-1)*0.75)
                ##                    setProgress(detail = "Halfway! Be patient...")
                ##                if ( (nt-1)*0.75 <= n & n < (nt-1)*0.95)
                ##                    setProgress(detail = "Still working...")
                ##                if (n >= (nt-1)*0.95)
                ##                    setProgress(detail = "Almost done...")
            }
        ##    })
        dim(h.store) <- c(ni,nt)

        plot(h.store[1:200,input$animation], type="l",lwd=2,ylim=c(-1,1), ylab = "Wave height", xlab = "", col = "blue")
    })
    output$plot2 = renderPlot({
        ni <- 101 # number of gridcells in 1 direction
        nt <- 1000 # number of timestesp
        ## Physical Constants
        g <- input$gravity2
        dx <- 1e5 #gridcell length (10 km)
        dy <- 1e5
        dt <- 1000 # timestep 1000 seconds
        H <- input$depth2
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
        dim(f) <- c(ni,ni)

        ## TODO: Make h.store for plotting via anim slider
        u.new <- u
        h.new <- h
        v.new <- v
        h.store <- array()
        ## Initial Conditions
        ## Two smooth blobs at each side of the equator
        h[60:80,60:80] <- sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))
        h[20:40,20:40] <- sin(0:20/2*pi/10)%*%t(sin(0:20/2*pi/10))
        h.store = h

        ## 1st step euler forward
        u.new[ia.0,ia.0]<-u[ia.0,ia.0]-g*dt/2/dx*(h[ia.p1,ia.0]-h[ia.m1,ia.0]) 
        v.new[ia.0,ia.0]<-v[ia.0,ia.0]-g*dt/2/dy*(h[ia.0,ia.p1]-h[ia.0,ia.m1]) 
        h.new[ia.0,ia.0]<-h[ia.0,ia.0]-H*dt/2*((u[ia.p1,ia.0]-u[ia.m1,ia.0])/dx + (v[ia.0,ia.p1]-v[ia.0,ia.m1])/dy)  #(du/dx + dv/dy)
        h.store = c(h.store, h.new)

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

            h.store = c(h.store, h.new)
        }
        dim(h.store) <- c(ni,ni,(nt-1))
        image(h.store[,,input$animation2], zlim=c(-1,1), col=rainbow(200))
    })
})

#Wind plotting routines


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

quiver<-function(lon,lat,u,v,scale=1,length=0.2,maxv=max(abs(na.omit(u)),abs(na.omit(v))), ...) 
 # first stab at matlab's quiver in R
 # from http://tolstoy.newcastle.edu.au/R/help/01c/2711.html
 # Robin Hankin Tue 20 Nov 2001 - 13:10:28 EST
  {
    ypos <- lat[col(u)] 
    xpos <- lon[row(u)]

    speed <- sqrt(u*u+v*v) 

    u <- u*scale/maxv
    v <- v*scale/maxv
    

   # matplot(xpos,ypos,type="p",cex=0,xlab="lon",ylab="lat", ...) 
    arrows(xpos,ypos,xpos+c(u),ypos+c(v),length=length*min(par.uin())) 
   
  }


addwind<-function(dataU,dataV,shift=F)
{
	tmpV<-plot.preparation(dataV,shift)
	tmpU<-plot.preparation(dataU,shift)
	
	quiver(tmpV$lon,tmpU$lat,tmpU$data,tmpV$data,length=2,scale=10)
	quiver(tmpV$lon+360,tmpU$lat,tmpU$data,tmpV$data,length=2,scale=10)
	quiver(tmpV$lon-360,tmpU$lat,tmpU$data,tmpV$data,length=2,scale=10)

}

plot(selspace(wind.pi.mean.u10,lat1=30,lat2=90,lon1=250,lon2=30),FUN=addwind(wind.pi.mean.u10,wind.pi.mean.v10))
#newFig()
#plot(selspace(wind.lgm.mean.u10,lat1=30,lat2=90,lon1=250,lon2=30),FUN=addwind(wind.lgm.mean.u10,wind.lgm.mean.v10))

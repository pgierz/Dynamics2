#ownfunctions.R
#here you can put your own and/or updated functions

sb<-function(a,b,debug=FALSE)
  {
    Na<-length(time(a))
    Nb<-length(time(b))
    tStart<-max(time(a)[1],time(b)[1])
    tEnd<-min(time(a)[Na],time(b)[Nb])
    if (debug) print(paste("Start",tStart,"End",tEnd))
    return(list(a=window(a,tStart,tEnd),b=window(b,tStart,tEnd)))
  }


#Not yet optimized for speed !
areamean<-function(data)
{
	
	temp<-attributes(data)
	nlon<-length(temp$lon)
	nlat<-length(temp$lat)
	ntime<-length(time(data))

	data3D<-array(data,c(ntime,nlon,nlat)) #make a 2D array
	weight<-t(matrix(cos(temp$lat/180*pi),nlat,nlon))
	result<-vector()
	for (i in 1:ntime) result[i]<-(sum(weight*data3D[i,,])/sum(weight))
	return(pTs(result,time(data),0,0,paste("areamean",getname(data))))
	#Jetzt Spalte davor und danach hinzufügen

}





cor.sb<-function(a,b)
  {
    t<-sb(a,b)
    return(cor.test(t$a,t$b))
  }

applyspace<-function(data,FUN, ...)
{
     index<-!is.na(colSums(data)) 
   ts<-apply(data[,index],1,FUN, ...)
   return(pTs(ts,time(data),name=getname(data)))
       }
applytime<-function(data,FUN,newtime=NULL, ...)
{
   if (is.null(newtime)) newtime<-mean(time(data))
   field<-apply(data,2,FUN, ...)
   return(pField(field,newtime,getlat(data),getlon(data),name=getname(data)))
}
composite <-function(ts,field,sign=FALSE,sp=sd(ts),sm=(-1*sp),anomaly=T)
{
        temp<-attributes(field)
     if (anomaly) field<-scale(field,scale=FALSE)
        ts<-scale(ts)
#bring both on the same time basis
        start<-max(start(ts)[1],start(field)[1])
        end<-min(end(ts)[1],end(field)[1])
        print(paste("Common time period: ",start,end))
        ts<-window(ts,start,end)
        field<-unclass(window(field,start,end))

        ts[is.na(ts)]<-0
        if (sign)
        {
                index.plus<-ts>0
                index.minus<-ts<0
        }
        else
        {
                
                
                index.plus <- ts>sp
                index.minus <- ts<sm
        }
        
        field.plus<-field[index.plus,]
        field.minus<-field[index.minus,]
       
        field.plus<-pField(colMeans(field.plus),9999,temp$lat,temp$lon,paste("+ Compos.",getname(ts),temp$name),temp$history,date=FALSE)
        field.minus<-pField(colMeans(field.minus),9999,temp$lat,temp$lon,paste("- Compos.",getname(ts),temp$name),temp$history,date=FALSE)
 
        return(list(plus=field.plus,minus=field.minus))
        

}



plotmap.square <- function(plotdata,main=NULL,zlim=range(plotdata,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=rbow,FUN=NULL, ...)
{
	temp<-attributes(plotdata)
      sSub<-NULL
	if (time(plotdata) != 9999) sSub<-paste("time:",format(time(plotdata)))
	if (is.null(main)) main<-temp$name
        tmp<-plot.preparation(plotdata)
	plotsquare(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,color=palette,plot.title={
        title(main=main,sub=sSub);
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)
        addland(col="black");
        grid()})
      }

plotsquare<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), 
    zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), 
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
        1), plot.title, plot.axes, key.title, key.axes, asp = NA, 
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, 
    ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    #plot.new()
    #plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    image(x,y,z,col=col,zlim=zlim)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            axis(1)
            axis(2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}

#RMSE without any error checking
rmse<-function(data1,data2)
{	
	sqrt(mean((data1-data2)^2))
}

cbind.pTs<-function(...,deparse.level = 1)
  {
	
     result<-cbind(...,deparse.level = 1)
    args <- list(...)
    lat<-NULL
    lon<-NULL
    name<-NULL
    for (a in args)
      {
        lat<-c(lat,getlat(a))
        lon<-c(lon,getlon(a))
        name<-c(name,getname(a))
	  time<-time(a)
      }
    return(pTs(result,time,lat,lon,name,"cbind"))
  }


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
	quiver(tmpV$lon+360,tmpU$lat,tmpU$data,tmpV$data,length=1,scale=10)
	quiver(tmpV$lon-360,tmpU$lat,tmpU$data,tmpV$data,length=1,scale=10)




}

#plot(selspace(wind.pi.mean.u10,lat1=30,lat2=90,lon1=250,lon2=30),FUN=addwind(wind.pi.mean.u10,wind.pi.mean.v10))
#newFig()
#plot(selspace(wind.lgm.mean.u10,lat1=30,lat2=90,lon1=250,lon2=30),FUN=addwind(wind.lgm.mean.u10,wind.lgm.mean.v10))


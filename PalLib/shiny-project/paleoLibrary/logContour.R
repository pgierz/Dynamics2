test<-matrix(1:100,10,10)


filled.contour.own<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
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
  #  rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    
    delta<-(levels[length(levels)]-levels[1])/(length(levels)-1)
    breaks<-delta*(0:(length(levels)-1))+levels[1]
    rect(0, breaks[-length(levels)], 1, breaks[-1], col = col)
 
    if (missing(key.axes)) {
        if (axes) 
		{    #use modified axes
            	axis(4,labels=levels,at=delta/2+breaks)
		}
    }	
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
        col = col))
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
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


plotlog <- function(plotdata,main=NULL,zlim=range(plotdata,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=rbow,FUN=NULL, ...)
{
	temp<-attributes(plotdata)
      sSub<-NULL
	if (time(plotdata) != 9999) sSub<-paste("time:",format(time(plotdata)))
	sSub=""
	if (is.null(main)) main<-temp$name
        tmp<-plot.preparation(plotdata)
	filled.contour.own(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,color=palette,plot.title={
        title(main=main,sub=sSub);
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)
        addland(col="black");
        grid()})
      }

plotlog(selspace(T0K-T7K,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-0.5,0.5),levels=c(-1,-0.7,-0.4,-0.2,-0.1,-0.03,0,0.03,0.1,0.2,0.4,0.7,1),sSub=0,"present-7K, polynomial model",FUN=plotpolygon())
plotlog(selspace(diff8K.field.linear*transfer.field,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-0.5,0.5),levels=c(-1,-0.7,-0.4,-0.2,-0.1,0,0.1,0.2,0.4,0.7,1),sSub=0,"present-7K, only bottom cut")
plotlog(selspace(diff8K.field.linear*transfer.field,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-0.5,0.5),levels=c(-1,-0.7,-0.4,-0.2,-0.1,0,0.1,0.2,0.4,0.7,1),sSub=0,"present-7K, linear response")

plotlog(selspace(diff8K.field.half*transfer.field,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-0.5,0.5),levels=c(-1,-0.7,-0.4,-0.2,-0.1,0,0.1,0.2,0.4,0.7,1),sSub=0,"present-7K, summer response (50%)")
plotlog(selspace(diff8K.field.i.half*transfer.field,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-0.5,0.5),levels=c(-1,-0.7,-0.4,-0.2,-0.1,0,0.1,0.2,0.4,0.7,1),sSub=0,"present-7K, winter response (50%)")


plot(selspace(diff8K.field.linear,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-5,5),sSub="","present-7K, linear response")
plot(selspace(diff8K.field.half,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-10,10),sSub="","present-7K, summer response (50%)")
plot(selspace(diff8K.field.i.half,lat1=-90,lat2=90,lon1=180,lon2=175),zlim=c(-10,10),sSub="","present-7K, winter response (50%)")





filled.contour(test,levels=c(0,20,40,60,80,100))
filled.contour.own(test,levels=c(1,2,5,10,50,100))

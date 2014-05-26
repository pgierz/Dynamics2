#plotting library, 15.August.06  tlaepple@awi-bremerhaven.de


#plot a eof calculated from multiple timeseries
#ts for getting the lat's and lon's
ploteof <- function(pcomp,ts,i=1,minLat=NULL,maxLat=NULL,minLon=NULL,maxLon=NULL)
{

 if (is.null(c(minLat,maxLat,minLon,maxLon))) 
 {
	minLat<-min(getlat(ts))
	maxLat<-max(getlat(ts))
	extraLat<-(maxLat-minLat)/8
	minLat<-minLat-extraLat
	maxLat<-maxLat+extraLat
 
	minLon<-min(getlon(ts))
	maxLon<-max(getlon(ts))
	
	extraLon<-(maxLon-minLon)/8
	minLon<-minLon-extraLon
	maxLon<-maxLon+extraLon
 }

	#Give colors to the points
	a<-vector()
	a[pcomp$eof[,i]>0]<-"red"
	a[pcomp$eof[,i]<0]<-"blue"
	titlevar<-sprintf("%2.1f",pcomp$var[i]/pcomp$varsum*100)
	title=paste("EOF ",i,"  Expl.Var:",titlevar,"%",  sep="")
	plot(getlon(ts),getlat(ts),ylim=c(minLat,maxLat),xlim=c(minLon,maxLon),lwd=4,xlab="EAST",ylab="NORTH",col=a,main=title)
	text(getlon(ts),getlat(ts),sprintf("%1.2f",pcomp$eof[,i]),pos=1,offset=c(1,1))

	text(getlon(ts),getlat(ts),getname(ts),pos=3,offset=c(1,1))
	
	addland(col="black")
	grid()
	
}

plotwind<-function(data,title=NULL,zlim=range(data,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=rbow,FUN=NULL, ...)
  {
    tmp<-attributes(data)
    dim(data)<-c(length(getlat(data)),length(getlon(data)))
    sSub<-""

filled.contour(tmp$lat,tmp$lon,data,zlim=zlim,nlevels=nlevels,levels=levels,color=palette,plot.title={
        title(main=title,sub=sSub);
        if (!is.null(FUN)) FUN(tmp$lat,tmp$lon,data)
        addland(col="black");
        grid()})
      }



#Plots an index 
plotindex <- function(data,rintervall=5,lwd=3,lwd1=2,main=NULL)
{
	if (is.null(main)) main<-getname(data)
	data.plus<-data
	data.minus<-data
	data.plus[data<=0]<-0
	data.minus[data>=0]<-0
	plot.ts(data,type="n",xlab="time",ylab="std",main=main)
	lines(data.minus,type="h",col="blue",lwd=lwd)
	lines(data.plus,type="h",col="red",lwd=lwd)
	lines(rollmean(data,5),col="black",lwd=lwd1)
}



plot.pTs<-function(x, plot.type = c("multiple", "single"), xy.labels, 
    xy.lines, panel = lines, nc, yax.flip = FALSE, mar.multi = c(0, 
        5.1, 0, if (yax.flip) 5.1 else 2.1), oma.multi = c(6, 
        0, 5, 0), axes = TRUE, main=NULL,...)
{
 
 if (!is.null(ncol(x))) colnames(x)<-strtrim(getname(x),8) else if (is.null(main)) main=getname(x)
 plot.ts(x, NULL, plot.type, xy.labels, 
    xy.lines, panel, nc, yax.flip , mar.multi, oma.multi,axes = TRUE,main=main, ...)
}

plot.pField <- function(x, ...) plotmap(x, ...)
plotmap<-function(plotdata, ...) UseMethod("plotmap")

myfun1<-function(sTitle,sSub)
{
       title(main=sTitle,sub=sSub)
       addland(col="black")
       grid()
}

myfun2<-function(sTitle,sSub,lat,lon,plotdata)
{ title(main=sTitle,sub=sSub)
contour(lon,lat,plotdata,col="white",zlim=c(min(plotdata),0),lty=1,lwd=2,nlevels=5,add=TRUE);
contour(lon,lat,plotdata,col="grey20",zlim=c(0,max(plotdata)),lty=1,lwd=2,nlevels=5,add=TRUE);
addland(col="black")
grid()
}

#checks the data
#wrap the data to get a continous lat/lon field
#reverse latitudes if needed
plot.preparation <- function(plotdata)
{
        temp<-attributes(plotdata)
  	if (prod(dim(plotdata)) != length(temp$lon)*length(temp$lat)) stop("N(data) != N(lat)*N(lon)")

        plotdata<-matrix(plotdata,length(temp$lon),length(temp$lat)) #make a 2D array
        
        #arrange to get a continous field
        d<-diff(temp$lon)
        if (max(d) > (min(d)+0.01))
          { nlon<-length(temp$lon)
            edgelon<-which(d==max(d))
            plotdata<-rbind(plotdata[(edgelon+1):nlon,],plotdata[1:edgelon,])
            temp$lon<-c(temp$lon[(edgelon+1):nlon],temp$lon[1:edgelon]+360)
          }
        
        
        if (temp$lat[2] < temp$lat[1])    #if the latitudes are from + to -, reverse them
         {
          temp$lat<-rev(temp$lat)
          plotdata<-plotdata[,rev(seq(len=ncol(plotdata)))]
        }
        return(list(data=plotdata,lat=temp$lat,lon=temp$lon))
}

icecontour<-function(lon,lat,data)
  {
    contour(lon,lat,data,levels=0.5,add=TRUE,drawlabels=FALSE,col="blue",lwd=2)
  }
normcontour1<-function(lon,lat,data)
  {

    levels<-(1:5)/5
    contour(lon,lat,data,col="black",zlim=c(min(data),0),lty=2,lwd=1,levels=-1*levels,add=TRUE,drawlabels=FALSE)
    contour(lon,lat,data,col="black",zlim=c(0,max(data)),lty=1,lwd=1,levels=levels,add=TRUE,drawlabels=FALSE)
    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }
normcontour0<-function(lon,lat,data)
  {

    levels<-(1:8)/80
    contour(lon,lat,data,col="black",zlim=c(min(data),0),lty=2,lwd=1,levels=-1*levels,add=TRUE,drawlabels=FALSE)
    contour(lon,lat,data,col="black",zlim=c(0,max(data)),lty=1,lwd=1,levels=levels,add=TRUE,drawlabels=FALSE)
    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }

pnacontour<-function(lon,lat,data)
  {

    levelsp=c(0.25,0.5,0.75)
    levelsm=c(-0.25,-0.5,-0.75)
    
    contour(lon,lat,data,col="black",lty=2,lwd=1,levels=levelsp,add=TRUE,drawlabels=FALSE)
    contour(lon,lat,data,col="black",lty=1,lwd=1,levels=levelsm,add=TRUE,drawlabels=FALSE)
    contour(lon,lat,data,col="grey",lty=1,lwd=2,levels=c(0),add=TRUE,drawlabels=FALSE)

    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }


corcontour<-function(lon,lat,data)
  {

    levelsp=c(0.2,0.4,0.6)
    levelsm=c(-0.2,-0.4,-0.6)
    
    contour(lon,lat,data,col="black",lty=2,lwd=1,levels=levelsp,add=TRUE,drawlabels=TRUE)
    contour(lon,lat,data,col="black",lty=1,lwd=1,levels=levelsm,add=TRUE,drawlabels=TRUE)
    contour(lon,lat,data,col="grey",lty=1,lwd=2,levels=c(0),add=TRUE,drawlabels=TRUE)

    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }



gphcontour<-function(lon,lat,data)
  {

    levelsp=c(5,10,15,20)
    levelsm=c(-5,-10,-15,-20)
    
    contour(lon,lat,data,col="black",lty=2,lwd=1,levels=levelsp,add=TRUE,drawlabels=TRUE)
    contour(lon,lat,data,col="black",lty=1,lwd=1,levels=levelsm,add=TRUE,drawlabels=TRUE)
    contour(lon,lat,data,col="grey",lty=1,lwd=2,levels=c(0),add=TRUE,drawlabels=TRUE)

    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }


cwind<-function(lon,lat,data)
  {

    levelsp<-(1:5)*8
    levelsm<--2*(1:10)

    contour(lon,lat,data,col="black",zlim=c(min(data),0),lty=2,lwd=1,levels=levelsm,add=TRUE,drawlabels=FALSE)
    contour(lon,lat,data,col="black",zlim=c(0,max(data)),lty=1,lwd=1,levels=levelsp,add=TRUE,drawlabels=FALSE)

  }

rbow <- function (n, s = 1, v = 1, start = 0, end = 0.7, gamma = 1) 
{
    if ((n <- as.integer(n[1])) > 0) {
        if (start == end || any(c(start, end) < 0) || any(c(start, 
            end) > 1)) 
            stop("'start' and 'end' must be distinct and in [0, 1].")
        result<-hsv(h = seq(start, ifelse(start > end, 1, 0) + end, length = n)%%1, 
            s, v, gamma)
        reverse(result)    
    }
    else character(0)
}
plotmap.pField <- function(plotdata,main=NULL,zlim=range(plotdata,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=rbow,FUN=NULL, ...)
{
	temp<-attributes(plotdata)
      sSub<-NULL
	if (time(plotdata) != 9999) sSub<-paste("time:",format(time(plotdata)))
	if (is.null(main)) main<-temp$name
        tmp<-plot.preparation(plotdata)
	filled.contour(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,color=palette,plot.title={
        title(main=main,sub=sSub);
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)
        addland(col="black");
        grid()})
      }

plotmap.pFieldb <- function(plotdata,plotdata2,title=NULL,zlim=range(plotdata,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=rbow,FUN=NULL, ...)
{
	temp<-attributes(plotdata)
      sSub<-NULL
	if (time(plotdata) != 9999) sSub<-paste("time:",format(time(plotdata)))
	if (is.null(title)) title<-temp$name
        tmp<-plot.preparation(plotdata)
	  tmp2<-plot.preparation(plotdata2)

	filled.contour(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,color=palette,plot.title={
        title(main=title,sub=sSub);
        if (!is.null(FUN)) FUN(tmp2$lon,tmp2$lat,tmp2$data)
        addland(col="black");
        grid()})
      }


plotmapc.pField <- function(plotdata,sTitle=NULL, ...)
{
	temp<-attributes(plotdata)
 	sSub<-paste("time:",format(time(plotdata)))
	if (is.null(sTitle)) sTitle<-temp$name

	if (prod(dim(plotdata)) != length(temp$lon)*length(temp$lat)) stop("N(data) != N(lat)*N(lon)")

        plotdata<-matrix(plotdata,length(temp$lon),length(temp$lat)) #make a 2D array
        
        #arrange to get a continous field
        d<-diff(temp$lon)
        if (max(d) > (min(d)+0.01))
          { nlon<-length(temp$lon)
            edgelon<-which(d==max(d))
            plotdata<-rbind(plotdata[(edgelon+1):nlon,],plotdata[1:edgelon,])
            temp$lon<-c(temp$lon[(edgelon+1):nlon],temp$lon[1:edgelon]+360)
          }
        
        
        if (temp$lat[2] < temp$lat[1])    #if the latitudes are from + to -, reverse them
         {
          temp$lat<-rev(temp$lat)
          plotdata<-plotdata[,rev(seq(len=ncol(plotdata)))]
                    }
	
	 levels=(0:6)*0.01
	 contour(temp$lon,temp$lat,plotdata,col="blue",zlim=c(min(plotdata),0),lty=1,lwd=2,levels=-1*levels)
         contour(temp$lon,temp$lat,plotdata,col="red",zlim=c(0,max(plotdata)),lty=2,lwd=2,levels=levels,add=TRUE)
     title(main=sTitle,sub=sSub);addland(col="black");grid();
      }

plotcont.pField <- function(plotdata,sTitle=NULL, ...)
{
	temp<-attributes(plotdata)
 	sSub<-paste("time:",format(time(plotdata)))
	if (is.null(sTitle)) sTitle<-temp$name

	if (prod(dim(plotdata)) != length(temp$lon)*length(temp$lat)) stop("N(data) != N(lat)*N(lon)")

        plotdata<-matrix(plotdata,length(temp$lon),length(temp$lat)) #make a 2D array
        
        #arrange to get a continous field
        d<-diff(temp$lon)
        if (max(d) > (min(d)+0.01))
          { nlon<-length(temp$lon)
            edgelon<-which(d==max(d))
            plotdata<-rbind(plotdata[(edgelon+1):nlon,],plotdata[1:edgelon,])
            temp$lon<-c(temp$lon[(edgelon+1):nlon],temp$lon[1:edgelon]+360)
          }
        
        
        if (temp$lat[2] < temp$lat[1])    #if the latitudes are from + to -, reverse them
         {
          temp$lat<-rev(temp$lat)
          plotdata<-plotdata[,rev(seq(len=ncol(plotdata)))]
                    }
	 
      	 contour(temp$lon,temp$lat,plotdata,col="blue",zlim=c(min(plotdata),0),lty=1,lwd=2,nlevels=5)
         contour(temp$lon,temp$lat,plotdata,col="red",zlim=c(0,max(plotdata)),lty=2,lwd=2,nlevels=5,add=TRUE)
     title(main=sTitle,sub=sSub);addland(col="black");grid();
      }


makefilm<-function(data,startdate,enddate,...,avrg=11,step=5,prefix="ani_",anomaly=FALSE)
{

meandata<-applyspace(data,mean)
meandata<-na.omit(rollmean(meandata,avrg))
ypos<-max(meandata)

if (anomaly) data<-data-applytime(data,mean)

nStep<-(enddate-startdate) %/% step 
starttime=(1:nStep)*step + startdate
starttime<-starttime[starttime<(enddate-avrg)]


i<-100
for (it in starttime)
{
  i<-i+1
  tdata<-window(data,it,it+avrg)
  result<-applytime(tdata,mean)
  jpeg(paste(prefix,i,".jpg",sep=""),width=600,height=600,quality=90)

  plot(result,title=paste(it,"-",it+avrg),...)

  dev.off()
 
}
}

#### import of ncdf field data

read_data <- function(FILENAME="",varname=NULL,name="",lonname=NULL,latname=NULL,missVal=NULL,cal=NULL,plot.hist=FALSE){ #example: missVal=1e+20   #cal can be "365days" or "standard"
  temp.nc = open.ncdf(FILENAME)
  
  # read varname from temp.nc
  if(is.null(varname)){
    if(temp.nc$nvars>1){
      varnames<-c()
      for (i in 1:length(temp.nc$var)){
        varnames<-c(varnames,temp.nc$var[[i]]$name)
      }
      print("following varnames are given:")
      for (i in 1:length(varnames)){print(varnames[i])}
      stop("you have to specify varname")
    }
    varname<-temp.nc$var[[1]]$name
    print(paste("Varname used:",varname))
  }
  
  # read name for lon and lat variabels from temp.nc
  if(is.null(lonname)){
    lonnames<-c("lon","longitude") # list of known lonnames
    lonname<-find.var(temp.nc,lonnames)[1]
  }
  if(is.null(latname)){
    latnames<-c("lat","latitude")   # list of known latnames
    latname<-find.var(temp.nc,latnames)[1]
  }
  
  #Read out the data
  temp.time <- get.var.ncdf(temp.nc,"time")
  temp.data <-get.var.ncdf(temp.nc,varname)
  temp.lat <-get.var.ncdf(temp.nc,latname)
  temp.lon <-get.var.ncdf(temp.nc,lonname)
  
  
  
  #convert from missVal given values to NA
  if(is.null(missVal)){
    #missVal.nc<-temp.nc$var[[1]]$missval
    print(paste("Given missing Value of netcdf data set is:",temp.nc$var[[1]]$missval,". To use this value you need to specify missVal=..."))
    # Try to assume missval by 2 conditions (The function does not use the missval given by the netcdf data set because usually it doesn't work)
    #   1. missval should be maximal absolute value of data
    #   2. missval should be maximal in histogram
    m.max<-max(temp.data, na.rm=T)                                                   
    m.min<-min(temp.data, na.rm=T)                                                   
    if (abs(m.max)>=abs(m.min)) m.guess<-m.max else m.guess<-m.min          
    m.h<-hist(temp.data,plot=F,n=10000)                                     
    m.h.max<-which(m.h$counts==max(m.h$counts))
    if((m.h.max==length(m.h$counts)&&m.guess==m.max)||(m.h.max==1&&m.guess==m.min)){
      missVal <- round(m.guess, digits=3)
      print(paste("Assumed missval is:", missVal))
    }else {
      warning(paste("Assuming of missval failed. Set missval to NA."))
      hist(temp.data,main="Histogram of values in dataset")
      missVal = NA
    }
    
  }else{
    if(class(missVal)=="numeric"&&length(missVal)==1) print(paste("missval",missVal,"specified by user"))
    else stop(paste("missVal no proper specified by user:",missVal,". missVal should be numeric and of length 1"))
  }
  
  if (!is.na(missVal)){
    if (missVal>0) temp.data[temp.data>=missVal]<-NA
    else if (missVal<0) temp.data[temp.data<=missVal]<-NA
    else stop(paste("Unknown problem with missVal:",missVal))
  }
  
  if(plot.hist) hist(temp.data,main="Histogram of values in dataset")
  
  
  
  ##convert dates for yearly and monthly data
  # get informations about "time"-variable
  timevar<-as.numeric(find.var(temp.nc,"time")[2:3])
  unit.time<-temp.nc$var[[timevar[1]]]$dim[[timevar[2]]]$units
  diff.time<-max(diff(temp.nc$var[[timevar[1]]]$dim[[timevar[2]]]$vals))
  #diff.time<-temp.nc$var[[timevar[1]]]$dim[[timevar[2]]]$vals[[2]]-temp.nc$var[[timevar[1]]]$dim[[timevar[2]]]$vals[[1]]
  
  
  if(unit.time=="day as %Y%m%d.%f"){
    if(diff.time==100){
      year <- floor(temp.time/10000)
      temp.date <- year + (floor((temp.time-(year*10000))/100)-1)/12
    }else{
      if(diff.time==10000){
        temp.date<-temp.time%/%10000
      }else{
        if(min(diff(temp.nc$var[[timevar[1]]]$dim[[timevar[2]]]$vals))==1){
          d.year<-floor(temp.time/10000)
          
          reftime<-julday.own(floor(temp.time[1]/10000)*10000+101)
          d.day<-julday.own(temp.time)-reftime
          len<-length(temp.date)
          d.day[d.day>(len-1)]<-d.day[d.day>(len-1)]-len
          temp.date<-d.year+d.day/365
          
        }else{stop("time steps are not daily, monthly or yearly")}
      }}
  }else{
    if(unit.time=="hours since 1-1-1 00:00:0.0"|unit.time=="hours since 1-01-01 00:00"|unit.time=="hours since 1-01-01 00:00:00"){
      if (diff.time==24){
        temp.date<-(chron(temp.time/24,origin=c(month=1,day=1,year=01)))
        d.year<-as.numeric(as.character(years(temp.date)))
        d.day<-as.numeric(temp.date-chron(paste("1/1/",years(temp.date),sep="")))
        temp.date<-d.year+d.day/365
        
      }else{
        temp.date <- as.vector(as.yearmon(chron(temp.time/24,origin=c(month=1,day=1,year=01))))
      }
    }else{
      if(length(grep(glob2rx("hours since ????-??-?? ??:??:??"),unit.time))){
        start.year<-as.numeric(sub("-..-.....:..:..","",sub("hours since ","",unit.time)))
        start.mon<-as.numeric(sub("-.....:..:..","",sub("hours since ....-","",unit.time)))
        start.day<-as.numeric(sub("...:..:..","",sub("hours since ....-..-","",unit.time)))
        abs.start.day<-julday(start.mon,start.day,2001)-julday(1,1,2001)
        
        temp.time<-temp.time/24
        if (is.null(cal)) cal<-check.cal(temp.time,start.year,start.mon,start.day)
        if(cal=="standard"){
          temp.date<-caldat(julday(start.mon,start.day,start.year)+temp.time)$year
        }else{
          if (cal!="365days") warning("calendar not known. Assume 365days. Check if years are correct!")
          d.day<-(temp.time+abs.start.day)/365
          temp.date<-floor(start.year+d.day)
        }
        print("should only work for yearly data")
      }else{
        if(length(grep(glob2rx("days since ????-??-?? ??:??"),unit.time))){
          start.year<-as.numeric(sub("-..-.....:..","",sub("days since ","",unit.time)))
          start.mon<-as.numeric(sub("-.....:..","",sub("days since ....-","",unit.time)))
          start.day<-as.numeric(sub("...:..","",sub("days since ....-..-","",unit.time)))
          abs.start.day<-julday(start.mon,start.day,2001)-julday(1,1,2001)
          
          if (is.null(cal)) cal<-check.cal(temp.time,start.year,start.mon,start.day)
          if(cal=="standard"){
            temp.date<-caldat(julday(start.mon,start.day,start.year)+temp.time)$year
          }else{
            if (cal!="365days") warning("calendar not known. Assume 365days. Check if years are correct!")
            d.day<-(temp.time+abs.start.day)/365
            temp.date<-floor(start.year+d.day)
          }
          print("should only work for yearly data")
        }else{
          if(length(grep(glob2rx("days since ????-??-?? ??:??:??"),unit.time))){
            start.year<-as.numeric(sub("-..-.....:..:..","",sub("days since ","",unit.time)))
            start.mon<-as.numeric(sub("-.....:..:..","",sub("days since ....-","",unit.time)))
            start.day<-as.numeric(sub("...:..:..","",sub("days since ....-..-","",unit.time)))
            abs.start.day<-julday(start.mon,start.day,2001)-julday(1,1,2001)
            
            if (is.null(cal)) cal<-check.cal(temp.time,start.year,start.mon,start.day)
            if(cal=="standard"){
              temp.date<-caldat(julday(start.mon,start.day,start.year)+temp.time)$year
            }else{
              if (cal!="365days") warning("calendar not known. Assume 365days. Check if years are correct!")
              d.day<-(temp.time+abs.start.day)/365
              temp.date<-floor(start.year+d.day)
            }
            print("should only work for yearly data")
          }else{
            if(length(grep(glob2rx("months since ????-??-?? ??:??:??"),unit.time))){
              start.year<-as.numeric(sub("-..-.....:..:..","",sub("months since ","",unit.time)))
              start.mon<-as.numeric(sub("-.....:..:..","",sub("months since ....-","",unit.time)))
              start.day<-as.numeric(sub("...:..:..","",sub("months since ....-..-","",unit.time)))
              
              d.mon<-(temp.time+start.mon)/12
              temp.date<-floor(start.year+d.mon-0.0001)
              print("should only work for yearly data")
            }else{stop(paste("time format",unit.time,"not supported by read_data"))}
          }}}}}
  
  
  #Sort the latitudes
  tmp<-sort(temp.lat,index.return=TRUE)
  temp.lat<-temp.lat[tmp$ix]
  temp.data<-temp.data[,tmp$ix,]
  
  #sort the longitudes
  temp.lon[temp.lon<0]<-temp.lon[temp.lon<0]+360
  tmp<-sort(temp.lon,index.return=TRUE)
  temp.lon<-temp.lon[tmp$ix]
  temp.data<-temp.data[tmp$ix,,]
  
  
  return(pField(temp.data,temp.date,lat=temp.lat,lon=temp.lon,name=name,history=FILENAME))
}


check.cal<-function(t.time,year,mon,day){
    if (sum(diff(t.time)/365==1)==length(diff(t.time)))
        cal<-"365days"
    else if (sum(diff(t.time)/365==1)+sum(diff(t.time)/366==1)==length(diff(t.time)))
        cal<-"standard"
    else
        cal<-"unknown"
    return(cal)
}

find.var<-function(data.nc,searched_vars)
{
	for (i in 1:length(data.nc$var))
	{
		for (j in 1:length(data.nc$var[[i]]$dim))
		{
			if(is.element(data.nc$var[[i]]$dim[[j]]$name,searched_vars)){
				varname<-data.nc$var[[i]]$dim[[j]]$name
				return(c(varname,i,j))
			}
		}
	}	
}


julday.own<-function(x){
	year<-floor(x/10000)	
	month<-floor((x-year*10000)/100)
	day<-x-(year*10000+month*100)

	return(julday(month,day,year))
}


read.field <- function(x,season){   # 11.11.2013
  # Function based on db.field
  # Reads in a field with name as specified in db.field
  dataset <- subset(db.field,db.field$name.field==x)
  if (dim(dataset)[1]!=1) stop(paste(x,"does not exist in db.field"))
  
  seasons.db <- c("annual", "DJF", "MAM", "JJA", "SON", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", " Dec", "Apr2Aug")
  seasons.file <- c("annual", "DJF", "MAM", "JJA", "SON", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "AAT")
  season.db <- seasons.db[which(seasons.file==season)]
  if (!eval(parse(text = paste("dataset$",season.db,sep="")))) stop(paste("Season",season,"does not exist for data set"))
  
  if (dataset$cal=="") field.cal = NULL else field.cal = dataset$cal
  
  data.path <- paste(data.dir, dataset$path, dataset$prefix, ".", season, ".nc",sep="")
  field <- read_data(data.path, cal = field.cal, varname = dataset$varname, missVal = dataset$miss.val, name = paste(dataset$name,season.db))
  return(field)
}



####################


cor.pTs<-function(ts,field,use="pairwise.complete.obs",min.obs=30,debug=F)
{
        #bring both on the same time basis
        start<-max(start(ts)[1],start(field)[1])
        end<-min(end(ts)[1],end(field)[1])
        if (debug) print(paste("Common time period: ",start,end))
        ts<-window(ts,start,end)
        field<-window(field,start,end)

        if (class(field)[1]=="pField") #field correlation
        {
		    n.Time<-length(time(field))
		    class(field)<-"matrix"
		    index<-((n.Time-colSums(is.na(field)))>min.obs)
		    dat<-field[,index]
                result<-matrix(NA,1,ncol(field))
        
                tresult<-cor(dat,ts,use=use)
                result[,index]<-tresult
                class(field)<-"pField"
                return(copyattr(result,field))
        }
        else return(cor(ts,field,use=use))
}

cortest.pTs<-function(pTs,field,min.obs=30)
{
	#bring both on the same time basis
	start<-max(start(pTs)[1],start(field)[1])
	end<-min(end(pTs)[1],end(field)[1])
	print(paste("Common time period: ",start,end))
	pTs<-window(pTs,start,end)
	field<-window(field,start,end)
	

	#Filter out data which contain not enough timesteps
	n.Time<-length(time(field))
	class(field)<-"matrix"
      index<-((n.Time-colSums(is.na(field)))>min.obs)
      dat<-field[,index]
	result<-matrix(NA,2,ncol(field))

	tresult<-apply(dat,2,mycor.test,c(pTs))
	result[,index]<-tresult

	class(field)<-"pField"
	return(copyattr(result,field))
}



#############  Plotmap Functions

##### only one thing changed
plot.preparation<-function(plotdata,shift=F,long=F,...)
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
            temp$lon<-c(temp$lon[(edgelon+1):nlon]-360,temp$lon[1:edgelon])          #### changed that 300-400 gives now -60 to 40
          }

        if (shift) {
                nlon<-length(temp$lon)
            edgelon<-which(temp$lon>180)[1]
            plotdata<-rbind(plotdata[(edgelon-1):edgelon,],plotdata[(edgelon+1):nlon,],plotdata[1:edgelon,])
            temp$lon<-c(temp$lon[(edgelon-1):edgelon]-360,temp$lon[(edgelon+1):nlon]-360,temp$lon[1:edgelon])
        }


        if (long)
        {
                nlon<-length(temp$lon)
            edgelon<-which(temp$lon>180)[1]
            plotdata<-rbind(plotdata[1:nlon,],plotdata[1:edgelon,])
            temp$lon<-c(temp$lon[1:nlon]-360,temp$lon[1:edgelon])


        }

        if (temp$lat[2] < temp$lat[1])    #if the latitudes are from + to -, reverse them
         {
          temp$lat<-rev(temp$lat)
          plotdata<-plotdata[,rev(seq(len=ncol(plotdata)))]
        }
        return(list(data=plotdata,lat=temp$lat,lon=temp$lon))
}

plotmap.pField<-function(plotdata,main=NULL,zlim=range(plotdata,finite=TRUE),levels=pretty(zlim,nlevels),nlevels=20,palette=NULL,FUN=NULL,shift=F,long=F,xlim=NULL,ylim=NULL,stype=0,sSub="",set.bg=NULL, ...)
{
        temp<-attributes(plotdata)



        if (is.null(sSub)) if (time(plotdata) != 9999) sSub<-paste("time:",format(time(plotdata)))
        if (is.null(main)) main<-temp$name
        #gridcolor="lightgray"
        gridcolor <- "black"

        if (stype == 1) {
                shift=T
                xlim=c(-180,180)
                if (is.null(palette)) {
                                palette=colorRampPalette(c("violetred4","blue","steelblue", "lightgreen","white", "yellow","orange","red","brown"))
                                gridcolor="black"
                                }

                        }

        if (stype == 2) {

                if (is.null(palette)) {
                                palette=colorRampPalette(c("violetred4","blue","steelblue", "lightgreen","white", "yellow","orange","red","brown"))
                                gridcolor="black"
                                }

                        }
        if (stype == 3) {

                if (is.null(palette)) {
                              palette=colorRampPalette(c("darkblue","blue","steelblue", "green","white", "yellow","orange","red","brown"))
                              gridcolor="black"
					print("stype = 3")
                                }

                        }

	  if (stype == 4) {

                if (is.null(palette)) {
                              palette=colorRampPalette(c("darkblue","blue","steelblue", "green","white", "yellow","orange","red","brown"))
                              gridcolor="black"
                                }

                        }


	  gridcolor="black"
        if (is.null(palette)) { palette=rbow;}

        tmp<-plot.preparation(plotdata,shift,long)
        if (is.null(xlim))  xlim = range(tmp$lon, finite = TRUE)
        if (is.null(ylim))  ylim = range(tmp$lat, finite = TRUE)

        if (stype == 1)
        {
        filled.contour.own(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,xlim=xlim,ylim=ylim,color=palette,set.bg=set.bg,plot.title={
        title(main=main,sub=sSub);
        addland(col="black");
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)

        },plot.axes=axes.stype(gridcolor,tmp$lat,tmp$lon))
      } else
       if (stype == 3)
        {
        filled.contour.own(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,xlim=xlim,ylim=ylim,color=palette,set.bg=set.bg,plot.title={
        title(main=main,sub=sSub);
        addland(col="black");grid(nx=4,col=gridcolor)
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)

        },plot.axes=axes.stype3(gridcolor,tmp$lat,tmp$lon))
	} else
       if (stype == 4)
        {
        filled.contour.own(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,xlim=xlim,ylim=ylim,color=palette,set.bg=set.bg,plot.title={
        title(main=main,sub=sSub);
        addland(col="black");grid(col=gridcolor)
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)

        },plot.axes=axes.stype4(gridcolor,tmp$lat,tmp$lon))
      } else
        filled.contour.own(tmp$lon,tmp$lat,tmp$data,zlim=zlim,nlevels=nlevels,levels=levels,xlim=xlim,ylim=ylim,color=palette,set.bg=set.bg,plot.title={
        title(main=main,sub=sSub);
        addland(col="black"); grid()
        if (!is.null(FUN)) FUN(tmp$lon,tmp$lat,tmp$data)

       }, ...)
}

axes.stype4<-function(gridcolor,lat,lon)
{
	at.lat<-pretty(lat)
	at.lon<-pretty(lon)


	labels.lat<-paste(at.lat)

	at.lon.tmp<-at.lon
	at.lon.tmp[at.lon.tmp>180]<-at.lon.tmp[at.lon.tmp>180]-360
	#labels.lon[at.lon.tmp>0]<-paste(at.lon.tmp[at.lon.tmp>0],"E",sep="")
	#labels.lon[at.lon.tmp<0]<-paste(-at.lon.tmp[at.lon.tmp<0],"W",sep="")
	labels.lon<-paste(at.lon.tmp)


      Axis(lon, side = 1,at=at.lon,labels=labels.lon)
      Axis(lat, side = 2,at=at.lat,labels=labels.lat)

}

axes.stype3<-function(gridcolor,lat,lon)
{
	labels.lat<-c("0","20","40","60","80")
	#labels.lat<-c("EQ","20N","40N","60N","80N")
	at.lat<-c(0,20,40,60,80)

	labels.lon<-c("-100","-50","0","50","100")
	#labels.lon<-c("100","50W","0","50E","100")
	at.lon=c(260,310,360,410,460)

#title(main = "", xlab = "", ylab = "")
             Axis(lat, side = 1,at=at.lon,labels=labels.lon)
             Axis(lon, side = 2,at=at.lat,labels=labels.lat)
#abline(h=at.lat,col = gridcolor, lty = "dotted")
#abline(v=at.lon,col = gridcolor, lty = "dotted")

}






filled.contour.own<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels),
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) -
        1), plot.title, plot.axes, key.title, key.axes, asp = NA,
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, set.bg = NULL,
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
                axis(4,labels=levels,at=breaks)
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

    if(!is.null(set.bg)){
        usr<-par('usr')
        rect(usr[1],usr[3],usr[2],usr[4],col=set.bg)
    }

    .filled.contour(as.double(x), as.double(y), z, as.double(levels),
        col = col)
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






##########  Plot with highlighting of significant areas

plot.Polygon<-function(sigline)
{
   col="grey60"
   for (i in 1:length(sigline)) polygon(sigline[[i]]$x,sigline[[i]]$y,angle=-30,density=5,col=col, border = col)
}



corcontour<-function(lon,lat,data)
  {

    levelsp=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
    levelsm=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9)


    contour(lon,lat,data,col="grey",lty=6,lwd=1,levels=levelsp,add=TRUE,drawlabels=TRUE,labcex=1)
    contour(lon,lat,data,col="grey",lty=1,lwd=1,levels=levelsm,add=TRUE,drawlabels=TRUE,labcex=1)
    contour(lon,lat,data,col="grey",lty=1,lwd=2,levels=c(0),add=TRUE,drawlabels=TRUE,labcex=1)

    #contour(lon,lat,data,col="grey",lty=2,lwd=1,levels=levelsp,add=TRUE,drawlabels=TRUE,labcex=1)
    #contour(lon,lat,data,nlevels=10,add=TRUE,drawlabels=FALSE,col="black",lwd=2)
  }

sigline.preparation<-function(sigmap,levels,...)
{
	temp<-plot.preparation(sigmap,...)

	diff.lon<-max(diff(temp$lon))
	diff.lat<-max(diff(temp$lat))
	len.lon<-length(temp$lon)
	len.lat<-length(temp$lat)

	new.lon<-c(temp$lon[1]-diff.lon,temp$lon,temp$lon[len.lon]+diff.lon)
	new.lat<-c(temp$lat[1]-diff.lat,temp$lat,temp$lat[len.lat]+diff.lat)

	empty.row<-matrix(1,ncol=len.lat)   	## wenn 1, werden sign. Gebiete schraffiert. Wenn 0, werden nicht-signifikante Gebiete schraffiert
	empty.col<-matrix(1,nrow=len.lon+2)


	new.data<-rbind(empty.row,temp$data)
	new.data<-rbind(new.data,empty.row)
	new.data<-cbind(empty.col,new.data)
	new.data<-cbind(new.data,empty.col)


	new.data[is.na(new.data)]<-1

	return(contourLines(new.lon,new.lat,1-new.data,levels=levels))

}



plot.sig<-function(plotmap,sigmap,p_val=0.05,plot_sig,FUN=NULL,...)
{
    if (plot_sig=="siglines"){
        siglines<-sigline.preparation(sigmap,levels=1-p_val,...)
        if (length(siglines) == 0)
            if(!is.null(FUN)) eval(parse(text= paste("plot(plotmap,FUN=",FUN,",...)",sep='')))
            else              plot(plotmap,...)
        else
             if(!is.null(FUN)) eval(parse(text= paste("plot(plotmap,FUN={plot.Polygon(siglines);",FUN,"},...)",sep='')))
             else              plot(plotmap,FUN=plot.Polygon(siglines),...)
    }
    else if (plot_sig=="contour"){
		sigfield<-plotmap
		sigfield[sigmap>p_val]<-NA
		temp<-plot.preparation(plotmap,...)
		if(!is.null(FUN))  eval(parse(text= paste("plot(sigfield,FUN={corcontour(temp$lon,temp$lat,temp$data);",FUN,"},...)",sep='')))
		else              plot(sigfield,FUN=corcontour(temp$lon,temp$lat,temp$data),...)

	#	plotmap.pFieldb (sigfield,plotmap,FUN=points(365,47,pch=21,cex=1,col="black",bg="red",lwd=2),set.bg="grey90",...)
    }
    else stop("plot_sig definition wrong")

}




###### filtering of pField

filter.pField<-function(field,Filter,...)
{
    if (sum(is.na(field))) warning(paste(sum(is.na(field)))," NA-Values in pField!")
	result<-apply(field,2,"filter.pTs1",Filter,...)
	newField<-pField(NULL,time(field),getlat(field),getlon(field),paste(getname(field)),gethistory(field),date=FALSE)
	for (i in 1:length(time(field))) newField[i,]<-result[i,]
	return(newField)
}












############################ Running correlation or the like

#concept for running calculations
# executes a function in a running/rolling window

#input: ts1,
roll.1ts <- function(ts1,width,FUN,by=1,name=NULL,detrend=TRUE,scale=TRUE,... )
{
	nTimeGuess<-(end(ts1)[1]-start(ts1)[1]) / by
	StartTime<-(0:nTimeGuess)*by+start(ts1)[1]
	StartTime<-StartTime[StartTime<=end(ts1)[1]-width]
	nTime<-length(StartTime)

	newName<-paste(name,getname(ts1))
	result<-pTs(NULL,StartTime+(width/2),9999,9999,newName,gethistory(ts1),date=FALSE)

	for (i in 1:nTime)
	{
		data<-window(ts1,StartTime[i],StartTime[i]+width)
		if (detrend) data<-detrend(data)
		if (scale) data<-scale(data)
		result[i]<-FUN(data, ...)
	}

	return(addhistory(result,newName))
}

roll.1field<-function(field,width,FUN,by=1,name=NULL,detrend=TRUE,scale=TRUE,... )
{
	nTimeGuess<-(end(field)[1]-start(field)[1]) / by
	StartTime<-(0:nTimeGuess)*by+start(field)[1]
	StartTime<-StartTime[StartTime<=end(field)[1]-width]
	nTime<-length(StartTime)

	newName<-paste(name,getname(field))
      result<-pField(NULL,StartTime+(width/2),attr(field,"lat"),attr(field,"lon"),newName,gethistory(field),date=FALSE)

	for (i in 1:nTime)
	{
		data<-window(field,StartTime[i],StartTime[i]+width)
		if (detrend) data<-detrend(data)
		if (scale) data<-scale(data)
		result[i,]<-apply(data,2,FUN,...)
	}

	return(addhistory(result,newName))
}


roll.1<-function(data,width,FUN,by=1,name=NULL,detrend=FALSE,scale=FALSE,... )
{
	if(class(data)[1]=="pTs"){
		roll.1ts(data,width,FUN,by=by,name=NULL,detrend=detrend,scale=scale,...)
	}else{
	if(class(data)[1]=="pField"){
		roll.1field(data,width,FUN,by=by,name=NULL,detrend=detrend,scale=scale,...)
	}else{
		stop("wrong data, pTs or pField needed")
	}}
}


roll.2ts <- function(ts1,ts2,width,FUN,...,by=1,name=NULL,detrend=FALSE,scale=FALSE)
{
	#bring on same timebase
	start<-max(start(ts1)[1],start(ts2)[1])
	end<-min(end(ts1)[1],end(ts2)[1])
	#print(paste("Common time period: ",start,end))
	ts1<-window(ts1,start,end)
	ts2<-window(ts2,start,end)

	nTimeGuess<-(end-start) / by
	StartTime<-(0:nTimeGuess)*by+start
	StartTime<-StartTime[StartTime<=end-width]
	nTime<-length(StartTime)

	newName<-paste(name,getname(ts1),getname(ts2))
	result<-pTs(NULL,StartTime+(width/2),9999,9999,newName,gethistory(ts1),date=FALSE)


	for (i in 1:nTime)
	{
		data1<-window(ts1,StartTime[i],StartTime[i]+width)
		data2<-window(ts2,StartTime[i],StartTime[i]+width)
		if (detrend) {data1<-detrend(data1);data2<-detrend(data2)}
		if (scale) {data1<-scale(data1);data2<-scale(data2)}
		result[i]<-FUN(data1,data2, ...)
	}


	return(addhistory(result,newName))
}


roll.2field<-function(field,ts,width,FUN,...,by=1,name=NULL,detrend=FALSE,scale=FALSE)
{
        #bring on same timebase
        start<-max(start(field)[1],start(ts)[1])
        end<-min(end(field)[1],end(ts)[1])
        #print(paste("Common time period: ",start,end))
        field<-window(field,start,end)
        ts<-window(ts,start,end)

        nTimeGuess<-(end-start) / by
        StartTime<-(0:nTimeGuess)*by+start
        StartTime<-StartTime[StartTime<=end-width]
        nTime<-length(StartTime)

        newName<-paste(name,getname(field),getname(ts))
        result<-pField(NULL,StartTime+(width/2),attr(field,"lat"),attr(field,"lon"),newName,gethistory(field),date=FALSE)


        for (i in 1:nTime)
        {
                data1<-window(field,StartTime[i],StartTime[i]+width)
                data2<-window(ts,StartTime[i],StartTime[i]+width)
                if (detrend) {data1<-detrend(data1);data2<-detrend(data2)}
                if (scale) {data1<-scale(data1);data2<-scale(data2)}
                result[i,]<-FUN(data2,data1, ...)
        }


        return(addhistory(result,newName))
}

roll.2<-function(data1,data2,width,FUN,...,by=1,name=NULL,detrend=FALSE,scale=FALSE){
	if (class(data1)[1]=="pTs"&class(data2)[1]=="pTs"){
		roll.2ts(ts1=data1,ts2=data2,width,FUN,...,by=by,name=NULL,detrend=detrend,scale=scale)
	}else{
	if (class(data1)[1]=="pField"&class(data2)[1]=="pTs"){
		roll.2field(field=data1,ts=data2,width,FUN,...,by=by,name=NULL,detrend=detrend,scale=scale)
	}else{
	if (class(data1)[1]=="pTs"&class(data2)[1]=="pField"){
		roll.2field(field=data2,ts=data1,width,FUN,...,by=by,name=NULL,detrend=detrend,scale=scale)
	}else{
		stop("wrong arguments")
	}}}
}


roll.plot<-function(field,width=0,...){

	for (i in 1:length(time(field))){
		newFig()
		title<-paste("timewindow from",time(field[i,])[1]-width/2,"to",time(field[i,])[1]+width/2)
		plot(field[i,],main=title,...)
	}
}



roll.movie<-function(field,path="F:/weinschmeckt/Ergebnisse/movie/",prefix="test",width=0,...){
	j<-100
	for (i in 1:length(time(field))){
		j<-j+1
		title<-paste("timewindow from",time(field[i,])[1]-width/2,"to",time(field[i,])[1]+width/2)
 		jpeg(paste(path,prefix,j,".jpg",sep=""),width=600,height=600,quality=90)
		plot(field[i,],main=title,...)
		dev.off()
	}
}







## Statistical test if the changes of the correlation values over time are significant ( Sterl et al 2007)
## the observed difference between the maximum and minimum correlation is compared with the distribution
## of correlation differences obtained with surrogate time-series
runcortest<-function(ts1,ts2,width=30,N.R=1000,p=0.05,detrend=FALSE,bootstrap=FALSE,fisher=FALSE,filt=NULL)
## filt=lowpass(1/10,21)
{
#bring both on the same time basis
    start<-max(start(ts1)[1],start(ts2)[1])
    end<-min(end(ts1)[1],end(ts2)[1])
    ts1<-window(ts1,start,end)
    ts2<-window(ts2,start,end)

#normieren
	ts1<-scale(ts1)
	ts2<-scale(ts2)

#globaler Regressionskoeffizient ; enspricht auch Korrelation
  lmresult<-lm(ts2~ts1)
  slope<-lmresult$coeff[2]
  residuals<-lmresult$residuals

#running c
  runcor<-roll.2(ts1,ts2,FUN=cor.pTs,width=width,detrend=detrend,scale=FALSE)
  if (fisher) cordiff<-diff(fisher(range(runcor))) else cordiff<-diff(range(runcor))

  cordiff_sim<-vector()
  j=0
  for (i in 1:N.R)
  {
     if (bootstrap){
        residuals_sim<-sample(c(residuals),length(residuals),replace=TRUE)
        if (!is.null(filt)) stop("bootstrap with filtered data not possible")
     }
     else {
        residuals_sim<-rnorm(length(residuals),sd=sd(residuals))   # filter drum rum setzen, bootstrap funktioniert nicht !!
        if (!is.null(filt)) residuals_sim<-filter.pTs1(residuals_sim,filt,method=2)
     }
     ts2_sim <- ts1*slope+residuals_sim
     runcor_sim <- roll.2(ts1,ts2_sim,FUN=cor.pTs,width=width,detrend=detrend,scale=FALSE)

  if (fisher)    cordiff_sim[i]<-diff(fisher(range(runcor_sim))) else cordiff_sim[i]<-diff(range(runcor_sim))

  j=(j+1)
  if  (j>=0.05*N.R)
    {
      j=0
      print(paste(i*100/N.R,"%  von",N.R))
    }
  }

  refdiff<-quantile(cordiff_sim,1-p)
  print(paste("maximal zuf?llige Korrelationsdifferenz (95% )",refdiff))
  print(paste("reale Korrelationsdifferenz betr?gt ",cordiff))
  if (refdiff > cordiff) print("Das Ergebnis ist nicht signifikant") else print("Das Ergebnis ist signifikant")
  print(paste("Korrelationsdifferenz wurde in",sum(cordiff_sim>cordiff)/N.R*100,"% der F?lle durch Zufall ?berschritten"))
  return(list(cordiff_sim=cordiff_sim,refdiff=refdiff,cordiff=cordiff))
}

fisher<-function(c) return(0.5*log((1+c)/(1-c)))





pvalFilter.cor<-function(N,filt,pval=0.05,one_sided=FALSE,N.R=5000)
{
	savecor<-vector()
	for (i in 1:N.R)
	{
		ts1<-rnorm(N)
		ts2<-rnorm(N)
		ts1.f<-filter.pTs1(ts1,filt,method=2)
		ts2.f<-filter.pTs1(ts2,filt,method=2)
		savecor[i]<-cor(c(ts1.f),c(ts2.f))
	}
  if (one_sided) sig_interval <- 1-pval else sig_interval=1-pval/2
  return(quantile(savecor,sig_interval))
}


cor.filter<-function(ts,field,filt,plot_sig=NULL,min.obs=50,use="pairwise.complete.obs")
#### filt muss die Form haben: filt<-list(filter="lowpass",x1=1/10,y=21,method=2), for bandpass: filt<-list(filter="bandpass",x1=1/10,x2=1/5,y=21,method=2)
{
    if (sum(is.na(ts))) stop("no NA-values allowed")
    # filter
    if (filt$filter=="bandpass"){
      if (is.null(filt$x2)) stop("Lower limit for bandpass filter needed")
      lp<-get(filt$filter)(filt$x1,filt$x2,filt$y)
    } else lp<-get(filt$filter)(filt$x1,filt$y)
    field<-filter.pField(field,lp,method=filt$method)
    ts<-filter.pTs1(ts,lp,method=filt$method)
    #ts<-ts[!is.na(ts)]


    #bring both on the same time basis
	start<-max(start(ts)[1],start(field)[1])
	end<-min(end(ts)[1],end(field)[1])
	print(paste("Common time period: ",start,end))
	ts<-window(ts,start,end)
	field<-window(field,start,end)


	n.Time<-length(time(field))
	class(field)<-"matrix"
    index<-((n.Time-colSums(is.na(field)))>min.obs)
    dat<-field[,index]
    tresult<-cor(dat,ts,use=use)
    result<-matrix(NA,1,ncol(field))
    result[,index]<-tresult

  if (!is.null(plot_sig))
    {
      result<-rbind(result,matrix(1,1,ncol(field)))
      sig_val<-pvalFilter.cor(n.Time,lp)
      result[2,][abs(result[1,])>sig_val]<-0

    }
  class(field)<-"pField"
  return(copyattr(result,field))


}



### Partial Correlation

part.cor<-function(ts1,ts2,ts3,...)
{
     c.12<-cor(ts1,ts2,...)
     c.13<-cor(ts1,ts3,...)
     c.23<-cor(ts2,ts3,...)
    return((c.12-c.13*c.23)/(sqrt(1-(c.13)^2)*sqrt(1-(c.23)^2)))
}


partcor.pTs<-function(ts1,field,ts2,use="complete.obs",min.obs=30,debug=F)
{
        #bring both on the same time basis
        start<-max(start(ts1)[1],start(field)[1],start(ts2)[1])
        end<-min(end(ts1)[1],end(field)[1],end(ts2)[1])
        if (debug) print(paste("Common time period: ",start,end))
        ts1<-window(ts1,start,end)
        field<-window(field,start,end)
        ts2<-window(ts2,start,end)

        if (class(field)[1]=="pField") #field correlation
        {
                    n.Time<-length(time(field))
                    class(field)<-"matrix"
                    index<-((n.Time-colSums(is.na(field)))>min.obs)
                    dat<-field[,index]
                result<-matrix(NA,1,ncol(field))

                tresult<-part.cor(dat,ts1,ts2,use=use)
                result[,index]<-tresult
                class(field)<-"pField"
                result<-copyattr(result,field)
        }
        else return(part.cor(ts1,field,ts2,use=use))
}


### Create local index of field

local.index <- function(ts1, field) {
  lat <- attr(ts1,"lat")
  lon <- attr(ts1, "lon")
  if (lon==0) lon=0.5
  if (lon<0) lon=lon+360
  print(attr(ts1,"name"))
  print(paste("lat:",lat,"lon:",lon))
  return(selspace(field,lat1=lat,lon1=lon))
}




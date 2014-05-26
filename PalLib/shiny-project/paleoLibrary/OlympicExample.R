#Hier der Code + Kommentare 


#cdo preparation to extract seasons from monthly files:
#cdo yearmean selseas,SEASON input output

#the climate data can be found under: paleo5/data/gridded/monthly/....

#Loading the paleolibrary
path<-"c:/data/paleoLibrary/"
source("c:/data/paleoLibrary/header.R")
#The first time some libraries may be missing, you have to install them


#Read the ASCII File
olympic<-read.table("c:/data/paleoLibrary/olympic.txt",skip=1)


#Read the Hadley annual mean SST dataset
hadsst<-read.had.annual("c:/data/paleoLibrary/hadsst.t63.nc",varname="temp",latname="lat",lonname="lon")

hadsst[hadsst<0]<-NA #Change sea ice = negative values to missing values


#Linear interpolation to the time of hadsst
olympic.int<-approx(olympic[,1],olympic[,2],time(hadsst))

#Put them in the pTs framework
olympic<-pTs(olympic.int$y,olympic.int$x,0,0,"Romanian sportsmen in the Olympics Games")

#Omit the times where we have no data
olympic<-na.omit(olympic)


#Example how to extract a box
box<-selspace(hadsst,lat1=40,lat2=50,lon1=200,lon2=210)

#Take the mean over the box to derive an index
box.ts<-applyspace(box,mean)

#Take the mean over the time to get the climatological field over the box
plot(applytime(box,mean))

#Correlate the Olympic data with the Hadley SST
corresult<-cor.pTs(olympic,hadsst)

plot(corresult)

#Create an NA Index
NAtlantic<-applyspace(selspace(hadsst,lat1=40,lat2=60,lon1=-40,lon2=0),mean)
plot(NAtlantic)

#Extract 1990-2000 from the NAtlantic timeseries
window(NAtlantic,1990,2000)

#Put a 4 year timelag in the olympic timeseries
olympic.lag<-lag(olympic,4)

cormap2<-cor.pTs(olympic.lag,hadsst)

plot(cormap2,stype=1,main="",zlim=c(-0.8,0.9))

# load all needed packages
library(chron) #includes some time handling functions
library(clim.pact)  #includes the coastline drawing function
library(debug) #allows to debug the code
library(zoo)   #some timeseries function
library(ncdf)  #netcdf handling

#load all custom libraries
path<-"c:/data/paleoLibrary/"   #here supply the directory where the libraries
                         #are located

source(paste(path,'basis_cold.R',sep=""))
source(paste(path,'proxytype.R',sep=""))        #type definition
source(paste(path,'selspace.pField.R',sep=""))  #selection of areas and points
source(paste(path,'mat.pField.R',sep=""))       #EOF's, correlation etc...

source(paste(path,'plotlib.R',sep=""))       #plotting routines
source(paste(path,'read_routines.R',sep="")) #reading routines
source(paste(path,'indices.R',sep=""))       #standard climate indices
source(paste(path,'roll.pField.R',sep=""))   #running/rolling functions
source(paste(path,'noise.pField.R',sep=""))  #surrogate timeseries for MonteCarlo
source(paste(path,'filter.R',sep=""))        #lowpass and bandpass filter caclulation
source(paste(path,'ownfunctions.R',sep=""))                #put your own functions / 
							                  #changed functions here





#load ncep gph500 DJF data (prepared by CDO because its faster)
gph.ncep<-read_ncep(FILENAME="C:/data/paleoLibrary/exampledata/DJFgph500.nc",varname="hgt")
plot(applytime(gph.ncep,mean))  #plot time mean of gph data

#Read the accumulation datasets
drive<-"c:"
a.a<-read.accum(paste(drive,"/data/norel/cA.txt",sep=""),"accum_A",70.6349,324.18001)
a.b<-read.accum(paste(drive,"/data/norel/cB.txt",sep=""),"accum_B",70.65081,322.52121)
a.d<-read.accum(paste(drive,"/data/norel/cD.txt",sep=""),"accum_D",70.63980,320.38222)
a.e<-read.accum(paste(drive,"/data/norel/cE.txt",sep=""),"accum_E",71.75926,324.14947)
a.g<-read.accum(paste(drive,"/data/norel/cG.txt",sep=""),"accum_G",71.15495,324.16227)

#put them all together in one dataset (multivariate pTs)
a.all<-cbind.pTs(a.a,a.b,a.d,a.e,a.g)
plot(a.all) #plot all datasets

#Now only keep the data which is there all time
a.all<-na.omit(a.all)
plot(a.all)

#See how they correlate with each other
cor(a.all)

#Look how the correlations change if you apply a running mean of 10 years
#The argumentation in the original paper is, that there is high isotopic noise
#on the data and therefore applying the mean shows the physical dependence)
#Core E is the most independend one
cor(rollmean(a.all,10))


#Have a look at the histograms
par(mfcol=c(3,2))  #This command helps to put them all in one plot
hist(a.a)
hist(a.b)
hist(a.d)
hist(a.e)
hist(a.g)


#Principal component analysis... keep 5 PC's
accum.pcomp<-prcompO.pTs(all,nPc=5)
accum.pcomp$var/accum.pcomp$varsum  #Just print the explained variance 

par(mfcol=c(1,1))   #Now switch again to get one plot in one window
plot(accum.pcomp$pc) #Look at the PC's
ploteof(accum.pcomp,all,1) #And the EOF's
ploteof(accum.pcomp,all,2) #And the EOF's
ploteof(accum.pcomp,all,3) #And the EOF's
ploteof(accum.pcomp,all,4) #And the EOF's


tst<-accum.pcomp$eof[,2]+accum.pcomp$eof[,3]+accum.pcomp$eof[,4]
tst1<-accum.pcomp$pc[,2]+accum.pcomp$pc[,3]+accum.pcomp$pc[,4]
 

o<-accum.pcomp
o$eof[,1]<-tst
ploteof(o,all,1)

c<-sigcor(detrend(tst1),detrend(gph.ncep))
plot(c)

gph.day<-read_ncep_day( paste(drive,"/data/norel/gph/gph_all.nc"), "hgt")

gph.year<-read_ncep_yr(paste(drive,"/data/norel/gph_yrmean.nc",sep=""), "hgt")
gph.year.an<-scale(gph.year,scale=FALSE)

ploteof(accum.pcomp,all,2)
ploteof(accum.pcomp,all,3)

pc1<-accum.pcomp$pc[,1]


#read accum data from a greenland core
coredata<-read.accum("C:/data/paleoLibrary/exampledata/coreA.txt","accum A",70.6349,324.18001)
plot(coredata)

#plot a surrogate dataset
plot(enoise.pTs(coredata),main="Surrogate data with the same distribution")

#plot only 20yrs of accum data
plot(window(coredata,1950,1970)) #plot only 1950-1970

#composite 
comp.result<-composite.pTs(coredata,slp.ncep)
plot(comp.result$plus)
plot(comp.result$minus)

#plot the local significant correlations
result<-cortest.pTs(coredata,slp.ncep)
sigresult<-result[1,]
sigresult[result[2,]>0.05]<-NA
plot(sigresult)


#plot the correlations
plot(cor.pTs(coredata,slp.ncep.north))

#spectrum 
spectrum(coredata,spans=5)


#filtering in the 3-7yr band
filt.3.7<-calcbpfilt(20,1/3,1/7)

coredata.filtered<-filter.pTs(coredata,filt.3.7)

#apply the filter
spectrum(coredata.filtered,spans=5)

#spectrum again
plot(a)




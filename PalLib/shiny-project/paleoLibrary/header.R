
# load all needed packages
library(chron) #includes some time handling functions
library(clim.pact)  #includes the coastline drawing function
library(debug) #allows to debug the code
library(zoo)   #some timeseries function
library(ncdf)  #netcdf handling


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
source(paste(path,'season.R',sep=""))                #put your own functions / 
source(paste(path,'ownfunctions.R',sep=""))                #put your own functions / 
							                  #changed functions here



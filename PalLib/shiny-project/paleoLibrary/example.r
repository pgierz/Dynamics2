#load all custom libraries
path<-"c:/data/paleoLibrary/"   #here supply the directory where the libraries
                                #are located
source(paste(path,'header.R',sep=""))


#load ncep sea level pressure data
sst.ncep<-read_ncep(FILENAME="C:/data10Feb/NCEP/DJFskt.nc",varname="skt",name="DJF SKT")

sst.pacific<-selspace(sst.ncep,lat1=-30,lat2=30,lon1=100,lon2=280)

sst.atlantic<-selspace(sst.ncep,lat1=-30,lat2=30,lon1=280,lon2=360)


pcresult<-prcompO.pField(sst.pacific) 
plot(pcresult$eof[2,],main="1st EOF SST")
plot(pcresult$pc[,1:3],main="PC's SLP")


nao<-index.nao(slp.ncep,plot=TRUE)  #get the NAO and plot the EOF
plotindex(nao) #plot the NAO index


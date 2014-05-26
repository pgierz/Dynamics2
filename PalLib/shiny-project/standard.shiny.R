## standard executed inputs
# rm(list=ls(all=T))
# 
#
# You have to set data.dir!!

data.dir<-"../data/"


path <- paste(getwd(),"/paleoLibrary/",sep="")
source("paleoLibrary/header.R")
source("paleoLibrary/4paleolibrary.R")



##################### Read in db.ts and db.field db.area Data base #################################


##### Load Area Data base
db.area.colClasses<-c("character","character","numeric","numeric","numeric","numeric")
db.area<-read.csv(paste(data.dir,"database-area.csv",sep=""),dec = ",",sep=";",header=T,colClasses=db.area.colClasses)


##### Load Field Data base 
db.field.colClasses<-c("character","factor","character","character","character","numeric","numeric","character","numeric","character","character","character","character","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical","logical")
db.field<-read.csv(paste(data.dir,"database-field.csv",sep=""),dec = ",",sep=";",header=T,colClasses=db.field.colClasses)


##### load TS Database
db.ts.colClasses<-c("character","factor","character","character","numeric","numeric","numeric","numeric","numeric","character","character","character","character")
db.ts<-read.csv(paste(data.dir,"database-ts.csv",sep=""),dec = ",",sep=";",header=T,colClasses=db.ts.colClasses)
## set tabulator seperator to "\t" for read.table
db.ts[which(db.ts$sep=="tab"),11]<-"\t"

### read in all time series from database
for (i in 1:dim(db.ts)[1]){
  
  ts.input <- db.ts[i,]$ts.name
  sub.db <- subset(db.ts,db.ts$ts.name==ts.input)
  temp<-read.table(paste(data.dir,sub.db$path,sep=""), dec=sub.db$dec,sep = sub.db$sep, na.strings=(sub.db$na.string))
  assign(ts.input, pTs(temp[,2], temp[,1], lat=sub.db$lat, lon=sub.db$lon, name=sub.db$name))
}





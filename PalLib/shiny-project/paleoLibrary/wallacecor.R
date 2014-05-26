data<-scale(detrend(sstdata))
datac<-scale(detrend(window(data,1900,2000)))

wcor<-function(datac)
{
n<-ncol(datac)
mincor2000<-rep(0,n)
for (i in (1:n))
{
	mincor2000[i]<-min(cor(datac[,i],datac))
}
return(pField(mincor2000,1,getlat(datac),getlon(datac)))
}


newFig()
plot(pField(mincor,1,getlat(sstdata),getlon(sstdata)))
newFig()
plot(pField(mincor2000,1,getlat(datac),getlon(datac)))


which(mincor2000==min(mincor2000))
newFig()
plot(fastcor.pTs(data[,5200],data))

nindex<-data[,4417]-data[,1535]

6Sekunden/100

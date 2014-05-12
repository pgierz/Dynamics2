
#logistic difference equation, with parameter r, N iterations, and initial value x0
f = function(r,N,x0)
{
  x <- vector()
  x[1]<-x0
  for (i in 2:N)  x[i]<-r*x[i-1]*(1-x[i-1])
  return(x)
}


#determine the values of the local extrema and give them back in a vector
local_extrema <- function(x)
{
	result <- vector()
	for (i in 1:(length(x)-2))
	{
	if ((x[i]<x[i+1]) && (x[i+1] > x[i+2])) result<-c(result,x[i+1]) #save the local maximum in the result vector
      if ((x[i]>x[i+1]) && (x[i+1] < x[i+2])) result<-c(result,x[i+1]) #savel the local minimum in the result vector
	}
	return(result)
}


#test the functions:

#Test the logistic difference equation function
plot(f(3.9,100,0.4),type="l")  

hist(f(3.9,1000,0.4),1000)



#Test the local extrema function
temp<-sin((1:300)/30)
plot(temp)
local_extrema(temp)

#test histo

hist(f(4,2000,0.4)[1000:2000],breaks=50)

#main program

resolution<-400  #number of r-parameter values to be scanned
rlim<-c(1,4)     #minimum and maximum r-value
xlim<-c(0,1)     #minimum and maximum x-value


r<-rlim[1]+(1:resolution)*((rlim[2]-rlim[1])/resolution) #vector of all r-values we will scan

plot(xlim=rlim,ylim=xlim,1,type="n",xlab="r",ylab="x",main="logistic map") #empty plot with axes and title

for (i in 1:resolution)
{
  temp<-f(r[i],300,0.5)[200:300]
  save<-local_extrema(temp)
  points(rep(r[i],length(save)),save,pch=".")
}

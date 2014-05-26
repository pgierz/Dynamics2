#functions to create surrogate ts


#surrogate timeseries with the same timebasis and autocorrelation as the original
#(based on AR1, / white noise)

snoise.pTs <- function(ts)
{
ar_result<-ar(ts,order.max=1,aic=FALSE)
nts<-pTs(scale(arima.sim(list(ar = ar_result$ar),length(ts))),time(ts),getlat(ts),getlon(ts),paste("Surrogate Noise ",getname(ts)))
return(nts)
}	

red<-function(a1,n) return(c(arima.sim(list(ar = a1),n)))

#create noise with the same distribution, by folding a uniform distribution with
#the quantile function
#up to now only for independent samples
#only for univariate ts
enoise.pTs <- function(ts)
{
n<-length(time(ts))
noise_data<-ts
q<-quantile(c(noise_data),probs=(1:1000)/1000)
noise_sim<-q[runif(n)*1000+1]
	#correct the noise to get the same sd
noise_sim<-noise_sim*(sd(noise_data)/sd(noise_sim))
return(pTs(noise_sim,time(ts),name="enoise"))
}
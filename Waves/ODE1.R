#demonstration of Euler forward method in 1st order ODE: dy/dt=A*y


#constants
A<-  -0.5  #growth / decay rate
T<-  50    #integration time in time units
dt<-  .1     #step size in time units
Y0<- 100   #inital value


n<-T/dt          #number of time steps (time / timestep)
t<-(0:(n-1))*dt  #create a vector of discrete timesteps 
y<-vector()     #define an empty vector for the state variable y(t)
y[1]<-Y0        #assign initial value


for (i in 1:(n-1))
{
	y[i+1]<-y[i]+dt*A*y[i]  
}

plot(t,y,type="l") #plot the result against time

#additionaly plot the analytical solution in red
lines(t,Y0*exp(A*t),col="red")

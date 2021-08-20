

###########################################################################
###########################################################################
############## Code to simulate AR(1) and MA(1) time series ###############
###########################################################################
###########################################################################

###### AR(1)
rm(list = ls())
#can also use library(library(ggfortify))
AR_sim <- function(phi,mu,sigma,TT,plot){
  #phi = AR(1) parameter; the autocorrelation basically
  #mu = mean of the series
  #sigma = sd of errors
  #TT = the number of time points

  #make a time vector going from 1 to T for storing the outcomes
  #time <- seq(1:TT)
  
  ##generate the y values from an AR(1) process
  #set up the vector of T values -- initialize at 0 for convenience
  y <- rep(0, TT)
  
  #draw the first time point from a normal distribution
  y[1] <- mu + rnorm(1, 0, sigma)
  
  #create the rest of the outcomes from the AR(1) model
  for(j in 2:TT) {
    y[j] <- mu + phi*(y[j - 1] - mu) + rnorm(1, 0, sigma)
  }
  
  ytimeseries <- ts(y)
  
  if(plot==T){
    par(mfrow=c(3,1))
    plot.ts(ytimeseries,col="blue4",ylab="y")
    acf(y)
    pacf(y)
  } else {
    return(ytimeseries)
  }
  
}

AR_sim(phi=0.3,mu=0,sigma=1,TT=500,plot=T)
AR_sim(phi=0.5,mu=0,sigma=1,TT=500,plot=T)
AR_sim(phi=-0.5,mu=0,sigma=1,TT=500,plot=T)
AR_sim(phi=0.95,mu=0,sigma=1,TT=500,plot=T)
AR_sim(phi=-0.9,mu=0,sigma=1,TT=500,plot=T)
#play around with other values


###### MA(1)
MA_sim <- function(phi,mu,sigma,TT,plot){
  #phi = MA(1) parameter
  #mu = mean of the series
  #sigma = sd of errors
  #TT = the number of time points
  
  #make a time vector going from 1 to T for storing the outcomes
  #time <- seq(1:TT)
  
  ##generate the y values from a MA(2) process
  #set up the vector of T values -- initialize at 0 for convenience
  y <- rep(0, TT)
  
  #draw y for first time point from a normal distribution
  error_lag <- rnorm(1, 0, sigma)
  
  #create the rest of the outcomes from the MA(1) model
  for(j in 1:TT) {
    error <- rnorm(1, 0, sigma)
    y[j] <- mu + phi%*%error_lag + error
    error_lag <- error
  }
  
  ytimeseries <- ts(y)
  
  if(plot==T){
    par(mfrow=c(3,1))
    plot.ts(ytimeseries,col="blue4",ylab="y")
    acf(y)
    pacf(y)
  } else {
    return(ytimeseries)
  }
}

MA_sim(phi=0.3,mu=0,sigma=1,TT=500,plot=T)
MA_sim(phi=0.5,mu=0,sigma=1,TT=500,plot=T)
MA_sim(phi=-0.5,mu=0,sigma=1,TT=500,plot=T)
MA_sim(phi=0.95,mu=-5,sigma=1,TT=500,plot=T)
MA_sim(phi=-0.9,mu=5,sigma=1,TT=500,plot=T)
#play around with other values



#compare AR(1) and MA(1) with the same value of phi
par(mfrow=c(2,1))
plot.ts(AR_sim(phi=0.7,mu=0,sigma=1,TT=500,plot=F),col="blue4",ylab="y",main="AR(1)")
plot.ts(MA_sim(phi=0.7,mu=0,sigma=1,TT=500,plot=F),col="red4",ylab="y",main="MA(1)")

#Can be hard to differentiate. AR maybe a bit more sticky than MA?




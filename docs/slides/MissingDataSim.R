
###########################################################################
###########################################################################
######################## Missing Data Illustration ########################
###########################################################################
###########################################################################

library(sjPlot)

#Generate X
set.seed(1201)
n <- 10000
X <- rnorm(n,mean=2,sd=1)

#Generate Y from X
beta <- c(-1,2); sigma <- 5
Xbeta <- cbind(rep(1,n),X)%*%beta
Y <- rnorm(n,mean=Xbeta,sd=sigma)

#Confirm that the estimates approximate the true values with small standard errors
M_full <- lm(Y~X)


#Generate the missing data indicator R (MCAR)
theta <- c(-0.80,0,0) #MCAR
logit_pi_R <- cbind(rep(1,n),Y,X)%*%theta
pi_R <- exp(logit_pi_R)/(1+exp(logit_pi_R))
R <- rbinom(n,1,pi_R)
mean(R)

#Set Y missing for R = 1
Y_obs <- Y
Y_obs[R==1] <- NA
Data_obs <- data.frame(Y=Y_obs,X=X)

#Now, fit a linear regression model to the data with missing values
M_mcar <- lm(Y~X,data=Data_obs)



#Generate the missing data indicator R (MAR)
theta <- c(0.10,0,-0.50) #MAR
logit_pi_R <- cbind(rep(1,n),Y,X)%*%theta
pi_R <- exp(logit_pi_R)/(1+exp(logit_pi_R))
R <- rbinom(n,1,pi_R)
mean(R)

#Set Y missing for R = 1
Y_obs <- Y
Y_obs[R==1] <- NA
Data_obs <- data.frame(Y=Y_obs,X=X)

#Now, fit a linear regression model to the data with missing values
M_mar <- lm(Y~X,data=Data_obs)


#Generate the missing data indicator R (MNAR)
theta <- c(0.5,-2,-0.10) #MNAR
logit_pi_R <- cbind(rep(1,n),Y,X)%*%theta
pi_R <- exp(logit_pi_R)/(1+exp(logit_pi_R))
R <- rbinom(n,1,pi_R)
mean(R)

#Set Y missing for R = 1
Y_obs <- Y
Y_obs[R==1] <- NA
Data_obs <- data.frame(Y=Y_obs,X=X)

#Now, fit a linear regression model to the data with missing values
M_mnar <- lm(Y~X,data=Data_obs)



#Compare the results from all four
tab_model(M_full,M_mcar,M_mar,M_mnar,show.se=T,show.ci=F,
          dv.labels=c("Full Data","MCAR","MAR","MNAR"))

#How do the estimates compare to the true value of "beta" and to the regression on the full data?
#How about the standard errors of those estimates?





###########################################################################
###########################################################################
########################## Time series analuyses ##########################
###########################################################################
###########################################################################


############################ Part One #####################################
###### Time series modeling of FTSE 100 returns
rm(list = ls())
library(tseries)
library(forecast)
library(ggplot2)

ftse100 <- read.csv("data/ftse2018.csv", header = T)
#invert the order of the rows to make the time series increasing in date
ftse100 <- ftse100[nrow(ftse100):1,]

#make a time series object of the closing prices
tsClose <- ts(ftse100$Close)
ts.plot(tsClose,col="red3")

#hard to tell if this is stationary, and maybe not...
#let's look at autocorrelations
acf(tsClose)
pacf(tsClose)
#looks like the autocorrelations are strong,
#but that lag 1 accounts for most of the autocorrelation

#Tests for stationarity
adf_test <- adf.test(tsClose,alternative = 'stationary')
print(adf_test)
#note that the alternative hypothesis here is "stationary"
#so that low p-values support stationarity
#looks like the series is not stationary
kpss_test <- kpss.test(tsClose)
print(kpss_test)
#by the way, KPSS stands for Kwiatkowski-Philips-Schmidt-Shin
#here, the null hypothesis is actually "stationary"
#so that high p-values support stationarity
#again, looks like the series is not stationary


#To get around the stationary problem,  
#People often (for stock data) work with returns instead
ftse100$return <- (ftse100$Close - ftse100$Open)/ftse100$Open 
tsReturn <- ts(ftse100$return)
ts.plot(tsReturn,col="red3")
acf(tsReturn)
pacf(tsReturn)

adf_test_Return <- adf.test(tsReturn,alternative = 'stationary')
print(adf_test_Return)
#looks like this is stationary, however,
#these returns basically have no "strong" time series structure! 
#let's fit an AR(1) model to the returns anyway
Model1 <- arima(tsReturn, order = c(1,0,0))
Model1

#actually we can use the auto.arima function in R to find the best ARIMA model
#it uses AIC, AICc (corrected for small samples) and BIC
auto.arima(tsReturn)
#again, no time series structure

#let's try auto.arima on the original closing prices
auto.arima(tsClose)
#time series basically taken care off by differencing or using returns instead

#suppose you want to make a prediction of the returns six days ahead
predict(Model1, n.ahead = 6)

#there are a few different ways of defining returns
#we can also use log returns for stocks, where they take log(Close/Open) 
ftse100$logreturn <- log(ftse100$Close/ftse100$Open)
tsLogreturn <- ts(ftse100$logreturn)
ts.plot(tsLogreturn,col="red3")
acf(tsLogreturn)
pacf(tsLogreturn)

adf_test_LogReturn <- adf.test(tsLogreturn,alternative = 'stationary')
print(adf_test_LogReturn)

#same thing. We have no time series structure.
#again, let's fit an AR(1) model to the returns
Model2 <- arima(tsLogreturn, order = c(1,0,0))
Model2

#also predict the returns six days ahead
predict(Model2, n.ahead = 6)




############################ Part One #####################################
###### Time series modeling of melanoma data
cancersun <- read.csv("data/melanoma.csv", header = T)
names(cancersun) = c("year", "melanoma", "sunspot")
str(cancersun)
head(cancersun)

ggplot(cancersun, aes(x=sunspot, y=melanoma)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  labs(title="Melanoma Incidence Rate vs Sunspots")

tsMelanoma <- ts(cancersun$melanoma)
ts.plot(tsMelanoma,col="blue4")

#let's try a linear regression of melanoma on sunspots 
regmelanoma <- lm(melanoma ~ sunspot, data = cancersun)
summary(regmelanoma)
ggplot(cancersun, aes(x=sunspot, y=regmelanoma$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Sunspots")
#it looks nice... nothing obvious going on.  but these data are ordered in time

plot(regmelanoma)

#let's look at residuals versus year
ggplot(cancersun, aes(x=year, y=regmelanoma$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Year")
#huge trend!  

#let's compute the autocorrelations to verify
acf(regmelanoma$resid)
acf(regmelanoma$resid,plot=F)
#The lag-1 autocorrelation is large: 0.86.  There is evidence of serial correlation.
#Failing to account for this will result in inaccurate models

#let's compute the partial autocorrelations
pacf(regmelanoma$resid)
pacf(regmelanoma$resid,plot=F)
#seems like only the lag 1 correlation matters.  But the residuals are not stationary!  

#Let's fit a model that includes year as a predictor,
#to try to take care of the non-stationarity
regmelanoma2 <- lm(melanoma ~ sunspot + year, data = cancersun)
summary(regmelanoma2)
#year is clearly significant

ggplot(cancersun, aes(x=sunspot, y=regmelanoma2$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Sunspots")

ggplot(cancersun, aes(x=year, y=regmelanoma2$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Year")
#obivously a lot better!

#let's do a time series plot of residuals
tsresidregmelanoma2 <- ts(regmelanoma2$residual)
ts.plot(tsresidregmelanoma2,col="blue4")
#seems a lot more reasonable to assume stationarity
#not so many time periods though

adf_test_melanoma <- adf.test(tsresidregmelanoma2,alternative = 'stationary')
print(adf_test_melanoma)

kpss_test <- kpss.test(tsresidregmelanoma2)
print(kpss_test)

#let's look for autocorrelation in residuals...
acf(regmelanoma2$resid); acf(regmelanoma2$resid,plot=F)
pacf(regmelanoma2$resid); pacf(regmelanoma2$resid,plot=F)
#.377 lag 1 correlation in residuals.... still a little high. that could distort SE.
#Since the partial autocorrelations never really go to zero, we might try an MA model
#but let's fit an AR(1) model first

#AR(1) model
tsregmelanoma1 <- arima(cancersun$melanoma, order = c(1, 0, 0),
                        xreg = cbind(cancersun$sunspot, cancersun$year))
tsregmelanoma1

#diagnostics
ggplot(cancersun, aes(x=sunspot, y=tsregmelanoma1$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Sunspots")

ggplot(cancersun, aes(x=year, y=tsregmelanoma1$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Year")
#residuals show a little bit of a pattern -- more negative than positive values
#violation of equal variance perhaps??

acf(tsregmelanoma1$residual)
pacf(tsregmelanoma1$residual)
#much lower, pretty much statistically zero


#MA(1) model
tsregmelanoma2 <- arima(cancersun$melanoma, order = c(0, 0, 1),
                        xreg = cbind(cancersun$sunspot, cancersun$year))
tsregmelanoma2

#diagnostics
ggplot(cancersun, aes(x=sunspot, y=tsregmelanoma2$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Sunspots")

ggplot(cancersun, aes(x=year, y=tsregmelanoma2$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Year")
#a bit better

acf(tsregmelanoma2$residual)
pacf(tsregmelanoma2$residual)
#also lower



#actually let's try something else
#maybe we can get a better model by predicting from sunspots the previous year
prevsunspot <- c(NA, cancersun$sunspot[1:36])
#check to make sure we got it right
cbind(prevsunspot, cancersun$sunspot)
#add to the data
cancersun$prevsunspot <- prevsunspot

#try the linear model with previous year
regmelanoma3 <- lm(melanoma ~ prevsunspot + year, data = cancersun[2:37,])
summary(regmelanoma3)
#significant coefficient for prevsunspot. recall that sunspot was not significant before

ggplot(cancersun[2:37,], aes(x=prevsunspot, y=regmelanoma3$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Sunspots")

ggplot(cancersun[2:37,], aes(x=year, y=regmelanoma3$residual)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") + labs(title="Residuals vs Year")
#both looks better save for one potential outlier (you should investigate)

#time series plot of residuals
tsresidregmelanoma3 <- ts(regmelanoma3$residual)
ts.plot(tsresidregmelanoma3,col="blue4")

adf_test_melanoma <- adf.test(tsresidregmelanoma3,alternative = 'stationary')
print(adf_test_melanoma)

kpss_test <- kpss.test(tsresidregmelanoma3)
print(kpss_test)
#still reasonable to assume stationarity

#let's look for autocorrelation in residuals...
acf(regmelanoma3$residual)
pacf(regmelanoma3$residual)
#very low



#lets try using auto.arima
auto.arima(cancersun$melanoma,xreg = cbind(cancersun$sunspot))
auto.arima(cancersun$melanoma,xreg = cbind(cancersun$sunspot, cancersun$year))
auto.arima(cancersun$melanoma,xreg = cbind(cancersun$prevsunspot, cancersun$year))
#the same conclusions more or less







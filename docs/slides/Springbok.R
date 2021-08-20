
###########################################################################
###########################################################################
########################## The springbok analysis #########################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
#library(dplyr)
library(ggplot2)

###### Load the data
springbok <- read.table("data/springbok.csv",header=TRUE,sep=",")
springbok$LOCNUMBER <- as.factor(springbok$LOCNUMBER)
springbok$SITEI <- as.factor(springbok$SITEI)

###### View properties of the data
str(springbok)
head(springbok)
dim(springbok)


###### Exploratory data analysis
## First, let's look at the predictors alone
table(springbok$YEAR)
#since we care about trend, let's treat year as a discrete variable and 
#substract from the minimum year (1990) to reduce the scale
#we should be trying to fit a full time series model to this data
#but we have not covered any time series models yet
springbok$YEAR90 <- springbok$YEAR - min(springbok$YEAR)
table(springbok$YEAR90)

table(springbok$DATE)
#it is not fully clear what the date variable is 
#but looks like it might be weeks
#We may also want to look at trend for date,
#so let's substract from the minimum date (14) to also reduce the scale
springbok$DATE14 <- springbok$DATE - min(springbok$DATE)
table(springbok$YEAR90,springbok$DATE14)
#there are too many missing entries in the year/date combination
#we cannot add interactions between date and year
#fitting a full time series model also would be challenging anyway

table(springbok$YEAR90,springbok$LOCNUMBER)
table(springbok$YEAR90,springbok$SITEI)
#we do have some data for all the years by site
#not too many for some combinations but let's keep going
#we may be able to explore interactions between them

table(springbok$DATE14,springbok$LOCNUMBER)
#much less data for so many groups here interactions are out of the question


### Now we can explore the relationship between COUNTS and each predictor
## HourFromNoon
ggplot(springbok, aes(x = HourFromNoon, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") + theme_classic() +
  geom_smooth(method="lm",col="red3") +
  labs(title="Springbok counts vs hours from noon")
#we have an obvious outlier
ggplot(springbok[(springbok$HourFromNoon!=min(springbok$HourFromNoon)),],
       aes(x = HourFromNoon, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  labs(title="Springbok counts vs hours from noon")
#let's keep it for now but we need to assess it later
#it will probably infuence the results


## LOCNUMBER
ggplot(springbok, aes(x = LOCNUMBER, y = COUNTS,fill=LOCNUMBER)) +
  geom_boxplot() + theme_classic() +
  #scale_fill_brewer(palette="Blues") +
  theme(legend.position="none") +
  labs(title="Springbok counts by location")
#three locations have way more springboks than others


## YEAR
#first, let's use the raw points
ggplot(springbok, aes(x = YEAR, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  theme_classic() +
  labs(title="Springbok counts vs year")
#no obvious overall upward or downward trend

#Let's use mean counts by year instead
springbok_year <- aggregate(springbok$COUNTS,list(YEAR=springbok$YEAR),mean)
colnames(springbok_year)[2] <- "COUNTS_AGG"
ggplot(springbok_year, aes(x =YEAR, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  theme_classic() +
  labs(title="Mean springbok counts vs year")
#still no obvious overall upward or downward trend

#How about YEAR by LOCNUMBER
ggplot(springbok, aes(x =YEAR, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  theme_classic() +
  labs(title="Springbok counts vs year by location") +
  facet_wrap( ~ LOCNUMBER,ncol=4)
#there are multiple observations by year 

#let's use mean counts instead
springbok_yearsite <- aggregate(springbok$COUNTS,
                                list(YEAR=springbok$YEAR,LOCNUMBER=springbok$LOCNUMBER),mean)
colnames(springbok_yearsite)[3] <- "COUNTS_AGG"
#now let's plot the mean counts this time
#remember that we should see an exponential relationship
ggplot(springbok_yearsite, aes(x =YEAR, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + #change loess to lm to make it easire to see linear trend
  theme_classic() +
  labs(title="Mean springbok counts vs year by location") +
  facet_wrap( ~ LOCNUMBER,ncol=4)
#we see different trends for some sites. We should control for year by site
#we really should do this using a hierarchical model to ensure the sites are actually similar
#however, we haven't covered hierarchical models yet


## DATE
#first, let's use the raw points
ggplot(springbok, aes(x = DATE, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="loess",col="red3") +
  theme_classic() +
  labs(title="Springbok counts vs date")
#slight downward trend

#Let's use mean counts by date instead
springbok_date <- aggregate(springbok$COUNTS,list(DATE=springbok$DATE),mean)
colnames(springbok_date)[2] <- "COUNTS_AGG"
ggplot(springbok_date, aes(x =DATE, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + #change loess to lm to make it easire to see linear trend
  theme_classic() +
  labs(title="Mean springbok counts vs year")
#more obvious downward trend

#How about DATE by LOCNUMBER
ggplot(springbok, aes(x =DATE, y = COUNTS)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  theme_classic() +
  labs(title="Springbok counts vs date by location") +
  facet_wrap( ~ LOCNUMBER,ncol=4)
#different trend for some locations

#let's use mean counts instead
springbok_datesite <- aggregate(springbok$COUNTS,
                                list(DATE=springbok$DATE,LOCNUMBER=springbok$LOCNUMBER),mean)
colnames(springbok_datesite)[3] <- "COUNTS_AGG"
#now let's plot the mean counts this time
#remember that we should see an exponential relationship
ggplot(springbok_datesite, aes(x =DATE, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  theme_classic() +
  labs(title="Mean springbok counts vs date by location") +
  facet_wrap( ~ LOCNUMBER,ncol=4)
#we also see different trends for some sites.
#however, remember that the observed dates actually differ by year
#thus, controlling for year might in fact take care of some of these trends
#also, we don't have enough data to model this interaction confidently



###### Model fitting
#first, let's fit a model with main effects only
springreg1 <- glm(COUNTS ~ YEAR90 + HourFromNoon + DATE14 + LOCNUMBER, 
                  data=springbok, family=poisson)
summary(springreg1)

#residuals and predictions
springregresid1 <- resid(springreg1, type = "pearson")
springregpred1 <- predict(springreg1,type="response")

#residuals vs fitted
qplot(y=springregresid1, x=springregpred1,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")
#not good! we have two separate groups!
#we may also have a variance/over-dispersion issue

#residuals vs HourFromNoon
qplot(y=springregresid1, x=HourFromNoon,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "Hour From Noon", ylab = "Pearson Residuals")
#Also really tough see much here!
#That outlier might be a problem!

#residuals vs YEAR90
qplot(y=springregresid1, x=YEAR90,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "YEAR90", ylab = "Pearson Residuals")
#tough to really see much here!
#we do know that we still need to address the interactions

#residuals vs DATE14
qplot(y=springregresid1, x=DATE14,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "DATE14", ylab = "Pearson Residuals")
#Also really tough see much here!
#maybe some polynomial function

#The residuals vs fitted plot looks most useful here!


#Let's include an interaction between year and location
springreg2 <- glm(COUNTS ~ HourFromNoon + DATE14 + YEAR90*LOCNUMBER, 
                  data=springbok, family=poisson)
summary(springreg2)
anova(springreg1, springreg2, test= "Chisq")
#definitely significant

#residuals and predictions
springregresid2 <- resid(springreg2, type = "pearson")
springregpred2 <- predict(springreg2,type="response")

#residuals vs fitted
qplot(y=springregresid2, x=springregpred2,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")
#addressed some of the separation but
#clearly we still have that some variance/over-dispersion issue


#Let's test an interaction between date and location
#We really should not include this interaction because of the sparse data
springreg3 <- glm(COUNTS ~ HourFromNoon + DATE14*LOCNUMBER + YEAR90*LOCNUMBER, 
                  data=springbok, family=poisson)
summary(springreg3)
anova(springreg2, springreg3, test= "Chisq")
#yes it is significant but I would prefer to exclude it because of the very sparse data
#especially after we already inlcuded so many interaction terms for year:location

#Let's try a polynomial for date
springreg3 <- glm(COUNTS ~ HourFromNoon + poly(DATE14,4) + YEAR90*LOCNUMBER, 
                  data=springbok, family=poisson)
summary(springreg3)
anova(springreg2, springreg3, test= "Chisq")
#The third and fourth order terms are significant
#It would be tough to interpret those terms

#residuals and predictions
springregresid3 <- resid(springreg3, type = "pearson")
springregpred3 <- predict(springreg3,type="response")

#residuals vs fitted
qplot(y=springregresid3, x=springregpred3,data=springbok,col=LOCNUMBER, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")
#no clear indication that it helped the overall residuals


#Let's try to account for the overdispersion using quasipoisson
springreg4 <- glm(COUNTS ~ HourFromNoon + DATE14 + YEAR90*LOCNUMBER,
                        data=springbok, family=quasipoisson)
summary(springreg4)
#Indeed, the overdispersion is estimated to be high
#Notice that the parameter estimates will not change
#That means the residuals and predictions will not change
#confidence intervals and prediction intervals however will change to reflect that
#The standard errors will simply be inflated to account for the overdispersion

#Let's check to see if those polynomial terms are still significant
springreg5 <- glm(COUNTS ~ HourFromNoon + poly(DATE14,4) + YEAR90*LOCNUMBER,
                         data=springbok, family=quasipoisson)
summary(springreg5)
anova(springreg4, springreg5, test= "Chisq")
#hmmmm....not anymore. Looks like the overdispersion confounds things


#This is not a great model but simply for illustration
#We really should build a hierarchical model or time series model 
#(or a combination of both if you are ambitious!)



###### Model interpretation
summary(springreg4)
#let's see if the results change if we remove that outlier
springbok2 <- springbok[springbok$HourFromNoon!=min(springbok$HourFromNoon),]
springreg6 <- glm(COUNTS ~ HourFromNoon + DATE14 + YEAR90*LOCNUMBER,
                  data=springbok2, family=quasipoisson)
summary(springreg6)
#not that much actually!


springbok_newdata <- data.frame(LOCNUMBER=rep(unique(springbok2$LOCNUMBER),each=13),
                        HourFromNoon=0,DATE14=median(springbok$DATE14),
                        YEAR90=seq(0,12),YEAR=seq(1990,2002))
springbok_newdata$pred = predict(springreg4,newdata=springbok_newdata,type="response")

### Plot lines by site
ggplot(springbok_newdata, aes(x = YEAR, y = pred, colour = LOCNUMBER)) +
  #geom_line(aes(x = x$YEAR,y = x$pred), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Predicted Expected Counts") +
  theme(legend.position="none") +
  theme_classic() +
  geom_text(data = springbok_newdata,aes(label = LOCNUMBER), hjust = 0.5, vjust = 1)
#With time of day set at noon and date=24 (week 24),
#We see an increasing trend in counts across years for sites 12, 16 and 23.
#We also see a decreasing trend for the other sites with the most trend observed in site 24.

summary(springreg4)
exp(coef(springreg4))
#Given all other predictors are held constant, every additional hour from noon increases
#the expected count of springbok we see at any site by exp(0.033201), meaning, 3%. 
#This is only really significant at the 0.1 significant level

#Given all other predictors are held constant, every one week increase
#results in a decrease in the expected count of springbok we see at any site by about 2%. 
#we are less likely to see more springboks later in a year (close to winter) than earlier.























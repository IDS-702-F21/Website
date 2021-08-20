###########################################################################
###########################################################################
########################## Minimum wage analysis ##########################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(MatchIt) #for propensity score matching
library(cobalt)
library(ggplot2)


###### Load the data
MinWage <- read.csv("data/MinimumWageData.csv",header=T,
                    colClasses=c("numeric","numeric","numeric","numeric",
                                 "factor","factor","factor","factor"))


###### View properties of the data
str(MinWage)
head(MinWage)
dim(MinWage)
summary(MinWage)
#note that there are more in NJ than PA, and we labeled NJ = 1 and PA = 0
#we will want to switch the 0s and 1s later for matching


###### Covariate balance
#first, let's summarize the predictors by NJ and by PA.
summary(MinWage[MinWage$NJ.PA == 0, 3:8]) #first PA
summary(MinWage[MinWage$NJ.PA == 1, 3:8]) #now NJ
bal.tab(list(treat=MinWage$NJ.PA,covs=MinWage[,3:8],estimand="ATT"))
love.plot(list(treat=MinWage$NJ.PA,covs=MinWage[,3:8],estimand="ATT"),stars = "std")
#the distributions of prior employment are not well balanced
#other variables pretty close, but we might be able to do better by matching

#since there are more in NJ than PA, let's make PA the treated and NJ the control
#we can do this pretty easily by making a new dummy variable
MinWage$PA.NJ = 0
MinWage$PA.NJ[MinWage$NJ.PA == 0] = 1


###### Propensity scores estimation
pscorereg <- glm(NJ.PA ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys + Wendys,
                 data = MinWage, family=binomial)
summary(pscorereg)
#oops... we didn't need to include one of the dummy variables for the restaurants
#all four dummy variables sum up to the intercept, so that's a problem
#just like we usually have to make one level of all factor variables the baseline
#drop Wendys
pscorereg <- glm(NJ.PA ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys,
                 data = MinWage, family=binomial)
summary(pscorereg)
#EmploymentPre seems to be the only significant variable
#Not really a problem since we care about using the model to predict

#now let's estimate the propensity scores
pscores <- predict(pscorereg, type = "response")
#use type="response" since we want probabilities
head(pscores)
summary(pscores)
ggplot(MinWage, aes(pscores)) +
  geom_histogram(alpha=.6,fill=rainbow(10),bins=10)
#we don't have probabilities that are either too close to 0 or 1 which is good.

#look at distribution of propensity scores for treateds and controls
MinWage$state <- "PA"
MinWage$state[MinWage$NJ.PA == 1] <- "NJ"
ggplot(MinWage, aes(y=pscores, x=state, fill=state)) +
  geom_boxplot()
#we can see clear differences in the distributions of propensity scores
#thus, a simple comparison of the outcomes would be confounded
#by differences in the background variables

#actually we need to overlay the distributions to see overlap clearly
ggplot(MinWage, aes(x=pscores, fill=state)) +
  geom_density(alpha=.3) +
  xlim(0, 1)
#the propensity scores overlap very well
#so we can feel good about prospects for matching


###### Propensity scores matching
#main call-- embed the logistic regression inside the call
#start with a main effects only regression
matchesMW <- matchit(PA.NJ ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys,
                    method = "nearest", distance = "logit", data = MinWage)
#Can also do 1-to-n matching
#matchesMW <- matchit(PA.NJ ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys,
#                     method = "nearest", distance = "logit", data = MinWage,ratio=4)
#if you are curious, here are the row numbers of the matched controls
#using built in functions from MatchIt
matchesMW$match.matrix
matchesMW$nn

#there are a few more options here
#we could match with replacement
#we could also do one to many matching
#matchesMW <- matchit(NJ.PA ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys,
#                     method = "nearest", distance = "logit", data = MinWage,
#                     replace=TRUE,ratio =5)


#extract the matched dataset
minwagematcheddata <- match.data(matchesMW)

#let's look at the propensity scores after matching.  the "distance" variable gives scores.
ggplot(minwagematcheddata, aes(y=distance, x=state, fill=state)) +
  geom_boxplot()
#the distributions are more similar!
#Now let's look at balance post-matching for each covariate.
#can see results conveniently using the summary command on the MatchIt object

summary(matchesMW)
#for descriptions of all output, 
#see https://r.iq.harvard.edu/docs/matchit/2.4-20/Output_Values2.html 
#the columns labeled eQQ refer to empirical quantile-quantile plots. 
#They provide the median, mean, and maximum distance between the 
#empirical quantile functions of the treated and control groups.
#we will focus on the percent balance improvement table.
#postive values are good, negative values are bad
#generally improved balance!  Not quite balanced for percentage of Roys restaurants
#let's repeat but this time include the outputs for interactions and squared terms

matchesMWint <- matchit(PA.NJ ~ WagePre*(BurgerKing + KFC + Roys) + 
                         EmploymentPre*(BurgerKing + KFC + Roys), 
                       method = "nearest", distance = "logit", data = MinWage)
summary(matchesMWint)
#this seems to be an improvement! we have closer balance, 
#in general and especially for interactions
#still not good enough though

#let's try squared terms
matchesMWintsq <- matchit(PA.NJ ~ (WagePre + I(WagePre^2))*(BurgerKing + KFC + Roys) + 
                           (EmploymentPre+ I(EmploymentPre^2))*(BurgerKing + KFC + Roys), 
                         method = "nearest", distance = "logit", data = MinWage)
summary(matchesMWintsq)
#that didn't really seem to help...  wages are off.  

#let's drop interactions for wages
matchesMWintsq1 <- matchit(PA.NJ ~ (WagePre + I(WagePre^2))+
                            (EmploymentPre+ I(EmploymentPre^2))*(BurgerKing + KFC + Roys),
                          method = "nearest", distance = "logit", data = MinWage)
summary(matchesMWintsq1)


#let's try removing the quadratic for wage^2
matchesMWintsq2 <- matchit(PA.NJ ~ WagePre+
                            (EmploymentPre+I(EmploymentPre^2))*(BurgerKing + KFC + Roys), 
                          method = "nearest", distance = "logit", data = MinWage)
summary(matchesMWintsq2)
#A little better... let's go with this as our final model and matched set

#we can create the final matched set
minwagematcheddata <- match.data(matchesMWintsq2)
#we can estimate the difference in the outcome variable
trteffct <- mean(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==1]) - 
  mean(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==0])
trteffct

#a problem with this estimator is that there is still imbalance in the treated and 
#matched control covariate distributions!  So, we really shouldn't use it...
#also, we did one-to-one matching and we don't have that much data left
#we are only using 73*2=146 rows from the original data
 
#but, if you wanted to do so, you can treat the data like two independent samples 
#se would be 
se <- sqrt(var(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==1])/73 + 
            var(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==0])/73)

#using the normal approximation, confidence intervals would be
trteffct - 1.96*se
trteffct + 1.96*se
#contains zero so not enough evidence that the treatment effect
#is in fact different from zero

#given small imbalances remaining, we should do a regression analysis to 
#control for covariates when estimating the treatment effect.
#this involves regressing on the relevant covariates and adding a dummy variable
#for the treatment.  This is a regular regression analysis, so I leave the extensive
#regression analysis to you to pursue further!

regmodel <- lm(EmploymentPost ~ PA.NJ+EmploymentPre+WagePre+BurgerKing+KFC+Roys+distance,
               data=minwagematcheddata)
summary(regmodel)
#the treatment effect is definitely different from what we had before
#but also not significant



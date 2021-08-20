###########################################################################
###########################################################################
####################### Political ideology analysis #######################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(nnet)
library(knitr)
library(MASS)



###### Load the data
political <- read.table("data/political.txt", header = T,stringsAsFactors = T)



###### View properties of the data
political$Ideology <- ordered(political$Ideology,
                              levels=c("Very Liberal","Slightly Liberal","Moderate",
                                       "Slightly Conservative","Very Conservative"))
str(political)
head(political)
dim(political)
table(political$Ideology) #we definitely have more data points at the moderate level compared to other levels
table(political)


###### Exploratory data analysis
#we only have factor variables so let's look at tables
#first party
table(political$Ideology, political$Party)
prop.table(table(political$Ideology, political$Party), 2)
chisq.test(table(political$Ideology, political$Party))
#looks like party might be very correlated with ideology. No surprises there!
#now sex
table(political$Ideology, political$Sex)
prop.table(table(political$Ideology, political$Sex), 2)
chisq.test(table(political$Ideology, political$Sex))
#not as significant as party but still a bit correlated


################### Part One: Proportional Odds Model ##################
###### Model fitting
Model1_pom <- polr(Ideology ~ Party + Sex, data=political)
summary(Model1_pom)
#once again there are no p-values directly from function
#however, we can use the CIs for inference
coef(Model1_pom)
confint(Model1_pom)
#CI for party doesnt contain zero, so party is significant
#CI for sex does contain zero, so looks like it is not significant once we have controlled for party
#Interpretation for party:
  #For any fixed ideology level , the estimated odds that a Democrat’s response 
  #is in the liberal direction rather than the conservative direction 
  #is exp(0.9636) = 2.62 times the estimated odds for Republicans.
  #Looks like Democrats tend to be more liberal than Republicans.
exp(confint(Model1_pom))


###### Deviance test
#first, let's test to see whether sex is not significant indeed
Model1_nosex_pom <- polr(Ideology ~ Party, data=political)
anova(Model1_pom, Model1_nosex_pom, test = "Chisq")
#p-value is large -- looks like sex is not a useful predictor, at least once we've controlled for party

#Actually, let's see if we can pick up any signal for the interaction
Model1_int_pom <- polr(Ideology ~ Party*Sex, data=political)
anova(Model1_int_pom, Model1_pom, test = "Chisq")
summary(Model1_int_pom)
confint(Model1_int_pom)
#yes, looks like we do (barely!). 
#while being male vs female does not matter overall, it seems to matter depending on the party



###### Predictions
#predicted probabilities for cases in the model
predprobs_pom <- fitted(Model1_int_pom) 
head(predprobs_pom[sample(nrow(predprobs_pom)),])


###### Diagnostics
#looking at the residuals may not be that meaningful here because we only have two binary variables
rawresid1 <- (as.numeric(political$Ideology) == 1) -  predprobs_pom[,1]
rawresid2 <- (as.numeric(political$Ideology) <= 2) -  rowSums(predprobs_pom[,1:2])
rawresid3 <- (as.numeric(political$Ideology) <= 3) -  rowSums(predprobs_pom[,1:3])
rawresid4 <- (as.numeric(political$Ideology) <= 4) -  rowSums(predprobs_pom[,1:4])

tapply(rawresid1, political$Party, mean)
tapply(rawresid2, political$Party, mean)
tapply(rawresid3, political$Party, mean)
tapply(rawresid4, political$Party, mean)

tapply(rawresid1, political$Sex, mean)
tapply(rawresid2, political$Sex, mean)
tapply(rawresid3, political$Sex, mean)
tapply(rawresid4, political$Sex, mean)



################### Part Two: Multinomial Logistic Model ##################
#Let's compare to a multinomial logistic model
Model1_mlm <-  multinom(Ideology ~ Party + Sex, data=political)
summary(Model1_mlm)
#identical AIC but slightly lower deviance than before. Run summary(Model1_pom) to confirm
#so we don't really lose much but get a more parsimonous model
#also, notice that we have smaller standard errors for the proportional odds model
#definitely an advantage compared to the multinomial logistic
confint(Model1_mlm)
exp(confint(Model1_mlm))


Model1_nosex_mlm <- multinom(Ideology ~ Party, data=political)
anova(Model1_mlm, Model1_nosex_mlm, test = "Chisq")
#p-value is large -- looks like sex is not a useful predictor here either

Model1_int_mlm <- multinom(Ideology ~ Party*Sex, data=political)
anova(Model1_int_mlm, Model1_mlm, test = "Chisq")
summary(Model1_int_mlm)
confint(Model1_int_mlm)
#the interaction is definitely not significant here



#Now fit the proportional odds model to the sesame street data from last time and 
#see how it compares to the multinomial logistic model






###########################################################################
###########################################################################
################## In Class: Interpreting Interactions ####################
###########################################################################
###########################################################################


###### Clear environment and load libraries
rm(list = ls())
#library(arm)
#library(pROC)
#library(e1071)
#library(caret)
#library(ggplot2)
#require(gridExtra)


###### Load the data
setwd("~/Google Drive/Teaching/Courses/2021/Fall/IDS 702/Website/static/slides")
diamonds <- read.csv("data/diamonds.csv", header= T,
                     colClasses = c("numeric","factor","factor","factor","numeric"))
dim(diamonds)
head(diamonds)
summary(diamonds)
str(diamonds)


###### Let's interpret the coefficients of the following models
Model1 <- lm(Price~Carats+Clarity+Certification,data=diamonds)
summary(Model1)

Model2 <- lm(Price~Carats+Clarity+Certification+Clarity:Carats,data=diamonds)
summary(Model2)

Model3 <- lm(Price~Carats+Certification+Clarity+Clarity:Certification,data=diamonds)
summary(Model3)

Model4 <- lm(Price~Carats+Clarity+Certification+Clarity:Certification+Clarity:Carats,data=diamonds)
summary(Model4)


####### How does centering change the interpretations?
diamonds$CaratsCent <- diamonds$Carats - mean(diamonds$Carats)
Model5 <- lm(Price~CaratsCent+Clarity+Certification,data=diamonds)
summary(Model5)

Model6 <- lm(Price~CaratsCent+Clarity+Certification+Clarity:CaratsCent,data=diamonds)
summary(Model6)

Model7 <- lm(Price~CaratsCent+Clarity+Certification+Clarity:Certification,data=diamonds)
summary(Model7)

Model8 <- lm(Price~CaratsCent+Clarity+Certification+Clarity:Certification+Clarity:CaratsCent,data=diamonds)
summary(Model8)



###### How about for logistic regression?
###### Let's go to the arsenic data
arsenic <- read.csv("data/arsenic.csv",header=T,
                    colClasses=c("numeric","numeric","numeric","factor","numeric"))

arsenic$assoc <- factor(arsenic$assoc,levels=c(0,1),
                        labels=c("Not active in community","Active in community"))
arsenic$educnew <- rep(0,nrow(arsenic))
arsenic$educnew[arsenic$educ > 6] <- 1
arsenic$educnew <- factor(arsenic$educnew,levels=c(0,1),
                        labels=c("At most 6 years of educ","More than 6 years of educ"))

table(arsenic$educ,arsenic$educnew)
str(arsenic)
dim(arsenic)
head(arsenic)
summary(arsenic[,-1])
table(arsenic$switch)


###### Let's interpret the coefficients of the following models
###### We will talk in terms of the odds of the price of a diamond being at least $5000
Model1 <- glm(switch ~ arsenic + assoc + educnew, data = arsenic, family = binomial)
summary(Model1)

Model2 <- glm(switch ~ arsenic + assoc + educnew + arsenic:assoc, data = arsenic, family = binomial)
summary(Model2)

Model3 <- glm(switch ~ arsenic + assoc + educnew + assoc:educnew, data = arsenic, family = binomial)
summary(Model3)

Model4 <- glm(switch ~ arsenic + assoc + educnew + assoc:educnew + arsenic:assoc, data = arsenic, family = binomial)
summary(Model4)


####### How does centering change the interpretations?
arsenic$arsenic_c <- arsenic$arsenic - mean(arsenic$arsenic)
Model5 <- glm(switch ~ arsenic_c + assoc + educnew, data = arsenic, family = binomial)
summary(Model5)

Model6 <- glm(switch ~ arsenic_c + assoc + educnew + arsenic_c:assoc, data = arsenic, family = binomial)
summary(Model6)

Model7 <- glm(switch ~ arsenic_c + assoc + educnew + assoc:educnew, data = arsenic, family = binomial)
summary(Model7)

Model8 <- glm(switch ~ arsenic_c + assoc + educnew + assoc:educnew + arsenic_c:assoc, data = arsenic, family = binomial)
summary(Model8)





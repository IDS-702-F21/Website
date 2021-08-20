
###########################################################################
###########################################################################
###################### The contaminated wells analysis ####################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)

###### Load the data
arsenic <- read.csv("data/arsenic.csv",header=T,
                    colClasses=c("numeric","numeric","numeric","factor","numeric"))

###### View properties of the data
arsenic$switch_fac <- factor(arsenic$switch,levels=c(0,1),labels=c("No","Yes"))
arsenic$assoc <- factor(arsenic$assoc,levels=c(0,1),
                        labels=c("Not active in community","Active in community"))
str(arsenic)
dim(arsenic)
head(arsenic)
summary(arsenic[,-1])
table(arsenic$switch)

###### Exploratory data analysis

## We can do boxplots for the numeric variables
# First, let's look at arsenic vs switch
ggplot(arsenic,aes(x=switch_fac, y=arsenic, fill=switch_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Amount of arsenic in well vs switching wells",
       x="Switched to safe well?",y="Amount of arsenic in well") + 
  theme_classic() + theme(legend.position="none") #+
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes"))
# Next, arsenic vs switch by assoc
ggplot(arsenic,aes(x=switch_fac, y=arsenic, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Amount of arsenic in well vs switching wells, by association",
       x="Switched to safe well?",y="Amount of arsenic in well") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ assoc)

# Now, let's look at distance vs switch
ggplot(arsenic,aes(x=switch_fac, y=dist, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Distance to nearest safe well vs switching wells",
       x="Switched to safe well?",y="Distance to nearest safe well") + 
  theme_classic() + theme(legend.position="none")
# Next, distance vs switch by assoc
ggplot(arsenic,aes(x=switch_fac, y=dist, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Distance to nearest safe well vs switching wells, by association",
       x="Switched to safe well?",y="Distance to nearest safe well") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ assoc)

# Finally, education vs switch
ggplot(arsenic,aes(x=switch_fac, y=educ, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Years of schooling of head vs switching wells",
       x="Switched to safe well?",y="Years of schooling of head") + 
  theme_classic() + theme(legend.position="none")
# Next, education vs switch by assoc
ggplot(arsenic,aes(x=switch_fac, y=educ, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Years of schooling of head vs switching wells, by association",
       x="Switched to safe well?",y="Years of schooling of head") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ assoc)
#Actually, education is a discrete variable (not a continuous one)
#so maybe a boxplot isn't the best way to explore the variable 
#In fact, we do need to think a bit carefully about how we want to include it in our model
#We will revisit it soon


## We can do tables for the factor variables
# For switch vs association, look at joint probabilities
table(arsenic[,c("switch_fac","assoc")])
table(arsenic[,c("switch_fac","assoc")])/sum(table(arsenic[,c("switch_fac","assoc")]))
#actually, what we really want are conditional probabilities
#we want to see how the probability of switching changes for different levels of assoc
#that is, the probabilities for switching given each value of association
apply(table(arsenic[,c("switch_fac","assoc")])/sum(table(arsenic[,c("switch_fac","assoc")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(arsenic$switch_fac, arsenic$assoc, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(arsenic[,c("switch_fac","assoc")]))

# We could actually try the same with education
table(arsenic[,c("switch_fac","educ")])
#we do not have that many data points for educ=1 and for educ>12
table(arsenic[,c("switch_fac","educ")])/sum(table(arsenic[,c("switch_fac","educ")]))
apply(table(arsenic[,c("switch_fac","educ")])/sum(table(arsenic[,c("switch_fac","educ")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(arsenic$switch_fac, arsenic$educ, function(x) table(x)/sum(table(x)))
#tapply(arsenic$switch, arsenic$educ, mean)
plot(0:17,tapply(arsenic$switch, arsenic$educ, mean),col='blue4',pch=10)
# Notice that the average probabilty of switching is mostly below 58% below level 7, 
#but mostly above 58% above level 7.

#remember that there are few observations at some of these values of the predictors, 
#so the percentages need to be considered in the context of large uncertainties.  
#but, this does suggest a change at about 7 years of education at least
#we might consider a dummy variable for 7 (or 8) or higher rather than a linear term...
#something to think about for later.

#let's look at binnedplots of continuous predictors versus switch
#ignore the SD lines in these plots
#they are only relevant when plotting binned residuals versus the predicted probabilities
par(mfrow=c(1,1)) 
binnedplot(y=arsenic$switch,arsenic$arsenic,xlab="Arsenic",ylim=c(0,1),col.pts="navy",
           ylab ="Switched to safe well?",main="Binned Arsenic and Switch cases",
           col.int="white") # this is to set the SD lines to white and ignore them
#note the quickly increasing trend followed by flattening. 
#probability does not start to decrease, though, so unlikely we'd want a quadratic term.  
#we would expect some flattening with a linear trend. 
#still, we might need some transformation

binnedplot(y=arsenic$switch,arsenic$dist,xlab="Distance",ylim=c(0,1),col.pts="navy",
           ylab ="Switched to safe well?",main="Binned Distance and Switch cases",col.int="white")
#looks fine: no obvious transformations needed

binnedplot(y=arsenic$switch,arsenic$educ,xlab="Education",ylim=c(0,1),col.pts="navy",
           ylab ="Switched to safe well?",main="Binned Distance and Switch cases",col.int="white")
#looks like it might be decreasing from 0 to 6, then increasing to 12, then not enough data afterwards

# ACTUALLY, DO A QUICK GOOGLE SEARCH ABOUT THE EDUCATION SYSTEM IN BANGLADESH
# WHAT DID YOU FIND? HOW CAN THAT HELP WITH THE EDUCATION VARIABLE?


###### Model fitting
#let's try a logistic regression that has a main effect for every variable and linear predictors
#first begin by centering the continuous predictors 
#we'll leave educ alone since we might recode later
arsenic$arsenic_c <- arsenic$arsenic - mean(arsenic$arsenic)
arsenic$dist_c <- arsenic$dist - mean(arsenic$dist)
arsreg1 <- glm(switch ~ arsenic_c + dist_c + assoc + educ, data = arsenic, family = binomial)
summary(arsreg1)

# INTERPRET THE SIGNIFICANT COEFFICIENTS


###### Model diagnostics

#binned residual plots

#save the raw residuals
rawresid1 <- residuals(arsreg1,"resp")

#binned residual plots
binnedplot(x=fitted(arsreg1),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

binnedplot(x=arsenic$arsenic_c,y=rawresid1,xlab="Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# definitely something to think about here.
# "sharp increase" and then somewhat steady afterwards
# tough to argue this looks "random"
# GOOGLE THE MOST COMMON FUNCTIONS TO SEE WHICH MIGHT BE IDEAL:
# WHAT DOES THE QUADRATIC FUNCTION LOOKS LIKE COMPARED TO THE NATURAL LOG FUNCTION??


binnedplot(x=arsenic$dist_c,y=rawresid1,xlab="Distance centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#not as much of a trend, really but seven points outside the bands.
#something to keep in mind

binnedplot(arsenic$educ,rawresid1,xlab="Education",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks like a decreasing trend from 0 to 6 but an increasing trend from 7 upwards, 
#save for levels above 12. Again not enough data

#let's look at average residuals by education using the tapply command
plot(0:17,tapply(rawresid1, arsenic$educ, mean),col='blue4',pch=10)
#looks upward-downward. Not enough data for some of the levels
#we could try the dummy variable splits. Maybe 0 to 6, 7 to 12, then 12 upwards
#again, maybe we only need to split at 6/7.
#SCIENTIFICALLY, what makes sense?

tapply(rawresid1, arsenic$assoc, mean) 
#nothing helpful here, because we have a binary variable


###### Model validation

#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg1) >= 0.5, "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#Maybe we can try to increase that accuracy.
#Also, the TNR looks low here.

#first, let's repeat with the marginal percentage in the data
mean(arsenic$switch)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg1) >= mean(arsenic$switch), "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#huge difference!  seems a lot of predicted probabilities are in the .5 yo .58  range, so cutoff matters.
#either way, we have large off-diagonal numbers. specificity is sensitive to the cutoff

#look at ROC curve
roc(arsenic$switch,fitted(arsreg1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#pretty tight to the line -- not a strongly predictive logistic regression

#let's see if we can improve the model.

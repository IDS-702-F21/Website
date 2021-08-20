
###########################################################################
###########################################################################
###################### The contaminated wells analysis ####################
###########################################################################
###########################################################################


################################## PART I #################################


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
  geom_boxplot() + #coord_flip() +
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








################################## PART II ################################
## Model building

#based on binned residual plot, let's try a log of arsenic to start with, and see what happens.
arsenic$logarsenic = log(arsenic$arsenic)
arsenic$logarsenic_c = arsenic$logarsenic - mean(arsenic$logarsenic)

arsreg2 = glm(switch ~ logarsenic_c + dist_c + assoc + educ, data = arsenic, family = binomial)
summary(arsreg2)



#back to diagnostics
rawresid2 <- residuals(arsreg2,"resp")

#binned residual plots
binnedplot(x=fitted(arsreg2),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#still looks good!
binnedplot(x=arsenic$logarsenic_c,y=rawresid2,xlab="Log Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#has helped a lot for two points. So, not perfect. 
#well doing model assessment, always remember that we often cannot achieve "perfection"
#we simply try to fix "obvious" issues, including violations.

binnedplot(x=arsenic$dist_c,y=rawresid2,xlab="Distance centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#still not as much of a trend.
#the points are still outside the bands

binnedplot(arsenic$educ,rawresid2,xlab="Education",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

plot(0:17,tapply(rawresid2, arsenic$educ, mean),col='blue4',pch=10)
#similar pattern as before for education.


#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg2) >= 0.5, "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#let's repeat with the marginal percentage in the data
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg2) >= mean(arsenic$switch), "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#some improvement


#look at ROC curve
roc(arsenic$switch,fitted(arsreg2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#let's compare this roc curve to the previous one
roc(arsenic$switch,fitted(arsreg1),plot=T,legacy.axes=T,print.auc =T,col="red3")
roc(arsenic$switch,fitted(arsreg2),plot=T,legacy.axes=T,col="blue3",add=T)
legend('bottomright', c('model1','model2'),lty=c(1,1),
       lwd=c(2,2),col=c('red3','blue3'))

#not much difference from last curve really, although a little more prediction accuracy









#let's see what happens if we collapse education to 2 levels
#we could collapse education to 3 levels but we do not have enough data for the tertiary levels
arsenic$educnew <- rep(0,nrow(arsenic))
arsenic$educnew[arsenic$educ > 6] <- 1
table(arsenic$educ,arsenic$educnew)

arsreg3 = glm(switch ~ logarsenic_c + dist_c + assoc + educnew, data = arsenic, family = binomial)
summary(arsreg3)

#this is scientifically plausible.  Let's keep it!
#should go through binned residuals again to make sure this did not worsen model fit. 
#back to diagnostics
rawresid3 <- residuals(arsreg3,"resp")

#binned residual plots
binnedplot(x=fitted(arsreg2),y=rawresid3,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=arsenic$logarsenic_c,y=rawresid3,xlab="Log Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#pretty much the same as before

binnedplot(x=arsenic$dist_c,y=rawresid3,xlab="Distance centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#pretty much the same as before; only 5 points outside now. much better

tapply(rawresid1, arsenic$educnew, mean) 
# nothing helpful here



#look at new roc curve to see if it is any better
roc(arsenic$switch,fitted(arsreg3),plot=T,legacy.axes=T,print.auc =T,col="red3")

# it is a little better, with a higher area under the curve (AUC = .6632).  
# let's keep this model.




### Interactions in logistic regression

# WHAT DO YOU THINK ARE THE MOST PLAUSIBLE INTERACTIONS?

#scientifically, it is plausible to think that there might be interactions among all the variables and arsenic, 
# or among education and distance.
#let's add the interactions to see if any stand out.

#first, here is a way to explore the data for interactions using the binnedplot command

#lets set up the graphics device to show two plots side by side 
par(mfcol=c(2,1))

#first plot for educnew = 0
binnedplot(arsenic$logarsenic_c[arsenic$educnew==0], y=arsenic$switch[arsenic$educnew==0], 
           xlab = "Log Arsenic", ylab = "Switch cases", main = "Binned Arsenic and Switch cases (Educ < 7)") 

#next the plot for educnew = 1
binnedplot(arsenic$logarsenic_c[arsenic$educnew==1], y=arsenic$switch[arsenic$educnew==1], 
           xlab = "Log Arsenic", ylab = "Switch cases", main = "Binned Arsenic and Switch cases (Educ > 6)") 

#we are looking for differences in the trend.  not strong ones, except possibly at low levels of arsenic.
#I will include an interaction based on scientific arguments in favor of an interaction effect, 
# but I am not expecting a very strong interaction effect based on this plot.


#let's try for association and arsenic
#first plot for assoc = 0
binnedplot(arsenic$logarsenic_c[arsenic$assoc=="Not active in community"], 
           y=arsenic$switch[arsenic$assoc=="Not active in community"], 
           xlab = "Log Arsenic", ylab = "Switch cases", main = "Binned Arsenic and Switch cases (Assoc = 0)") 

#next the plot for assoc = 1
binnedplot(arsenic$logarsenic_c[arsenic$assoc=="Active in community"], 
           y=arsenic$switch[arsenic$assoc=="Active in community"],
           xlab = "Log Arsenic", ylab = "Switch cases", main = "Binned Arsenic and Switch cases (Assoc = 1)") 

#even less reason to suspect an interaction effect from this plot.

#how about distance and education?
#first plot for assoc = 0
binnedplot(arsenic$dist_c[arsenic$assoc=="Not active in community"], 
           y=arsenic$switch[arsenic$assoc=="Not active in community"], 
           xlab = "Distance", ylab = "Switch cases", main = "Binned Distance and Switch cases (Educ < 7)") 
#next the plot for assoc = 1
binnedplot(arsenic$dist_c[arsenic$assoc=="Active in community"], 
           y=arsenic$switch[arsenic$assoc=="Active in community"],
           xlab = "Distance", ylab = "Switch cases", main = "Binned Distance and Switch cases (Educ > 6)") 

#this is a little more interesting -- we see one plot flatten and the other decrease.  
#here an interaction might be useful.


# Next, distance vs switch by education
ggplot(arsenic,aes(x=switch_fac, y=dist, fill=switch_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Distance to nearest safe well vs switching wells, by education",
       x="Switched to safe well?",y="Distance to nearest safe well") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ educnew)


#let's first try the model with all the interactions 
arsreg4 = glm(switch ~ dist_c*educnew  + logarsenic_c * (assoc + educnew), data = arsenic, family = binomial)
summary(arsreg4)

#these collectively look sort of useful, especially the education ones! 

#change in deviance tests to see if the full set of interactions are useful.

anova(arsreg4, arsreg3, test= "Chisq")

#the whole group of interactions is significant.  let's just test if the distance interaction is useful, given the other two in the model.

arsreg4a = glm(switch ~ logarsenic_c * (assoc + educnew) + dist_c, data = arsenic, family = binomial)
summary(arsreg4a)

anova(arsreg4a, arsreg4, test= "Chisq")
#looks like the interaction with distance and education is useful given the others are in the model


#let's test if the distance interaction is useful, given the other two are NOT in the model.

arsreg4b = glm(switch ~ logarsenic_c + assoc + dist_c*educnew, data = arsenic, family = binomial)
summary(arsreg4b)

anova(arsreg4b, arsreg3, test= "Chisq")
#looks like the interaction with distance and education is useful even when the others are NOT in the model

#let's test the other two interactions jointly
anova(arsreg4b, arsreg4, test= "Chisq")
#looks like they are not


#let's make our final model (arsreg5) be the one with only the interaction with distance and education

arsreg5 = glm(switch ~ logarsenic_c + assoc + dist_c*educnew, data = arsenic, family = binomial)
summary(arsreg5)


#let's use the stepwise function to do model selection (using BIC)
n <- nrow(arsenic)
null_model <- glm(switch~1,data=arsenic,family=binomial)
step(null_model,scope=formula(arsreg4),direction="both",
     trace=0,k = log(n))
#pretty much agrees with arsreg5 except for assoc. 
#Let's keep it just because we will try to interpret it (even though it isn't significant)


#let's do the binned residual plots with this perhaps final model one more time!
rawresid5 <- residuals(arsreg5,"resp")

par(mfcol=c(1,1))
binnedplot(x=arsenic$logarsenic_c,y=rawresid5,xlab="Log Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=arsenic$dist_c,y=rawresid5,xlab="Distance centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


#let's do the confusion matrix
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg5) >= 0.5, "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#let's repeat with the marginal percentage in the data
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg5) >= mean(arsenic$switch), "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#still not moving much.... the model can predict only so well


#ROC curve...
roc(arsenic$switch,fitted(arsreg5),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#a little better still... but we really aren't gaining a whole lot.  this is about as
#good as we are going to get with only these variables, apparently.

###model interpretations

confint.default(arsreg5)   #on log odds scale
exp(confint.default(arsreg5))   #on odds scale


#Interpreting arsenic is a bit complicated
#let's make plots to display relationships.

#plot of predicted probabilities as arsenic increases for different groups.
#set distance = to average distance (centering means we don't need to worry about it when making predictions at the average distance)
#we can just set it to zero

#First set arsenic and dist values
new_arsenic <- seq(from = min(arsenic$arsenic), to = max(arsenic$arsenic), by = .1)
newdata <- data.frame(dist_c=rep(0,length(new_arsenic)))
newdata$logarsenic_c <- log(new_arsenic) - mean(arsenic$logarsenic)

#now for association = educnew = 0
newdata$assoc <- factor("Not active in community",levels=levels(arsenic$assoc))
newdata$educnew <- 0
predprobbaseline <- predict(arsreg5,newdata,type='response')

#next association =1, educnew = 0
newdata$assoc <- factor("Active in community",levels=levels(arsenic$assoc))
newdata$educnew <- 0
predprobassoc <- predict(arsreg5,newdata,type='response')

#set association =0 , educnew = 1
newdata$assoc <- factor("Not active in community",levels=levels(arsenic$assoc))
newdata$educnew <- 1
predprobeducnew <- predict(arsreg5,newdata,type='response')

#set association = 1, educnew = 1
newdata$assoc <- factor("Active in community",levels=levels(arsenic$assoc))
newdata$educnew <- 1
predprobeducnewassoc <- predict(arsreg5,newdata,type='response')

plot(y = predprobbaseline, x = new_arsenic, ylab = "Predicted probability", xlab = "Arsenic", 
     main = "Predicted Probability vs. Arsenic for Different Groups",ylim=c(0.2,1),pch= 1,
     col="red2")
points(y=predprobassoc, x=new_arsenic, pch= 2,col="red4")
points(y=predprobeducnew, x=new_arsenic, pch= 3,col="tan2")
points(y=predprobeducnewassoc, x=new_arsenic, pch= 4,col="tan4")
legend('bottomright',pch=c(3,4,1,2),bty = "n",col=c('tan2','tan4','red2','red4'),
       c('assoc=0; educnew=1','assoc=1; educnew=1','assoc=0; educnew=0','assoc=1; educnew=0'))



# Do it yourself: collapse education to the 4 (more scientifically plausible) levels and see what happens instead......


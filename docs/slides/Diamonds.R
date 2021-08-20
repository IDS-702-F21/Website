
###########################################################################
###########################################################################
################## Multiple Regression of Diamonds Data ###################
###########################################################################
###########################################################################


###### Clear environment and load libraries
rm(list = ls())
#library(knitr)
library(ggplot2)
#library(kableExtra)
#library(lattice)
#library(dplyr)
library(rms) #for VIF
library(MASS)




###### Data
diamonds <- read.csv("data/diamonds.csv", header= T,
                     colClasses = c("numeric","factor","factor","factor","numeric"))
dim(diamonds)
head(diamonds)
summary(diamonds)
str(diamonds)




###### EDA
#Is the distribution of the response variable normal?
#hist(diamonds$Price,xlab="Price",main="Distribution of price",col=rainbow(10))
ggplot(diamonds,aes(x=Price)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(16),binwidth = 1000) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price of Diamonds",y="Price") + 
  theme_classic() + theme(legend.position="none")


#Since the distribution of `carats` is not really normal, let's transform the variable. 
#This is the natural log, you can try other bases on your own.
#hist(log(diamonds$Price),xlab="Log Price",main="Distribution of log price",col=rainbow(10))
ggplot(diamonds,aes(x=log(Price))) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(9),binwidth = 0.4) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Log Price of Diamonds",y="Log Price") + 
  theme_classic() + theme(legend.position="none")
#Better but still not fully normal. We will revisit after fitting the model



#Next, explore the relationship between `price` and each predictor
#Use scatter plots for continuous/numeric predictors
#Use boxplots for categorical vpredictors

#plot(diamonds$Price~diamonds$Carats,xlab="Carats",ylab="Price",col='cyan4')
#abline(lm(Price~Carats,data=diamonds),col='red3',lty=10,lwd=2)
ggplot(diamonds,aes(x=Carats, y=Price)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Price vs Carats",x="Carats",y="Price")
#Not exactly linear
  
#Now the relationship between log(price) and carats
#plot(log(diamonds$Price)~diamonds$Carats,xlab="Carats",ylab="Log Price",col='cyan4')
#abline(lm(log(Price)~Carats,data=diamonds),col='red3',lty=10,lwd=2)
ggplot(diamonds,aes(x=Carats, y=log(Price))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Carats",x="Carats",y="Log Price")
#Still not exactly linear
  
#Next, `price` and `color`.
#boxplot(Price~Color,data=diamonds,ylab="Price",xlab="Color",col=rainbow(15))
ggplot(diamonds,aes(x=Color, y=Price, fill=Color)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Price vs Color",x="Color",y="Price") + 
  theme_classic() + theme(legend.position="none")
#Looks like we may have some difference in the means

#How about, log(price) and color instead
ggplot(diamonds,aes(x=Color, y=log(Price), fill=Color)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Color",x="Color",y="Log Price") + 
  theme_classic() + theme(legend.position="none")
#May still have some difference in the means
  
#Next, `price` and `clarity`.
ggplot(diamonds,aes(x=Clarity, y=Price, fill=Clarity)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Price vs Clarity",x="Clarity",y="Price") + 
  theme_classic() + theme(legend.position="none")
#Looks like we may have some difference in the means

#How about, `log(price)` and `clarity` instead
ggplot(diamonds,aes(x=Clarity, y=log(Price), fill=Clarity)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Clarity",x="Clarity",y="Log Price") + 
  theme_classic() + theme(legend.position="none")
#Obvious especially for the first group
  
#Next, `price` and `Certification`.
ggplot(diamonds,aes(x=Certification, y=Price, fill=Certification)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Price vs Certification",x="Certification",y="Price") + 
  theme_classic() + theme(legend.position="none")
#What do you think of this plot?
  
#How about, `log(price)` and `Certification` instead
ggplot(diamonds,aes(x=Certification, y=log(Price), fill=Certification)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Certification",x="Certification",y="Log Price") + 
  theme_classic() + theme(legend.position="none")
#What do you think of this plot?


#Let's make some plots to explore interactions. 
#Use the log(price) going forward
#First, `log(price)` and `Carats` by `Color`
ggplot(diamonds,aes(x=Carats, y=log(Price))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Carats by Color",x="Carats",y="Log Price") +
  facet_wrap( ~ Color,ncol=4)
#Looks mostly like the same trend by color; shouldn't need this interaction

#Next, `log(price)` and `Carats` by `Clarity`
ggplot(diamonds,aes(x=Carats, y=log(Price))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Carats by Clarity",x="Carats",y="Log Price") +
  facet_wrap( ~ Clarity,ncol=4)
#Shouldn't need this interaction

#Next, `log(price)` and `Carats` by `Certification`
ggplot(diamonds,aes(x=Carats, y=log(Price))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Carats by Certification",x="Carats",y="Log Price") +
  facet_wrap( ~ Certification,ncol=4)
#Is there an interaction effect?

#We also can examine interactions among categorical variables
ggplot(diamonds,aes(x=Clarity, y=log(Price), fill=Clarity)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Clarity by Color",x="Clarity",y="Log Price") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Color,ncol=4)
#Looks like the relationship between log price and clarity changes some by color
#Might need that interaction

ggplot(diamonds,aes(x=Certification, y=log(Price), fill=Certification)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Certification by Color",x="Certification",y="Log Price") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Color,ncol=4)
#Shouldn't need this interaction

ggplot(diamonds,aes(x=Certification, y=log(Price), fill=Certification)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Certification by Clarity",x="Certification",y="Log Price") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ Clarity,ncol=4)
#Is there an interaction effect here?

#TAKEAWAYS:
#We see some evidence of non-normality, non-linearity (with `Carats`) and non-constant variance overall. 
#Taking the log transformation helped with those. 
#We might need a quadratic term for `Carats`. Maybe?
#We might also consider interactions between `Clarity` and `Color`.




###### Modeling and Model Assessment

#Let's mean center the numerical predictors (just `Carat`)  to help avoid multicollinearity.
diamonds$CaratsCent <- diamonds$Carats - mean(diamonds$Carats)
diamonds$CaratsCent2 <- diamonds$CaratsCent^2

#Based on our EDA, our candidate model includes:
#   Carats, Carats^2, Color, Clarity, Certification, and Color:Clarity.

#First, a MLR model on price with only main effects
Model1 <- lm(Price~CaratsCent+Color+Clarity+Certification,data=diamonds)
summary(Model1)

#By the way, we can change the baseline levels for the categorical variables.
#Model1 <- lm(Price~CaratsCent+relevel(Color,ref="E")+Clarity+Certification,data=diamonds)
#summary(Model1)

#Now some model assessment
ggplot(diamonds,aes(x=CaratsCent, y=Model1$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Carats",x="Carats (centered)",y="Residuals")
#Clearly, some problems!!!

plot(Model1,which=1:5,col=c("blue4"))
#Even more problems!!

#Let's fit our EDA suggested model instead but without the interaction.
Model2 <- lm(log(Price)~CaratsCent+CaratsCent2+Clarity+Color+Certification,data=diamonds)
summary(Model2)

#Now some model assessment on the new model.
ggplot(diamonds,aes(x=CaratsCent, y=Model2$residual)) + 
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Carats",x="Carats (centered)",y="Residuals")
#Better but still not great

plot(Model2,which=1:5,col=c("blue4"))
#Not the greatest but so much better!

#Do you see any clear violations of the independence and equal variance assumptions?
#Do you see any clear violations of the normality assumption?
#Are there any potential outliers or influential points?


# Let's do a nested $F$-test to see if all the interaction terms between `Clarity` and `Color` are important.
Model2_inter <- lm(log(Price)~CaratsCent+CaratsCent2+Clarity*Color+Certification,
                   data=diamonds)
summary(Model2_inter)
anova(Model2,Model2_inter)
#Very significant; this confirms our EDA.

plot(Model2_inter,which=1:5,col=c("blue4"))


# How about a nested $F$-test for interaction terms between `Color` and `Certification`?
Model2_inter3 <- lm(log(Price)~CaratsCent+CaratsCent2+Color+Clarity*Certification,
                   data=diamonds)
#summary(Model2_inter3)
anova(Model2,Model2_inter3)
#What do we conclude?
#This also confirms our EDA.

#Should we be worried about multicollinearity (really for the continuous variables)?
#library(rms)
vif(Model2)
#Should we be worried?
#How about the other model?
vif(Model2_inter3)
#Should we be worried?



#The transformation really helped but let's try one more thing
#Box Cox transformation (Tukey's formula)
Model2_inter_star  <- lm(Price~CaratsCent+CaratsCent2+Clarity*Color+Certification,
                         data=diamonds) 
#Must fit preferred model on raw scale for response
boxcox_trans <- boxcox(Model2_inter_star,lambda = seq(-5, 5, length = 50))
lambda_trans <- boxcox_trans$x[boxcox_trans$y == max(boxcox_trans$y)]
lambda_trans
#Looks like we should actually be trying a different transformation

Model3 <- lm(Price^lambda_trans~CaratsCent+CaratsCent2+Color+Clarity*Certification,
                    data=diamonds)
#summary(Model3)
plot(Model3,which=1:5,col=c("blue4"))
#Is it better than Model2_inter? I'll let you decide






###########################################################################
###########################################################################
############################# The radon analysis ##########################
###########################################################################
###########################################################################


###### Clear environment and load libraries
rm(list = ls())
library(lattice)
library(dplyr)
library(ggplot2)
library(lme4)


###### Load the data
Radon <- read.csv("data/Radon.txt", header = T,sep="")


###### View properties of the data
Radon$floor <- factor(Radon$floor,levels=c(0,1),labels=c("Basement","First Floor"))
str(Radon)
dim(Radon)
head(Radon) #note that uranium (and thus log_uranium) is measured at the county level
summary(Radon[,-c(2,7)])


###### Exploratory data analysis
#How much data do we have by county?
table(Radon$countyname)
#we don't have enough data in some counties
#this already gets us thinking about hierarchical modeling
#we should look to borrow information across counties.

#Next, look at the response variable
ggplot(Radon,aes(radon)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Radon Levels",y="Radon") + 
  theme_classic() + theme(legend.position="none")
#note that the raw radon levels can only take on positive values.
#also, this is obviously very skewed

#Let's look at log_radon instead.
ggplot(Radon,aes(log_radon)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Log Radon Levels",y="Log Radon") + 
  theme_classic() + theme(legend.position="none")
#Much better! Let's go with log radon for now


set.seed(1000)
sample_county <- sample(unique(Radon$countyname),15,replace=F)
ggplot(Radon[is.element(Radon$countyname,sample_county),],
       aes(x=countyname, y=log_radon, fill=countyname)) +
  geom_boxplot() +
  #scale_fill_brewer(palette="Greens") +
  labs(title="Log radon vs floor",
       x="Lowest living area of each house",y="Log Radon") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


#Next, let's focus on predicting the radon levels from `floor`, 
#this is the only individual-level (different observation for each house) variable we have.
ggplot(Radon,aes(x=floor, y=log_radon, fill=floor)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log radon vs floor",
       x="Lowest living area of each house",y="Log Radon") + 
  theme_classic() + theme(legend.position="none")
#Looks like radon levels are higher for houses with the basement as the lowest living area.

#Let's look at the same relationship by county
#There are too many counties
#so, let's do it for a random sample of counties. 
set.seed(1000)
sample_county <- sample(unique(Radon$countyname),7,replace=F)
ggplot(Radon[is.element(Radon$countyname,sample_county),],
       aes(x=floor, y=log_radon, fill=floor)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log radon vs floor by county",
       x="Lowest living area of each house",y="Log Radon") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ countyname,ncol=4)
#Not enough data for some counties.

#Let's focus on counties with at least 15 houses.
sample_county <- which(table(Radon$countyID) > 15)
ggplot(Radon[is.element(Radon$countyID,sample_county),],
       aes(x=floor, y=log_radon, fill=floor)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log radon vs floor by county",
       x="Lowest living area of each house",y="Log Radon") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ countyname,ncol=4)
#Even though the overall direction is the same, 
#it looks like the actual differences between floor = 0 and floor = 1 differs for some counties.



###########################################################################
###########################################################################
######################## The 1988 Elections analysis ######################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(ggplot2) #Time to pivot to ggplot2
library(lme4)
library(rstan)
library(brms)
library(sjPlot) #another option for making nice html tables
library(lattice) #for ranef


###### Load the data
polls_subset <- read.table("data/polls_subset.txt",header=TRUE)

polls_subset$v_prev <- polls_subset$v_prev*100 #rescale 
polls_subset$region_label <- factor(polls_subset$region,levels=1:5,
                                    labels=c("NE","S","N","W","DC"))
#we consider DC as a separate region due to its distinctive voting patterns
polls_subset$edu_label <- factor(polls_subset$edu,levels=1:4,
                                 labels=c("No HS","HS","Some College","College Grad"))
polls_subset$age_label <- factor(polls_subset$age,levels=1:4,
                                 labels=c("18-29","30-44","45-64","65+"))
#the data includes states but without the names, which we will need, so let's grab that from R datasets
data(state) #"state" is an R data file (type ?state from the R command window for info)
state.abb #does not include DC, so we will create ours
#In the polls data, DC is the 9th "state" in alphabetical order
state_abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
polls_subset$state_label <- factor(polls_subset$state,levels=1:51,labels=state_abbr)
rm(list = ls(pattern = "state")) #remove unnecessary values in the environment


###### View properties of the data  
head(polls_subset)
dim(polls_subset)
str(polls_subset)


###### Exploratory data analysis
#You should do this yourself!
table(polls_subset$edu,polls_subset$age)


###### Model fitting
#Let's start with a simple model with fixed effects of race and gender plus random intercepts for state
model1 <- glmer(bush ~ black + female + (1|state_label), family=binomial(link="logit"),data=polls_subset)
summary(model1)
tab_model(model1)
#can also try probit; identical results
#model1 <- glmer(bush ~ black + female + (1|state_label), family=binomial(link="probit"),data=polls_subset)
#summary(model1)
dotplot(ranef(model1, condVar=TRUE))

#Let's includes other relevant survey factors, such as region, 
#prior vote history (average Republican vote share in the three prior elections, 
#adjusted for home-state and home-region candidate effects), 
#age category, and education category.
model2 <- glmer(bush ~ black + female + v_prev + edu_label:age_label + (1|state_label) + (1|region_label),
                family=binomial(link="logit"),data=polls_subset)
summary(model2)
tab_model(model2)
dotplot(ranef(model2, condVar=TRUE))


#Let's change the edu:age interaction to random effects instead
model3 <- glmer(bush ~ black + female + v_prev + (1|state_label) + (1|region_label) + (1|edu_label:age_label),
                family=binomial(link="logit"),data=polls_subset)
summary(model3)
tab_model(model3)
dotplot(ranef(model3, condVar=TRUE))

#Let's use a Bayesian approach instead
#Try xcode-select --install from the Terminal app or the Terminal tab of the RStudio console
model_bayes <- brm(bush ~ black + female + v_prev + (1|state_label) + (1|region_label) + (1|edu_label:age_label),
              family=bernoulli(link="logit"),data=polls_subset, control = list(adapt_delta = 0.995))
summary(model_bayes)
head(predict(model_bayes))
dim(posterior_samples(model_bayes))
#head(posterior_samples(model_bayes))


#visualize results: plot estimated OR's with 2 SD error bars ea. side
plot_model(model_bayes,show.values = TRUE)
plot_model(model_bayes,type="pred")
p <- plot_model(model_bayes,type="re",show.values = TRUE,ri.nr = c(1,2))
p[[17]]
p[[18]]

library(gridExtra)
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]])
grid.arrange(p[[5]],p[[6]],p[[7]],p[[8]])
grid.arrange(p[[9]],p[[10]],p[[11]],p[[12]])
grid.arrange(p[[13]],p[[14]],p[[15]],p[[16]])




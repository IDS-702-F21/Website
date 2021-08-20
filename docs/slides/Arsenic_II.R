
###########################################################################
###########################################################################
############### The contaminated wells analysis - tree methods ############
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)

###### Load the data
arsenic <- read.csv("data/arsenic.csv",header=T,
                    colClasses=c("numeric","numeric","numeric","factor","numeric"))


###### Leverage details from previous analysis
arsenic$arsenic_c <- arsenic$arsenic - mean(arsenic$arsenic)
arsenic$dist_c <- arsenic$dist - mean(arsenic$dist)
arsenic$logarsenic <- log(arsenic$arsenic)
arsenic$logarsenic_c <- arsenic$logarsenic - mean(arsenic$logarsenic)
arsenic$educnew <- rep(0,nrow(arsenic))
arsenic$educnew[arsenic$educ > 6] <- 1


###### Logistic regression
## Use final model from our previous analysis (see Arsenic.R file)
arsreg <- glm(switch ~ logarsenic_c + assoc + dist_c*educnew, data = arsenic, family = binomial)
summary(arsreg)

## Confusion matrix
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(arsreg) >= 0.5, "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

## ROC curve
roc(arsenic$switch,fitted(arsreg),plot=T,print.thres="best",legacy.axes=T,print.auc =T,col="red3")


###### CART
library(tree)
ars_cart <- tree(as.factor(switch) ~ arsenic + assoc + dist + educ, data = arsenic)
#ars_cart <- tree(switch ~ arsenic + assoc + dist + educnew, data = arsenic) #same result
summary(ars_cart)
plot(ars_cart)
text(ars_cart)
ars_cart
head(predict(ars_cart,type="class"))

## Confusion matrix
Conf_mat_cart <- confusionMatrix(predict(ars_cart,type="class"),
                            as.factor(arsenic$switch),positive = "1")
Conf_mat_cart$table #compare to Conf_mat$table
Conf_mat_cart$overall["Accuracy"] #compare to Conf_mat$overall["Accuracy"]
#lower accuracy than the logistic model
Conf_mat_cart$byClass[c("Sensitivity","Specificity")]
#compare to Conf_mat$byClass[c("Sensitivity","Specificity")]
#higher specificity but lower sensitivity

## ROC curve
roc(arsenic$switch,predict(ars_cart,type="vector")[,2],plot=T,print.thres="best",
    legacy.axes=T,print.auc =T,col="red3")
#worse AUC


###### Bagging
library(randomForest)
ars_bagg <- randomForest(as.factor(switch) ~ arsenic + assoc + dist + educ, data = arsenic,mtry=4)

ars_bagg

## Confusion matrix
Conf_mat_bagg <- confusionMatrix(predict(ars_bagg,type="response"),
                                 as.factor(arsenic$switch),positive = "1")
Conf_mat_bagg$table #compare to Conf_mat$table
Conf_mat_bagg$overall["Accuracy"]
#much worse
Conf_mat_bagg$byClass[c("Sensitivity","Specificity")]

## ROC curve
roc(arsenic$switch,predict(ars_bagg,type="prob")[,2],plot=T,print.thres="best",
    legacy.axes=T,print.auc =T,col="red3")
#also worse than logistic regression and cart


###### Random forest
ars_rf <- randomForest(as.factor(switch) ~ arsenic + assoc + dist + educ,
                       data = arsenic, importance =TRUE)
ars_rf
varImpPlot(ars_rf)
#MeanDecreaseAccuracy: mean decrease of accuracy in predictions when the variable is excluded. 
#MeanDecreaseGini: measure of total decrease in node impurity that
#results from splits over that variable, averaged over all trees
#importance(ars_rf)

## Confusion matrix
Conf_mat_rf <- confusionMatrix(predict(ars_rf,type="response"),
                                 as.factor(arsenic$switch),positive = "1")
Conf_mat_rf$table #compare to Conf_mat$table
Conf_mat_rf$overall["Accuracy"]
Conf_mat_rf$byClass[c("Sensitivity","Specificity")]

## ROC curve
roc(arsenic$switch,predict(ars_rf,type="prob")[,2],plot=T,print.thres="best",
    legacy.axes=T,print.auc =T,col="red3")
#worse than logistic regression, comparable to cart and better than bagging


###### Boosting
library(gbm)
ars_boost <-  gbm(switch ~ arsenic + assoc + dist + educ,data=arsenic,
               distribution="bernoulli",n.trees=5000, interaction.depth=2)
summary(ars_boost)

## Confusion matrix
pred_prob_boost <- predict(ars_boost,n.trees=5000,type="response")
Conf_boost <- confusionMatrix(as.factor(ifelse(pred_prob_boost >= 0.5, "1","0")),
                            as.factor(arsenic$switch),positive = "1")
Conf_boost$table
Conf_boost$overall["Accuracy"]
#much better accuracy although we probably over fit.
#use out-of-sample RMSE or cross validation using average RMSE
Conf_boost$byClass[c("Sensitivity","Specificity")]

## ROC curve
roc(arsenic$switch,pred_prob_boost,
    plot=T,print.thres="best",legacy.axes=T,print.auc =T,col="red3")
#much better AUC. Again, we may have overfit! 


#reduce the number of trees and lamba, then try again.
ars_boost2 <-  gbm(switch ~ arsenic + assoc + dist + educ,data=arsenic, shrinkage = 0.01,
                  distribution="bernoulli",n.trees=500, interaction.depth=2)
summary(ars_boost2)

## Confusion matrix
pred_prob_boost2 <- predict(ars_boost,n.trees=500,type="response")
Conf_boost2 <- confusionMatrix(as.factor(ifelse(pred_prob_boost2 >= 0.5, "1","0")),
                              as.factor(arsenic$switch),positive = "1")
Conf_boost2$table
Conf_boost2$overall["Accuracy"]
Conf_boost2$byClass[c("Sensitivity","Specificity")]

## ROC curve
roc(arsenic$switch,pred_prob_boost2,
    plot=T,print.thres="best",legacy.axes=T,print.auc =T,col="red3")
#results closer to what we had for logistic regression without
#spending so much time tuning the model

#YOU SHOULD PICK NUMBER OF TREES AND LAMBDA USING RMSE (PREFERABLY OUT-OF-SAMPLE)!



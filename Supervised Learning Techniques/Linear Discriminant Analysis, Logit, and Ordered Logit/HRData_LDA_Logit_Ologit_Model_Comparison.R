# ------------------------------------------------------------------------------
# Script Name: HRData_LDA_Logit_Ologit_Model_Comparison.R
# Purpose: Predicting performance scores with linear discriminant analysis (LDA), logit, and ordered logit models and comparing.
# Author: Ryan Mannix
# Date: 8-14-2025
# Dependencies: MASS, dplyr, caret, car, rms, glue, HRdata2groups.csv
# ------------------------------------------------------------------------------

library(MASS)
library(dplyr)
library(caret)
library(car)
library(rms)
library(glue)
frame<-read.csv("HRdata2groups.csv")
head(frame)

#Making CollapseScore - Converting performance score (PerfScoreID) into 2 groups.
frame <- frame %>% 
  mutate(
    CollapseScore = case_when(
      PerfScoreID > 2 ~ 1,
      PerfScoreID <3 ~ 0
      )
    )

#Correlation Matrix
cor(frame) 

#Scatter Plots
plot(frame$MechanicalApt, frame$CollapseScore) #Mechanical aptitude (MechanicalApt) test score against collapsed score.
plot(frame$VerbalApt, frame$CollapseScore) #Verbal aptitude (VerbalApt) test score against collapsed score.
plot(frame$VerbalApt, frame$MechanicalApt) # Mechanical Aptitude vs. Verbal Aptitude 


#Linear Regression to Observe Statistical Significance 
model <- lm(CollapseScore ~ frame$MechanicalApt + frame$VerbalApt, data = frame)
summary(model)  # This function tells you about the model, including the intercept and coefficients, their errors, and t- and p-values. It also tells you values for the R^2, adjusted R^2, and F-statistic.
anova_model<-anova(model) 
anova_model # This gives you the analysis of variance table for the model, which gives you more information about the errors around the coefficients

#Variance Inflation Factor to Detect Multicollinearity
vif(model) #MechanicalApt and VerbalApt are highly correlated.

#Not Included In Model: 
#VerbalApt because it is correlated with MechanicalApt

#Included In Model:
#MechanicalApt 

########


# Linear Discriminant Analysis
model <- lm(CollapseScore ~ frame$MechanicalApt, data = frame)
summary(model)  
anova_model<-anova(model) 
anova_model

#Applying Linear Discriminant Analysis
pred=predict(model, frame)
meanunsat=mean(pred[frame$CollapseScore==0]) # Average unsatisfactory MechanicalApt score
meansat=mean(pred[frame$CollapseScore==1]) # Average satisfactory MechanicalApt score
Cutoff <- 0.5*(meanunsat+meansat) # Setting cutoff halfway between averages. Common practice in LDA.

#Joining Original Frame and Predictions - Making a new column for prediction LDA classification.
frame <- cbind(frame, pred) # making a single dataframe.
frame <- frame %>% 
  mutate(
    lm_group = case_when(
      pred > Cutoff ~ 1,
      pred <= Cutoff ~ 0
    )
  )

#Model Accuracy (2 Performance Groups)
LDA_Model_Accuracy <- sum(frame$lm_group == frame$CollapseScore) / length(frame$CollapseScore)
print(LDA_Model_Accuracy)

#Scatter Plots Observing Model & Cutoff Value
plot(frame$MechanicalApt, frame$CollapseScore) + abline(model)
plot(frame$MechanicalApt, frame$pred) + abline(model)  + abline(a=Cutoff, b=0)
plot(frame$CollapseScore, frame$pred) + abline(a=Cutoff, b=0)

###########

#Logit Model Analysis
logit<-lrm(CollapseScore ~ MechanicalApt, data = frame)
print(logit)
#Find probability of each each individual being in group 1 (satisfactory group).
log_fitteddata<-predict(logit,data=frame, type = "fitted.ind") # “fitted.ind” - probability of each individual being satisfactory
print(log_fitteddata)


#Joining Original Frame and Predictions - Making a new column for logit model prediction.
frame <- cbind(frame, log_fitteddata)
frame <- frame %>% 
  mutate(
    logit_group = case_when(
      log_fitteddata > 0.5 ~ 1,
      log_fitteddata <= 0.5 ~ 0
    )
  )

#Model Accuracy (2 Performance Groups)
Logit_Model_Accuracy <- sum(frame$logit_group == frame$CollapseScore) / length(frame$CollapseScore)
print(Logit_Model_Accuracy)

# Scatter Plot - Gain insight into the marginal impacts of each of the variables
plot(frame$MechanicalApt, log_fitteddata)
plot(frame$CollapseScore, log_fitteddata)

###########

#Ordered Logit Model Analysis
ologit<-lrm(PerfScoreID ~ MechanicalApt, data = frame)
print(ologit)

#Finding the probability of each individual of being in each performance score group.
ologit_fitteddata<-predict(ologit,data=frame, type = "fitted.ind") # “fitted.ind” 
print(ologit_fitteddata)


frame <- cbind(frame, ologit_fitteddata)
frame <- frame %>% 
  mutate(
    ologit_group = max.col(select(.,starts_with('PerfScoreID=')), ties.method = "first")
    )

#Model Accuracy (4 Performance Groups)
Ologit_Model_Accuracy <- sum(frame$ologit_group == frame$PerfScoreID) / length(frame$PerfScoreID)
print(Ologit_Model_Accuracy)

#Scatter Plots - Mechanical Aptitude vs. Ordered Logit Probabilities & More
plot(frame$MechanicalApt, frame$`PerfScoreID=1`)
plot(frame$MechanicalApt, frame$`PerfScoreID=2`)
plot(frame$MechanicalApt, frame$`PerfScoreID=3`)
plot(frame$MechanicalApt, frame$`PerfScoreID=4`)
plot(frame$MechanicalApt, frame$ologit_group)
plot(frame$PerfScoreID, frame$ologit_group)

#Comparison
glue("Linear Discriminant Model Accuracy: {round(LDA_Model_Accuracy * 100, 2)}% (2 prediction groups)")
glue("Logit Model Accuracy: {round(Logit_Model_Accuracy * 100, 2)}% (2 prediction groups)")
glue("Ordered Logit Model Accuracy: {round(Ologit_Model_Accuracy * 100, 2)}% (4 prediction groups)")

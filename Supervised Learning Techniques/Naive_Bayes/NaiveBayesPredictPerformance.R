# Naive Bayes Example on HR Data

# Load the required library
library(e1071)

# Read in the data
frame<-read.csv("NaiveBayesHW.csv")
head(frame)

# Ensure PerfScore is a factor (for classification)
frame$PerfScore <- as.factor(frame$PerfScore)

# Build Naive Bayes Model using MechanicalApt to predict PerfScore
NBModel<-naiveBayes(PerfScore~ MechanicalApt, data=frame)
print(NBModel)

# Predict PerfScore for all data
pred<-predict(NBModel, frame)
print(pred)

# Add predictions to the dataframe
frame$pred <- pred

# Calculate number of correctly predicted Performance Scores
correct <- sum(frame$pred == frame$PerfScore)
cat("Number of correctly predicted Performance Scores:", correct, "\n")
cat("Total Number of Performance Scores:", nrow(frame), "\n")
cat("Percent Correct:", paste0(round((correct / nrow(frame) * 100),2), "%"), "\n")

# Examine Individual #10
Individual_10<-frame[10,]
print(Individual_10)

# Predict probability for Individual #10
pred1<-predict(NBModel, Individual_10, type="raw")
print(pred1)


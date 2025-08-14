# ------------------------------------------------------------------------------
# Script Name: HRData_Classification_Tree.R
# Purpose: Build and evaluate a decision tree to predict performance scores
# Author: Ryan Mannix
# Date: 8-13-2025
# Dependencies: partykit, HRdata4groups.csv
# ------------------------------------------------------------------------------

# Load the 'partykit' package, which provides tools for recursive partitioning (e.g., decision trees)
library(partykit)

# Read in the CSV file 'HRdata4groups.csv' into a dataframe called 'frame'
frame <- read.csv("HRdata4groups.csv")

# Convert the 'PerfScoreID' column to a factor (categorical variable)
frame$PerfScoreID <- as.factor(frame$PerfScoreID)

# Fit a conditional inference tree to predict 'PerfScoreID' using 'MechanicalApt' as the predictor
ctout <- ctree(PerfScoreID ~ MechanicalApt, data = frame)

# Use the trained tree model to make predictions on the training data
ctpred <- predict(ctout, frame)

# Calculate and print the classification accuracy by comparing predictions to actual labels
mean(ctpred == frame$PerfScoreID)  # Expected output: 0.943

# Create a confusion matrix to show the counts of predicted vs actual performance scores
table <- table(ctpred, frame$PerfScoreID)

# Print the confusion matrix
print(table)

# Plot the decision tree
plot(ctout)

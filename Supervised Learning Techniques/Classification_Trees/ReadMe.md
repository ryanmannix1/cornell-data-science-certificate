📄 README: HRData_Classification_Tree.R  

📌 Purpose  
This script builds and evaluates a decision tree classifier to predict employee performance scores based on mechanical aptitude using the conditional inference tree algorithm (ctree). It provides model accuracy, a confusion matrix, and a visual decision tree.

📂 Input Data  
File: HRdata4groups.csv  
Required Columns -  
PerfScoreID: Ordinal or categorical performance rating (target variable)  
MechanicalApt: Numeric score representing mechanical aptitude (predictor)  

🛠️ Dependencies  
The script requires the following R package:  
library(partykit)  # For building and visualizing conditional inference trees  

Install it if not already available:  
install.packages("partykit")

🔄 Workflow Overview  
-Data Import & Preparation  
-Loads the HR dataset from CSV  
-Converts PerfScoreID into a categorical variable (factor) for classification  
-Model Training  
-Trains a conditional inference tree using ctree() from the partykit package  
-Predicts performance groupings (PerfScoreID) using MechanicalApt  
-Model Evaluation  
-Calculates accuracy by comparing predicted vs actual performance scores  
-Generates a confusion matrix for detailed classification performance  
-Visualization  
-Plots the decision tree structure using plot(ctout)  

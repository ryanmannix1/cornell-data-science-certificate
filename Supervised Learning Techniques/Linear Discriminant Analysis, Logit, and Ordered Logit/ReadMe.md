📄 README: HRData_LDA_Logit_Ologit_Model_Comparison.R 

📌 Script Purpose  
This R script compares three statistical models — Linear Discriminant Analysis (LDA), Logistic Regression (Logit), and Ordered Logistic Regression (Ologit) — to predict employee performance scores using HR aptitude data. It evaluates model performance and provides accuracy comparisons for each.

📂 Input Data
File: HRdata2groups.csv
Expected Columns:
PerfScoreID: Ordinal performance score (used as outcome for ologit, collapsed into binary for LDA/Logit)
MechanicalApt: Mechanical aptitude score
VerbalApt: Verbal aptitude score

🛠️ Key Dependencies
Make sure the following R packages are installed and loaded:  
library(MASS)  
library(dplyr)  
library(caret)  
library(car)  
library(rms)  
library(glue)  


🧪 Modeling Steps    
1. Data Preprocessing   
The PerfScoreID is collapsed into a binary variable called CollapseScore:  
1 → High performers (PerfScoreID > 2)    
0 → Low performers (PerfScoreID ≤ 2)  

2. Exploratory Analysis
Correlation matrix and scatter plots  
Linear regression with multicollinearity check via VIF  
VerbalApt is removed due to high correlation with MechanicalApt  

📈 Modeling and Evaluation

✅ Linear Discriminant Analysis (LDA)  
Predicts CollapseScore using MechanicalApt  
Uses midpoint of group means as cutoff   

✅ Logistic Regression (Logit)  
Predicts probability of satisfactory (CollapseScore = 1)  
Classification threshold: 0.5   

✅ Ordered Logistic Regression (Ologit)  
Predicts the full ordinal PerfScoreID (4 levels)  
Uses the rms::lrm() function and fitted.ind predictions  
Maximum predicted probability used for group classification  

📊 Model Comparison Summary  
At the end, the script prints a comparison using glue():  
"Linear Discriminant Model Accuracy: ...% (2 prediction groups)"  
"Logit Model Accuracy: ...% (2 prediction groups)"  
"Ordered Logit Model Accuracy: ...% (4 prediction groups)"  

🔍 Assumptions & Notes  
Only MechanicalApt is used as a predictor due to collinearity  
Assumes the CSV file is in the working directory  
Uses max.col() to classify the most probable performance score in ordered logit predictions

🧼 To-Do / Improvements  
Add model diagnostics (e.g., ROC curves, confusion matrices)   
Consider cross-validation or train-test split for robustness  

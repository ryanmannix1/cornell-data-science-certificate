library(plotly)
library(e1071)
library(dplyr)
frame <- read.csv("acquisitionacceptanceSVM.csv")
head(frame)

#Correlation Matrix
cor(frame)

#Making a training and test data set
#ind<- sample(1:nrow(frame), round(.7*1531))
#print(ind)
#train_data <- frame[ind,]
#test_data <- frame[-ind,]
#print(test_data)
#save(ind, test_data, train_data, file = ".../Train_Test_Data_Split.RData")


#Using Previously Created Training and Testing Data for Consistency
load("Train_Test_Data_Split.RData")

#3D Plot of Binary Variables
plot_ly(x=train_data$After, y=train_data$Price75, z=train_data$Price90, type = "scatter3d", mode = "markers", color= train_data$Accept)

#3D Plot of Continuous Variables
train_data2 <- train_data %>% filter(train_data$CurMarketValue <4000000)
plot_ly(x=train_data2$CurMarketValue, y=train_data2$HomeTenure, z=train_data2$Distance, type = "scatter3d", mode = "markers", color= train_data2$Accept)

#Summarizing Data
train_data %>% filter(Accept == 1) %>% 
  summarise(
    Price75_Count = sum(Price75==1),
    Price90_Count = sum(Price90==1),
    Price100_Count = sum(Price100==1),
    Price110_Count = sum(Price110==1),
    Price125_Count = sum(Price125==1),
    After_Count = sum(After==1),
    count = sum(Accept==1)
  )

#Making "Accept" a factor
train_data$Accept <- as.factor(train_data$Accept)

# -------------------- RBF Kernel SVM Model --------------------

# Train SVM model with RBF kernel on selected features
result_RBF <- svm(formula = Accept~ HomeTenure + After + Price75 + Price90, 
              kernel= "radial", gamma=1.5, cost = 25, data=train_data)
print(result_RBF)
###Training Confusion Matrix
pred<- predict(result_RBF, train_data)
table(pred,train_data$Accept)
###Testing Confusion Matrix
pred<- predict(result_RBF, newdata = test_data)
table(pred,test_data$Accept)
###Accuracy
Test_Table <- table(pred,test_data$Accept)
print('Percentage of Correct Predictions')
sum(test_data$Accept == pred)/length(test_data$Accept)
print('Precision of Correct Accept Predictions')
Test_Table[2,2]/(Test_Table[2,1]+Test_Table[2,2])
print('Precision of Correct Decline Predictions')
Test_Table[1,1]/(Test_Table[1,1]+Test_Table[1,2])

# Add prediction outcome and correctness labels for visualization
test_data$pred <- pred
test_data <- test_data %>%
  mutate(Prediction_Status = 
           case_when(
             test_data$Accept == 1 & test_data$pred ==1 ~ 'Predicted Accept ; Accepted',
             test_data$Accept == 0 & test_data$pred ==1 ~ 'Predicted Accept ; Declined',
             test_data$Accept == 0 & test_data$pred ==0 ~ 'Predicted Decline ; Declined',
             test_data$Accept == 1 & test_data$pred ==0 ~ 'Predicted Decline ; Accepted'
           ),
         Correct =  
           case_when(
            test_data$Accept == test_data$pred ~ 1,
            test_data$Accept != test_data$pred ~ 0
           ),
         Correct_Label =  
           case_when(
             test_data$Accept == test_data$pred ~ 'Correct',
             test_data$Accept != test_data$pred ~ 'Incorrect'
           )
  )

# 3D plot showing prediction outcomes and accuracy (RBF model)
plot_ly(x=test_data$After, y=test_data$Price75, z=test_data$HomeTenure, type = "scatter3d", mode = "markers", color= test_data$Prediction_Status, colors = c('#cd32cd', '#cd32cd', '#0C4B8E', '#0C4B8E'), symbol = test_data$Correct,  symbols = c('circle', 'circle-open')) %>%
  layout(title= 'Home Offer Acceptance Prediction Accuracy', scene = list(xaxis = list(title = 'After'), yaxis = list(title = 'Price75'), zaxis = list(title = 'HomeTenure (Yrs)')))

# Another 3D plot showing correct vs incorrect predictions
plot_ly(x=test_data$After, y=test_data$Price75, z=test_data$HomeTenure, type = "scatter3d", mode = "markers", color= test_data$Correct_Label, colors = c('#32cd32', '#BF382A'), symbol = test_data$Price90,  symbols = c('circle-open', 'circle')) %>%
  layout(title= 'Home Offer Acceptance Prediction Accuracy', scene = list(xaxis = list(title = 'After'), yaxis = list(title = 'Price75'), zaxis = list(title = 'HomeTenure (Yrs)')))



# -------------------- Linear Kernel SVM Model --------------------

# Train linear kernel SVM on same features
result_linear <- svm(formula = Accept~ HomeTenure + After + Price75 + Price90, 
                     kernel= "linear", cost = 25, data=train_data)
print(result_linear)
####Training Confusion Matrix
pred_linear<- predict(result_linear, train_data)
table(pred_linear,train_data$Accept)
####Testing Confusion Matrix
pred_linear<- predict(result_linear, newdata = test_data)
table(pred_linear,test_data$Accept)
###Accuracy
Test_Table_linear <- table(pred_linear,test_data$Accept)
print('Percentage of Correct Predictions')
sum(test_data$Accept == pred_linear)/length(test_data$Accept)
print('Precision of Correct Accept Predictions')
Test_Table_linear[2,2]/(Test_Table_linear[2,1]+Test_Table_linear[2,2])
print('Precision of Correct Decline Predictions')
Test_Table_linear[1,1]/(Test_Table_linear[1,1]+Test_Table_linear[1,2])

# Add columns for comparison between RBF and Linear models
test_data$pred_linear <- pred_linear
test_data <- test_data %>%
  mutate(Prediction_Status_Linear = 
           case_when(
             test_data$Accept == 1 & test_data$pred_linear ==1 ~ 'Predicted Accept ; Accepted',
             test_data$Accept == 0 & test_data$pred_linear ==1 ~ 'Predicted Accept ; Declined',
             test_data$Accept == 0 & test_data$pred_linear ==0 ~ 'Predicted Decline ; Declined',
             test_data$Accept == 1 & test_data$pred_linear ==0 ~ 'Predicted Decline ; Accepted'
           ),
         Correct_Linear =  
           case_when(
             test_data$Accept == test_data$pred_linear ~ 1,
             test_data$Accept != test_data$pred_linear ~ 0
           ),
         # Identify where the two models differ in prediction performance
         Prediction_Diff=
           case_when(
             test_data$Accept == 1 & test_data$pred ==1 & test_data$pred_linear ==0 ~ 'RBF Correct Only',
             test_data$Accept == 0 & test_data$pred ==0 & test_data$pred_linear ==1 ~ 'RBF Correct Only',
             test_data$Accept == 0 & test_data$pred ==1 & test_data$pred_linear ==0 ~ 'Linear Correct Only',
             test_data$Accept == 1 & test_data$pred ==0 & test_data$pred_linear ==1 ~ 'Linear Correct Only',
             TRUE ~ 'Other'
            )
  )

# 3D plot comparing prediction differences between RBF and Linear models
plot_ly(x=test_data$After, y=test_data$Price75, z=test_data$HomeTenure, type = "scatter3d", mode = "markers", color= test_data$Prediction_Diff, colors = c('#2a67bf', '#2a67bf', '#ededed', '#2abf83', '#2abf83' ), symbol = test_data$Prediction_Diff,  symbols = c('circle','circle-open','circle','circle','circle')) %>%
  layout(title= 'Prediction Differences Between RBF & Linear SVM', scene = list(xaxis = list(title = 'After'), yaxis = list(title = 'Price75'), zaxis = list(title = 'HomeTenure (Yrs)')))


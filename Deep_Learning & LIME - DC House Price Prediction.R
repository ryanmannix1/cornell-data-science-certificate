library(h2o)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(lime)

# Initialize H2O
h2o.init() 

# Load the DC housing data
frame <- read.csv("DC_PropertieResidentialunder1mill.csv") 
glimpse(frame) # View a summary of the dataset (10,617 rows, 108 columns)

# Filter out rows with NA values in column "X"
frame_filt <- frame %>% filter(!(is.na(frame$X))) 

# Convert house condition (CNDTN) from character to numeric (1-6 scale)
frame_filt <- frame_filt %>%  
  mutate( 
    CONDITION = case_when(
      CNDTN == "Poor" ~ 1,
      CNDTN == "Fair" ~ 2,
      CNDTN == "Average" ~ 3,
      CNDTN == "Good" ~ 4,
      CNDTN == "Very Good" ~ 5,
      CNDTN == "Excellent" ~ 6
  )
  )

# Round Latitude and Longitude to 3 decimal places for easier grouping
frame_filt$LONGITUDE <- round(frame_filt$X,3)
frame_filt$LATITUDE <- round(frame_filt$Y,3)

# Extract 4-digit code from Census Block column
pattern <- " (\\w+)"
frame_filt<- frame_filt %>% 
  mutate(
    CENSUS_BLOCK_4Digit = str_extract(CENSUS_BLOCK, pattern)
  )
frame_filt$CENSUS_BLOCK_4Digit <- as.numeric(frame_filt$CENSUS_BLOCK_4Digit)

# Convert SQUARE column to numeric type
frame_filt$SQUARE <- as.numeric(frame_filt$SQUARE)

# Load the pre-sampled 70% training indices
load("ind2.RData") #ind <- sample(1:nrow(frame_filt), (0.7*nrow(frame_filt)))
train <- frame_filt[ind,]
test<- frame_filt[-ind,]


######################
# Add average price for categorical columns
######################

# Condition-based price percentage relative to average price
CNDTN_Table <- train %>% group_by(CNDTN) %>% 
  summarize(
    count_by_CNDTN = n(),
    Avg_Train_Price_By_CNDTN = mean(PRICE)
  )
CNDTN_Table$DC_Train_Mean <- mean(train$PRICE)
CNDTN_Table$CNDTN_Prcnt_Avg <- CNDTN_Table$Avg_Train_Price_By_CNDTN / CNDTN_Table$DC_Train_Mean
train <- train %>% left_join(CNDTN_Table[,c(1,5)], by="CNDTN")
test <- test %>% left_join(CNDTN_Table[,c(1,5)], by="CNDTN")

# Add average price by EXTWALL, ROOF, and INTWALL columns
# These are added in a similar manner, with each categorical variable grouped and summarized based on PRICE
# The tables are then merged with train and test datasets

EXTWALL_Table <- test %>% group_by(EXTWALL) %>% summarize(Avg_Price_By_EXTWALL = mean(PRICE))
EXTWALL_Table <- EXTWALL_Table %>% arrange(Avg_Price_By_EXTWALL)
EXTWALL_Table$EXTWALL_INDEX <- c(1:nrow(EXTWALL_Table))
train <- train %>% left_join(EXTWALL_Table[,c(1,3)], by="EXTWALL")
test <- test %>% left_join(EXTWALL_Table[,c(1,3)], by="EXTWALL")

ROOF_Table <- train %>% group_by(ROOF) %>% summarize(Avg_Price_By_ROOF = mean(PRICE))
ROOF_Table <- ROOF_Table %>% arrange(Avg_Price_By_ROOF)
ROOF_Table$ROOF_INDEX <- c(1:nrow(ROOF_Table))
train <- train %>% left_join(ROOF_Table[,c(1,3)], by="ROOF")
test <- test %>% left_join(ROOF_Table[,c(1,3)], by="ROOF")

INTWALL_Table <- train %>% group_by(INTWALL) %>% summarize(Avg_Price_By_INTWALL = mean(PRICE))
INTWALL_Table <- INTWALL_Table %>% arrange(Avg_Price_By_INTWALL)
INTWALL_Table$INTWALL_INDEX <- c(1:nrow(INTWALL_Table))
train <- train %>% left_join(INTWALL_Table[,c(1,3)], by="INTWALL")
test <- test %>% left_join(INTWALL_Table[,c(1,3)], by="INTWALL")


# Add neighborhood-based price information. (Test price information by neighborhood derived from training data)
Assess_NBHD_Table <- train %>% group_by(ASSESSMENT_NBHD) %>% 
  summarize(
    Avg_Price_By_NBHD = mean(PRICE),
    Sd_Train_Price_By_NBHD = sd(PRICE),
    Count_By_NBHD = n()
  )
train <- train %>% left_join(Assess_NBHD_Table, by="ASSESSMENT_NBHD")
test <- test %>% left_join(Assess_NBHD_Table, by="ASSESSMENT_NBHD")


# Add sub-neighborhood-based average price information. Filled in NAs with neighborhood average prices.
Assess_SUBNBHD_Table <- train %>% group_by(ASSESSMENT_SUBNBHD) %>% summarize(Avg_Price_By_SUBNBHD = mean(PRICE))
train <- train %>% left_join(Assess_SUBNBHD_Table, by="ASSESSMENT_SUBNBHD")
Train_INDEX <- c(1:nrow(train))
SUBNBHD_NAs <- Train_INDEX[train$ASSESSMENT_SUBNBHD == ""]
ASSESSMENT_NBHD <- train$ASSESSMENT_NBHD[SUBNBHD_NAs]
NBHD_forSUBNBHDNAs_MeanPrice <- data.frame(ASSESSMENT_NBHD) %>% left_join(Assess_NBHD_Table, by="ASSESSMENT_NBHD")
train[SUBNBHD_NAs,"Avg_Price_By_SUBNBHD"] <- NBHD_forSUBNBHDNAs_MeanPrice$Avg_Price_By_NBHD 

test <- test %>% left_join(Assess_SUBNBHD_Table, by="ASSESSMENT_SUBNBHD")
Test_INDEX <- c(1:nrow(test))
SUBNBHD_NAs <- c(Test_INDEX[is.na(test$Avg_Price_By_SUBNBHD)], Test_INDEX[test$ASSESSMENT_SUBNBHD == ""])
ASSESSMENT_NBHD <- test$ASSESSMENT_NBHD[SUBNBHD_NAs]
NBHD_forSUBNBHDNAs_MeanPrice <- data.frame(ASSESSMENT_NBHD) %>% left_join(Assess_NBHD_Table, by="ASSESSMENT_NBHD")
test[SUBNBHD_NAs,"Avg_Price_By_SUBNBHD"] <- NBHD_forSUBNBHDNAs_MeanPrice$Avg_Price_By_NBHD 


# Add price by census block, filling missing values with census tract prices.
Census_Block_Table <- train %>% group_by(CENSUS_TRACT,CENSUS_BLOCK_4Digit) %>% summarize(Avg_Train_Price_By_CENSUS_BLOCK = mean(PRICE))
train <- train %>% left_join(Census_Block_Table, by=c("CENSUS_TRACT","CENSUS_BLOCK_4Digit"))
test <- test %>% left_join(Census_Block_Table, by=c("CENSUS_TRACT","CENSUS_BLOCK_4Digit"))
Test_INDEX <- c(1:nrow(test))
CB_NAs <- Test_INDEX[is.na(test$Avg_Train_Price_By_CENSUS_BLOCK)]
CENSUS_TRACT <- test$CENSUS_TRACT[CB_NAs]

Census_Tract_Table <- train %>% group_by(CENSUS_TRACT) %>% summarize(Avg_Train_Price_By_CENSUS_TRACT = mean(PRICE))
CT_forCBNAs_MeanPrice <- data.frame(CENSUS_TRACT) %>% left_join(Census_Tract_Table, by="CENSUS_TRACT")
test[CB_NAs,"Avg_Train_Price_By_CENSUS_BLOCK"] <- CT_forCBNAs_MeanPrice$Avg_Train_Price_By_CENSUS_TRACT
test[is.na(test$Avg_Train_Price_By_CENSUS_BLOCK),"Avg_Train_Price_By_CENSUS_BLOCK"] <- test[is.na(test$Avg_Train_Price_By_CENSUS_BLOCK), "Avg_Price_By_SUBNBHD"]


# Add price by latitude/longitude. Since test is based on training data some test latitudes/longitudes don't have average prices. Filling NAs with closest latitude/longitude average prices.
LAT_LONG_Table <- train %>% group_by(LONGITUDE,LATITUDE) %>% summarize(Avg_Train_Price_By_LAT_LONG = mean(PRICE))
train <- train %>% left_join(LAT_LONG_Table, by= c("LONGITUDE", "LATITUDE"))
test <- test %>% left_join(LAT_LONG_Table, by= c("LONGITUDE", "LATITUDE"))
test_LAT_LONG_NAs <- test[is.na(test$Avg_Train_Price_By_LAT_LONG), c("LONGITUDE", "LATITUDE", "Avg_Train_Price_By_LAT_LONG")]
test_LAT_LONG_NAs$Sum_Of_Squares <- test_LAT_LONG_NAs$LONGITUDE^2 + test_LAT_LONG_NAs$LATITUDE^2
LAT_LONG_Table$Sum_Of_Squares <- LAT_LONG_Table$LONGITUDE^2 + LAT_LONG_Table$LATITUDE^2
for (i in 1:nrow(test_LAT_LONG_NAs)) {
  Value_If_Above <- test_LAT_LONG_NAs$Sum_Of_Squares[i] + min(abs(LAT_LONG_Table$Sum_Of_Squares - test_LAT_LONG_NAs$Sum_Of_Squares[i]))
  Value_If_Below <- test_LAT_LONG_NAs$Sum_Of_Squares[i] - min(abs(LAT_LONG_Table$Sum_Of_Squares - test_LAT_LONG_NAs$Sum_Of_Squares[i]))
  test_LAT_LONG_NAs$Avg_Train_Price_By_LAT_LONG[i] <- LAT_LONG_Table$Avg_Train_Price_By_LAT_LONG[LAT_LONG_Table$Sum_Of_Squares == Value_If_Above | LAT_LONG_Table$Sum_Of_Squares == Value_If_Below ]
}
test[rownames(test_LAT_LONG_NAs),"Avg_Train_Price_By_LAT_LONG"]<- test_LAT_LONG_NAs$Avg_Train_Price_By_LAT_LONG

#Add ward-based average price, standard deviation, and count information.
Ward_Table <- train %>% group_by(Ward) %>% 
  summarize(
    Avg_Train_Price_By_Ward = mean(PRICE),
    Sd_Train_Price_by_Ward = sd(PRICE),
    Count_by_Ward = n()
  )
train <- train %>% left_join(Ward_Table, by="Ward")
test <- test %>% left_join(Ward_Table, by="Ward")

# Add CNDTN_Prcnt_Avg_By_QUADRANT- House "Condition" expressed as a percentage of average price by quadrant
QUADRANT_CNDTN_Table <- train %>% group_by(QUADRANT,CNDTN) %>% 
  summarize(
    count_by_QUADRANT_CNDTN = n(),
    Avg_Train_Price_By_QUADRANT_CNDTN = mean(PRICE)
  )

QUADRANT_Table <- train %>% group_by(QUADRANT) %>% 
  summarize(
    Avg_Train_Price_By_QUADRANT = mean(PRICE),
  )
QUADRANT_CNDTN_Table <- QUADRANT_CNDTN_Table %>% left_join(QUADRANT_Table, by="QUADRANT")
QUADRANT_CNDTN_Table$CNDTN_Prcnt_Avg_By_QUADRANT <- round(QUADRANT_CNDTN_Table$Avg_Train_Price_By_QUADRANT_CNDTN / QUADRANT_CNDTN_Table$Avg_Train_Price_By_QUADRANT, 4)
train <- train %>% left_join(QUADRANT_CNDTN_Table[,c(1,2,6)], by=c("QUADRANT", "CNDTN"))
test <- test %>% left_join(QUADRANT_CNDTN_Table[,c(1,2,6)], by=c("QUADRANT","CNDTN"))


##########################
# Prepare data for deep learning model
##########################

# Remove unnecessary columns and prepare the features for modeling
train_NN <- train %>% select(-c("LONGITUDE":"CENSUS_BLOCK_4Digit", "ASSESSMENT_NBHD":"CENSUS_BLOCK", "QUADRANT", "X.1", "CNDTN", "logPrice", "EXTWALL":"INTWALL", "Single", "Residential_0.Condo_1"))
test_NN <- test %>% select(-c("LONGITUDE":"CENSUS_BLOCK_4Digit", "ASSESSMENT_NBHD":"CENSUS_BLOCK", "QUADRANT", "X.1", "CNDTN", "logPrice", "EXTWALL":"INTWALL", "Single", "Residential_0.Condo_1"))

train_NN_h2o <- as.h2o(train_NN)
test_NN_h2o<- as.h2o(test_NN)

# List of dependent variables for model to use.
X_list <- colnames(train_NN)
X_list <- X_list[c(-15)]

# Correlation matrix and scatter plots used to aid with variable selection above

##Correlation Matrix
cor(train_NN)
corr <- cor(train_NN, method="pearson") #Look at the correlation matrix for all variables in this data set
corr_melt <- melt(corr)
ggplot(corr_melt, aes(x=Var1, y=Var2, fill= value))+ geom_tile() +
  geom_text(aes(label = round(value, 2), color = "grey")) + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
##Scatter_Plots
colnames <- colnames(train_NN)
for (i in 1:length(train_NN)) {
  plot(train_NN[,colnames[i]], train_NN$PRICE, ylab= "Price", xlab=colnames[i])
  filename <- paste("Price_vs_",colnames[i],".jpg", sep = "") 
  dev.copy(jpeg, filename, width = 800, height= 600)
  dev.off()
}

##########################
# Perform grid search for optimal deep learning model
##########################

hyper_params <- list(
  activation= c("Rectifier", "Tanh"),
  hidden = list(c(36,18,9), c(9,6,4), c(36,9), c(36,24,16), c(45,27,9), c(42,30,18,6)),
  epochs = c(30, 500, 1000, 3000),
  l1= c(0,.01,.02),
  l2= c(0,.01,.02)
)

#Run grid search
dl_grid_12 <- h2o.grid(algorithm = "deeplearning",
                    x=X_list,
                    y="PRICE",
                    training_frame = train_NN_h2o,
                    validation_frame = test_NN_h2o,
                    nfolds=5,
                    adaptive_rate= TRUE,
                    #epochs= 30,
                    standardize=TRUE,
                    hyper_params = hyper_params,
                    seed = 33
                    )

# Retrieve the best model from grid search
grid <- h2o.getGrid(grid_id = dl_grid_12@grid_id, sort_by = "mae")
best_model_id <- grid@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Review model 
print(best_model)
summary(best_model)
plot(best_model)
h2o.varimp_plot(best_model, 10)

##########################
# Model Evaluation & Predictions
##########################

#Training Set Predictions / Evaluation
train_predictions <- h2o.predict(best_model,train_NN_h2o)
print(train_predictions)

mean(abs(train_predictions - train_NN_h2o$PRICE))

plot(as.data.frame(train_predictions)$predict, train_NN$PRICE, xlab = "H2O DL Price Prediction", ylab = "Price")
abline(0,1,lw=3,col='red')


#Test Set Predictions / Evaluation
test_predictions <- h2o.predict(best_model,test_NN_h2o)
print(test_predictions)

mean(abs(test_predictions - test_NN_h2o$PRICE))

plot(as.data.frame(test_predictions)$predict, test_NN$PRICE, xlab = "H2O DL Price Prediction", ylab = "Price")
abline(0,1,lw=3,col='red')

##########################
# LIME for Model Explanation
##########################
# Local Interpretable Model-agnostic Explanations (LIME) is a technique that approximates any black box... 
# machine learning model with a local, interpretable model to explain each individual prediction.

# Selecting 6 sample rows
for_lime <- sample(1:nrow(train_NN), 6)
train_NN_df <- as.data.frame(train_NN)
data_for_lime <- train_NN_df[for_lime,]
print(data_for_lime)

# Reviewing predictions
predict_data_for_lime <- as.h2o(data_for_lime)
predictions_for_lime <- h2o.predict(best_model,predict_data_for_lime)
print(predictions_for_lime)

# Running LIME. Looking at top 5 features.
explainer_price <- lime(data_for_lime, best_model)
explanation <- explain(data_for_lime, explainer_price, n_labels = 2, n_features = 5)

# Plotting LIME explanations
plot_features(explanation, ncol=3)
plot_explanations(explanation)
print(explanation)

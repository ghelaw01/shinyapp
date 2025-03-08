# train_and_deploy.R  
library(xgboost)  
library(data.table)  
library(ggplot2)  
library(caret)  
library(plumber)  

# Load Data  
df <- fread("data/processed/machine_data_clean.csv")  

# Define Features and Target  
target <- "machine_failure"  
features <- setdiff(names(df), target)  

# Convert all feature columns to numeric   
df[, (features) := lapply(.SD, as.numeric), .SDcols = features]  

# Train/Test Split  
set.seed(42)  
trainIndex <- createDataPartition(df[[target]], p = 0.8, list = FALSE)  
trainData <- df[trainIndex]  
testData <- df[-trainIndex]  

# Convert Data to xgboost Matrix  
dtrain <- xgb.DMatrix(data = as.matrix(trainData[, ..features]), label = trainData[[target]])  
dtest <- xgb.DMatrix(data = as.matrix(testData[, ..features]), label = testData[[target]])  

# Train XGBoost Model  
params <- list(objective = "binary:logistic", eval_metric = "logloss")  
model <- xgb.train(params = params, data = dtrain, nrounds = 100)  

# Save Model  
xgb.save(model, "deployment/model.xgb")  

# Start Plumber API - this should be run separately after the model is trained  
# r <- plumb("plumber.R")  
# r$run(host = "0.0.0.0", port = 8000)  

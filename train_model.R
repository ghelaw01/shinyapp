library(xgboost)
library(data.table)
library(caret)

# Load Data
df <- fread('data/processed/machine_data_clean.csv')

# Define Features and Target
target <- 'machine_failure'
features <- setdiff(names(df), target)

# Train/Test Split
set.seed(42)
trainIndex <- createDataPartition(df[[target]], p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Convert Data to xgboost Matrix
dtrain <- xgb.DMatrix(data = as.matrix(trainData[, features, with = FALSE]), label = trainData[[target]])
dtest <- xgb.DMatrix(data = as.matrix(testData[, features, with = FALSE]), label = testData[[target]])

# Train XGBoost Model
params <- list(objective = "binary:logistic", eval_metric = "logloss")
model <- xgb.train(params, dtrain, nrounds = 100)

# Save Model
xgb.save(model, 'deployment/model.xgb')

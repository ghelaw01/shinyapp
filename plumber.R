# plumber.R  
library(plumber)  
library(xgboost)  
library(data.table)  
library(jsonlite)  

# Print environment information  
message("Working directory: ", getwd())  
message("R version: ", R.version.string)  
message("xgboost version: ", packageVersion("xgboost"))  

# Create a synthetic dataset with a clear pattern  
set.seed(123)  
n <- 1000  
synthetic_data <- data.frame(  
  time_stamp = as.numeric(Sys.time()) + 1:n,  
  sensor_1 = runif(n),  
  sensor_2 = runif(n),  
  sensor_3 = runif(n)  
)  

# Create a rule: if sensor_1 > 0.7, then failure = 1  
synthetic_data$failure <- ifelse(synthetic_data$sensor_1 > 0.7, 1, 0)  

# Convert to DMatrix  
dtrain <- xgb.DMatrix(  
  data = as.matrix(synthetic_data[, c("time_stamp", "sensor_1", "sensor_2", "sensor_3")]),  
  label = synthetic_data$failure  
)  

# Train a model  
params <- list(  
  objective = "binary:logistic",  
  eval_metric = "logloss",  
  max_depth = 3  
)  
model <- xgb.train(params, dtrain, nrounds = 50)  

# Test the model with a few examples  
test_cases <- list(  
  list(time_stamp = as.numeric(Sys.time()), sensor_1 = 0.2, sensor_2 = 0.5, sensor_3 = 0.3),  # Should predict 0  
  list(time_stamp = as.numeric(Sys.time()), sensor_1 = 0.8, sensor_2 = 0.5, sensor_3 = 0.3)   # Should predict 1  
)  

for (i in 1:length(test_cases)) {  
  test_input <- matrix(  
    c(test_cases[[i]]$time_stamp, test_cases[[i]]$sensor_1, test_cases[[i]]$sensor_2, test_cases[[i]]$sensor_3),  
    nrow = 1,  
    dimnames = list(NULL, c("time_stamp", "sensor_1", "sensor_2", "sensor_3"))  
  )  
  test_pred <- predict(model, test_input)  
  message("Test case ", i, ": sensor_1 = ", test_cases[[i]]$sensor_1,   
          ", prediction = ", test_pred,   
          ", binary = ", ifelse(test_pred > 0.5, 1, 0))  
}  

#* @apiTitle Machine Failure Prediction API  
#* @apiDescription Predicts machine failure based on sensor data  

#* Make predictions for Machine Failure  
#* @param data:object The input data containing time_stamp, sensor_1, sensor_2, and sensor_3 values  
#* @post /predict  
#* @serializer json  
function(data) {  
  if (missing(data) || is.null(data)) {  
    return(list(error = "Missing parameter: data"))  
  }  
  
  # Convert to data.table  
  dt_input <- as.data.table(data)  
  
  # Process time_stamp  
  if ("time_stamp" %in% names(dt_input)) {  
    dt_input[, time_stamp := as.numeric(as.POSIXct(time_stamp, format = "%Y-%m-%d %H:%M:%S"))]  
  }  
  
  # Define features  
  features <- c("time_stamp", "sensor_1", "sensor_2", "sensor_3")  
  
  # Check for missing features  
  missing_features <- setdiff(features, names(dt_input))  
  if (length(missing_features) > 0) {  
    return(list(error = paste("Missing features:", paste(missing_features, collapse = ", "))))  
  }  
  
  # Create matrix  
  input_matrix <- as.matrix(dt_input[, ..features])  
  
  # Print input for debugging  
  message("Input matrix for prediction:")  
  print(input_matrix)  
  
  # Generate prediction  
  pred <- predict(model, input_matrix)  
  
  # Print prediction for debugging  
  message("Raw prediction: ", pred)  
  
  # Return result  
  list(  
    failure_prediction = ifelse(pred > 0.5, 1, 0),  
    probability = as.numeric(pred),  
    input_used = as.list(dt_input),  
    model_info = "Using synthetic model where sensor_1 > 0.7 indicates failure"  
  )  
}  

#* @get /health  
#* @serializer json  
function() {  
  # Test with both a failure case and non-failure case  
  test_input_fail <- matrix(c(as.numeric(Sys.time()), 0.8, 0.5, 0.3), nrow = 1,   
                            dimnames = list(NULL, c("time_stamp", "sensor_1", "sensor_2", "sensor_3")))  
  test_input_ok <- matrix(c(as.numeric(Sys.time()), 0.2, 0.5, 0.3), nrow = 1,   
                          dimnames = list(NULL, c("time_stamp", "sensor_1", "sensor_2", "sensor_3")))  
  
  list(  
    status = "API is running",  
    model_loaded = TRUE,  
    test_prediction_should_fail = predict(model, test_input_fail),  
    test_prediction_should_pass = predict(model, test_input_ok)  
  )  
}  
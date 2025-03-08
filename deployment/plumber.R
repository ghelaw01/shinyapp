# plumber.R  
library(xgboost)  
library(data.table)  
library(jsonlite)  

# Load the pre-trained model  
model <- tryCatch({  
  xgb.load("deployment/model.xgb")  
}, error = function(e) {  
  stop("Error loading model: " + e$message)  
})  

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
  
  # Convert input data to data.table  
  dt_input <- as.data.table(data)  
  
  # Process time_stamp if needed (convert from a datetime string to numeric POSIXct)  
  if ("time_stamp" %in% names(dt_input)) {  
    dt_input[, time_stamp := as.numeric(as.POSIXct(time_stamp, format = "%Y-%m-%d %H:%M:%S"))]  
  }  
  
  # Define expected features  
  features <- c("time_stamp", "sensor_1", "sensor_2", "sensor_3")  
  
  # Check for missing features  
  missing_features <- setdiff(features, names(dt_input))  
  if (length(missing_features) > 0) {  
    return(list(error = paste("Missing feature(s):", paste(missing_features, collapse = ", "))))  
  }  
  
  # Convert to matrix using the variable columns with the .. prefix  
  input_matrix <- as.matrix(dt_input[, ..features])  
  
  # Generate prediction using the xgboost model  
  pred <- predict(model, input_matrix)  
  
  # Return the binary prediction along with raw probabilities  
  list(  
    failure_prediction = ifelse(pred > 0.5, 1, 0),  
    probability = as.numeric(pred)  
  )  
}  

#* @get /health  
#* @serializer json  
function() {  
  list(status = "API is running")  
}  


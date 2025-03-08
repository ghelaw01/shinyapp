library(plumber)  
r <- plumb("plumber.R")  
r$run(host = "127.0.0.1", port = 8000)  

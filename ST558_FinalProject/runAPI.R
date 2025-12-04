library(plumber)
r <- plumb("ST558_FinalProject/MyAPI.R")
r$run(port = 8001)
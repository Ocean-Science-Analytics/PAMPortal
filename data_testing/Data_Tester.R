# Read in data

library(jsonlite)

# SON file
json_file <- file.choose()
json_data <- fromJSON(json_file)

# RDS file
rds_file <- file.choose() 
rds_data <- readRDS(rds_file)





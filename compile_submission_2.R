# Load libraries

# library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(doParallel)

#  Create predictions

rm(list = ls())

train_1 <- fread("data/train_2.csv", header = T, encoding = "UTF-8")

source("R/helpers.R")


# system.time(temp <- lapply(1:500, function(x) {predict_1(slice(train_1, x))}))
# system.time(submission_test <- do.call(rbind, temp))


cl <- makeCluster(4)
registerDoParallel(cl)

# registerDoParallel(cores = 2)
# registerDoSEQ() 

loop_num <- 8
lap_size <- 100

# submission_test <- foreach(i=1:nrow(train_1), .combine=rbind.data.frame) %dopar% {
system.time(submission_test <- foreach(i=1:loop_num, .combine=rbind, .packages = c("dplyr", "magrittr", "tidyr", "stringr", "lubridate", "data.table")) %dopar% {
  
  # size <- 1000
  temp <- lapply((lap_size*(i-1) + 1):(lap_size*i), function(x) {predict_2(slice(train_1, x))})
  do.call(rbind, temp)
  
  # page_data_1 <- train_1 %>% slice(i)
  # # predict_output <- suppressWarnings(predict_1(page_data_1))
  # suppressWarnings(predict_1(page_data_1))
  # # rbind.data.frame(predict_output)
})

registerDoSEQ()
stopCluster(cl)

loop_num <- 72
lap_size <- 2014

system.time(temp <- lapply((lap_size*loop_num + 1):(nrow(train_1)), function(x) {predict_2(slice(train_1, x))}))
system.time(temp <- do.call(rbind, temp))
submission_test <- rbind(submission_test, temp)

# Save predictions

saveRDS(submission_test, "output/submission_test_2.rds")

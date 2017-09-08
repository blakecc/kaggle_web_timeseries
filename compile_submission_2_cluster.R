# Load libraries

# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(doMPI)

#  Create predictions

rm(list = ls())

train_1 <- fread("data/train_1.csv", header = T, encoding = "UTF-8")

source("R/helpers.R")

cl <- startMPIcluster()
registerDoMPI(cl)

loop_num <- 72
lap_size <- floor(nrow(train_1) / loop_num)

submission_test <- foreach(i=1:loop_num, .combine=rbind, .packages = c("dplyr", "tidyr", "stringr", "lubridate", "data.table")) %dopar% {
  
  temp <- lapply((lap_size*(i-1) + 1):(lap_size*i), function(x) {predict_1(slice(train_1, x))})
  do.call(rbind, temp)
  
  # page_data_1 <- train_1 %>% slice(i)
  # suppressWarnings(predict_1(page_data_1))
}

closeCluster(cl)


temp <- lapply((lap_size*loop_num + 1):(nrow(train_1)), function(x) {predict_1(slice(train_1, x))})
temp <- do.call(rbind, temp)
submission_test <- rbind(submission_test, temp)

# Save predictions

saveRDS(submission_test, "output/submission_test.rds")

mpi.quit()

# Load libraries

library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)

rm(list = ls())

# Load reference data

# source("R/helpers.R")

#####
# ref_data <- fread("data/train_2.csv")
# start_date <- which(colnames(ref_data) == "2017-01-01")
# end_date <- start_date + 60
# ref_data <- cbind(ref_data[,1],ref_data[,start_date:end_date])
# ref_data_tall <- ref_data %>%
#   gather(-Page, key = Date, value = Visits)
# 
# key_1 <- fread("data/key_1.csv", header = T, encoding = "UTF-8")
# key_sep <- key_1 %>%
#   separate(Page, c("Page", "Date"), sep = -11) %>%
#   mutate(Page = str_sub(Page, 1, -2))
# 
# rm(key_1)
# 
# ref_data_tall$Date <- as.character(ref_data_tall$Date)
# key_sep$Date <- as.character(key_sep$Date)
# ref_data_tall$Page <- as.character(ref_data_tall$Page)
# key_sep$Page <- as.character(key_sep$Page)
# 
# SMAPE_ref <- left_join(key_sep, ref_data_tall, by = c("Page", "Date")) %>%
#   select(Id, Visits) %>%
#   mutate(Visits = ifelse(is.na(Visits), 0, Visits)) %>%
#   mutate(Visits = as.integer(Visits)) %>%
#   mutate(Visits = ifelse(is.na(Visits), 0, Visits))
# 
# saveRDS(SMAPE_ref, "output/SMAPE_ref_round_1.rds")

#####

actuals <- readRDS("output/SMAPE_ref_round_1.rds")
predictions <- fread("output/170912_Predict1_Round1_key.csv")
full_df <- left_join(actuals, predictions, by = "Id")
rm(actuals)
rm(predictions)
colnames(full_df) <- c("Id", "ref_vis", "sub_vis")

smape <- function(act, pred) {
  sm <- abs(act - pred) / (act + pred)
  return(sum(sm, na.rm = T) * 100 / length(act))
}

smape(full_df$ref_vis, full_df$sub_vis)

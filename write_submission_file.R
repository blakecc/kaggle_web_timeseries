library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)

rm(list = ls())

model_output <- readRDS("output/submission_test.rds")


# model_output_wide <- model_output %>% spread(key = Date, value = Visits)


key_1 <- fread("data/key_1.csv", header = T, encoding = "UTF-8")
# sample_submission_1 <- fread("data/sample_submission_1.csv", header = T, encoding = "UTF-8")


key_sep <- key_1 %>%
  separate(Page, c("Page", "Date"), sep = -11) %>%
  mutate(Page = str_sub(Page, 1, -2))

rm(key_1)

model_output$Date <- as.character(model_output$Date)
key_sep$Date <- as.character(key_sep$Date)
model_output$Page <- as.character(model_output$Page)
key_sep$Page <- as.character(key_sep$Page)

test_submission <- left_join(model_output, key_sep, by = c("Page", "Date")) %>%
  select(Id, Visits) %>%
  mutate(Visits = as.integer(Visits))

write_csv(submission1, "output/170904_1_key.csv")



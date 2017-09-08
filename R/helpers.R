predict_1 <- function(page_data){
  ##########
  page_data <- page_data %>% gather(-Page, key = Date, value = Visits) %>% mutate(Date = as.Date(Date))
  
  ######
  # page_data$Visits
  #####
  
  pred_dates <- rep(page_data$Date[550], 60) + seq(from = 1, to = 60, by = 1)
  
  if(sum(is.na(page_data$Visits)) >= 550){
    pred_visits <- rep(0, length(pred_dates))
  } else if (sum(is.na(page_data$Visits)) >= 530){
    pred_visits <- rep(round(mean(page_data$Visits, na.rm = T),0), length(pred_dates))
  } else if (sum(is.na(page_data$Visits)) + sum(page_data$Visits == 0, na.rm = T) >= 530){
    pred_visits <- rep(round(mean(page_data$Visits, na.rm = T),0), length(pred_dates))
  } else if (sum(is.na(page_data$Visits[(length(page_data$Visits) - 10):length(page_data$Visits)])) >= 9){
    pred_visits <- rep(0, length(pred_dates))
  } else {
    ###########
    
    page_data$weekday <- as.factor(weekdays(page_data$Date))
    
    # 7 day MA
    ma_lag <- 7
    week_ma <- rep(NA, length(page_data$Visits))
    for (i in 1:ma_lag){
      week_ma <- cbind(week_ma, lag(page_data$Visits, i))
    }
    page_data$week_ma <- ceiling(rowMeans(week_ma, na.rm = T) + 1) #ensure no log error in lm function
    
    page_data <- page_data %>%
      mutate(weekday_var = Visits - week_ma)
    
    ### Lag 1 of visits
    page_data$Visits_lag1 <- lag(page_data$Visits, 1)
    
    ### Lag 61 of visits
    page_data$Visits_lag61 <- lag(page_data$Visits, 61)
    
    ### Bimonthly rate of change
    page_data$Bimonthly_rateOC <- ((page_data$Visits_lag1 + 1) / (page_data$Visits_lag61 + 1))^(1/(60/365)) - 1
    
    if (sum(is.na(page_data$Bimonthly_rateOC[(length(page_data$Bimonthly_rateOC) - 10):length(page_data$Bimonthly_rateOC)])) >= 10){
      page_data$Bimonthly_rateOC <- rep(0, length(page_data$Visits))
    }
    
    ### TODO: Use try approach
    weekday_model <- try(lm(weekday_var ~ weekday:I(Visits_lag1^3) + I(week_ma^2) + week_ma + log(week_ma) + Bimonthly_rateOC, data = page_data, na.action = na.omit), TRUE)
    # summary(weekday_model)
    
    if (class(weekday_model) == "try-error") {
      pred_visits_weekday_movements <- rep(0, 60)
    } else {
      pred_visits_weekday_movements <-
        try(ceiling(predict.lm(weekday_model,
                               newdata = data.frame(weekday = weekdays(pred_dates),
                                                    week_ma = rep(page_data$week_ma[nrow(page_data)], length(pred_dates)),
                                                    Visits_lag1 = rep(page_data$Visits_lag1[nrow(page_data)], length(pred_dates)),
                                                    Bimonthly_rateOC = rep(page_data$Bimonthly_rateOC[nrow(page_data)], length(pred_dates))))), TRUE)
      
      if (class(pred_visits_weekday_movements) == "try-error"){
        pred_visits_weekday_movements <- rep(0, 60)
      } else if (is.na(mean(mean(pred_visits_weekday_movements) / range(pred_visits_weekday_movements)))) {
        pred_visits_weekday_movements <- pred_visits_weekday_movements
      } else if (mean(mean(pred_visits_weekday_movements) / range(pred_visits_weekday_movements)) >= 0.5 ){
        pred_visits_weekday_movements <- ceiling(pred_visits_weekday_movements - mean(pred_visits_weekday_movements))
      }
    }
    
    ###############
    
    ma_60 <- page_data %>%
      filter(Date >= page_data$Date[nrow(page_data) - 60]) %>%
      select(Visits) %>%
      # filter(rank(Visits) <= 20) %>%
      summarise(Mean = median(Visits, na.rm = T))
    ma_60 <- ceiling(ma_60$Mean)
    pred_visits_trend <- rep(ma_60, 60)
    
    #########
    
    ## final value
    last_observed_value <- page_data %>% select(Visits, Date) %>% filter(!is.na(Date), !is.na(Visits)) %>% arrange(desc(Date)) %>% slice(1) %>% .$Visits
    
    ## predict trend correction
    correction_length <- min(max(ceiling(sd(page_data$Visits[(length(page_data$Visits) - 60):length(page_data$Visits)], na.rm = T) / (last_observed_value + 1)),3),30)
    pred_correction <- numeric(correction_length)
    pred_correction[1] <- (last_observed_value - pred_visits_trend[1]) / 2
    for (i in 2:correction_length){
      last_observed_value <- pred_visits_trend[i-1] + pred_correction[i-1]   
      pred_correction[i] <- (last_observed_value - pred_visits_trend[i]) / 2
    }
    
    #########
    
    ## add correction to trend
    pred_visits <- numeric(60)
    pred_visits <- pred_visits_trend[1:length(pred_visits_trend)]
    pred_visits[1:correction_length] <- pred_visits_trend[1:correction_length] + pred_correction
    pred_visits <- pmax(ceiling(rowSums(cbind(pred_visits, pred_visits_weekday_movements))), rep(0, length(pred_visits)))
  }
  
  # add predictions to data
  page_predictions <- data.frame(Page = rep(page_data$Page[1], 60),
                                 Date = pred_dates,
                                 Visits = pred_visits)
  
  # page_combined <- rbind(page_data %>% select(Page, Date, Visits), page_predictions)
  return(page_predictions)
}
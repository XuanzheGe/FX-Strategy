rm(list = ls())

library(foreign)
library(dplyr)
library(readr)
library(tseries)
library(vars)
library(forecast)
library(gdata)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(stargazer)
library(readxl)
library(xts)
library(ggplot2)

# Import the dataset
FXspot <- read_excel("Data/spot.xlsx")
FXspot_bid <- read_excel("Data/spot_bid.xlsx")
FXspot_ask <- read_excel("Data/spot_ask.xlsx")
FX1M_bid <- read_excel("Data/1m_bid.xlsx")
FX1M_ask <- read_excel("Data/1m_ask.xlsx")
FXspot <- FXspot[,-1]
FXspot_bid <- FXspot_bid[,-1]
FXspot_ask <- FXspot_ask[,-1]
FX1M_bid <- FX1M_bid[,-1]
FX1M_ask <- FX1M_ask[,-1]


FX1M_bid_nodate <- FX1M_bid[, -1]
FX1M_ask_nodate <- FX1M_ask[, -1]
FXspot_nodate <- FXspot[, -1]
FXspotbid_nodate <- FXspot_bid[, -1]
FXspotask_nodate <- FXspot_ask[, -1]

# take out 1M Forward of INR, PHP, KRW, because they are already forward exchange rates not points
#INR1M_Bid <- FX1M_bid_nodate["INR 1M Forward Bid"]
PHP1M_Bid <- FX1M_bid_nodate["PHP 1M Forward Bid"]
KRW1M_Bid <- FX1M_bid_nodate["KRW 1M Forward Bid"]
#INR1M_Ask <- FX1M_ask_nodate["INR 1M Forward Ask"]
PHP1M_Ask <- FX1M_ask_nodate["PHP 1M Forward Ask"]
KRW1M_Ask <- FX1M_ask_nodate["KRW 1M Forward Ask"]


# 1M forward
FX1M_bid_nodate <- FX1M_bid_nodate / 10000
FX1M_ask_nodate <- FX1M_ask_nodate / 10000

FX1M_bid_nodate$`JPY 1M Forward Bid` <- FX1M_bid_nodate$`JPY 1M Forward Bid` * 100
FX1M_ask_nodate$`JPY 1M Forward Ask` <- FX1M_ask_nodate$`JPY 1M Forward Ask` * 100

FX1M_bid_price <- FX1M_bid_nodate + FXspotbid_nodate
FX1M_ask_price <- FX1M_ask_nodate + FXspotask_nodate

# put INR, PHP, KRW back
#FX1M_bid_price["INR 1M Forward Bid"] <- INR1M_Bid
#FX1M_ask_price["INR 1M Forward Ask"] <- INR1M_Ask
FX1M_bid_price["PHP 1M Forward Bid"] <- PHP1M_Bid
FX1M_ask_price["PHP 1M Forward Ask"] <- PHP1M_Ask
FX1M_bid_price["KRW 1M Forward Bid"] <- KRW1M_Bid
FX1M_ask_price["KRW 1M Forward Ask"] <- KRW1M_Ask
# Add dates
FX1M_bid_price$Date <- FXspot$Date
FX1M_ask_price$Date <- FXspot$Date

# Shift dates to the first
FX1M_bid_price <- FX1M_bid_price[, c(ncol(FX1M_bid_price), 1:(ncol(FX1M_bid_price) - 1))]
FX1M_ask_price <- FX1M_ask_price[, c(ncol(FX1M_ask_price), 1:(ncol(FX1M_ask_price) - 1))]

#Create Time-Series
FX1M_bid_price_ts <- xts(FX1M_bid_price[, - 1], order.by = FX1M_bid_price$Date)
FX1M_ask_price_ts <- xts(FX1M_ask_price[, - 1], order.by = FX1M_ask_price$Date)
FXspot_ts <- xts(FXspot[, - 1], order.by = FXspot$Date)
FXspot_bid_ts <- xts(FXspot_bid[, - 1], order.by = FXspot_bid$Date)
FXspot_ask_ts <- xts(FXspot_ask[, - 1], order.by = FXspot_ask$Date)

#Function for last day of the month:
last_make_month <- function(v_ts) {
  data_1 <- as.data.frame(v_ts)
  data_1 <- data_1 %>%
    mutate(Date = as.Date(index(v_ts)))

  data_2 <- data_1 %>%
    mutate(month = month(Date), #mutating for weeks as well, grouping my week, month year gives last day of the weeks (it should, haven't tried it yet)
           year = year(Date)) %>%
    group_by(month, year) %>%
    filter(Date == max(Date))   #max date gives last day

  v_ts <- xts(data_2, order.by = data_2$Date)
  v_ts
}

#Function for first day
first_make_month <- function(v_ts) {
  data_1 <- as.data.frame(v_ts)
  data_1 <- data_1 %>%
    mutate(Date = as.Date(index(v_ts)))
  data_2 <- data_1 %>%
    mutate(month = month(Date), #mutating for weeks as well, grouping my week, month year gives last day of the weeks (it should, haven't tried it yet)
           year = year(Date)) %>%
    group_by(month, year) %>%
    filter(Date == min(Date))   #min date gives first day
  v_ts <- xts(data_2, order.by = data_2$Date)
  v_ts
}
#Get last day of month
FX1M_bid_price_ts_last <- last_make_month(FX1M_bid_price_ts)
FX1M_ask_price_ts_last <- last_make_month(FX1M_ask_price_ts)
FXspot_ts_last <- last_make_month(FXspot_ts)
FXspot_bid_ts_last <- last_make_month(FXspot_bid_ts)
FXspot_ask_ts_last <- last_make_month(FXspot_ask_ts)

#Get first day of month
FX1M_bid_price_ts_first <- first_make_month(FX1M_bid_price_ts)
FX1M_ask_price_ts_first <- first_make_month(FX1M_ask_price_ts)
FXspot_ts_first <- first_make_month(FXspot_ts)
FXspot_bid_ts_first <- first_make_month(FXspot_bid_ts)
FXspot_ask_ts_first <- first_make_month(FXspot_ask_ts)

#Monthly Return Function for TS
#fwd_sign and spt_sign for long is -1 and 1, for short is 1 and -1 -> makes sure we add the markup properly
FXReturn <- function(Spot_dta, Forward_dta, fwdTC_bps, sptTC_bps, tenureTC, fwd_sign, spt_sign) {
  Tenure_multiplier <- tenureTC / 12
  SpotTC <- sptTC_bps / 10000
  ForwardTC <- fwdTC_bps / 10000

  Spot <- Spot_dta
  Spot_date <- index(Spot)                      #Creating Date index to use it as EOM Return
  Spot <- coredata(Spot)
  Spot <- data.frame(Spot)
  Spot <- Spot[, 1:(length(Spot) - 3)] # last three columns are date, month and year from TS function
  
  Forward <- Forward_dta
  Forward <- coredata(Forward)
  Forward <- data.frame(Forward)
  Forward <- Forward[, 1 :(length(Forward) - 3)]

  i <- c(seq_len(length(Spot)))
  Spot[, i] <- apply(Spot[, i], 2,             # Specify own function within apply to change full Data Frame to Numeric
                      function(x) as.numeric(as.character(x)))
  ii <- c(seq_len(length(Forward)))
  Forward[, ii] <- apply(Forward[, ii], 2,       # Specify own function within apply to change full Data Frame to Numeric; before was only i in bracets not ii
                          function(x) as.numeric(as.character(x)))

  Spot[is.na(Spot)] <- 0                          #necessary, in order to be able to calculate. Change back afterwards.
  Forward[is.na(Forward)] <- 0

  Spot <- Spot * (1 + ((SpotTC * Tenure_multiplier) * spt_sign))
  Forward <- Forward * (1 + (((ForwardTC * Tenure_multiplier) + (SpotTC * Tenure_multiplier)) * fwd_sign))
  Return <- Forward / Spot                        #Spot in t and Forward in t-30 or Spot EOM and Forward BOM



  Return <- do.call(data.frame,                      # Replace Inf, NaN in data by NA
                    lapply(Return,
                           function(x) replace(x, !is.finite(x), NA)))
  Return[Return == 0] <- NA
  Return <- xts(Return - 1, order.by = Spot_date)

}
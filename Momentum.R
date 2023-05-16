OneMReturnL <- FXReturn(FXspot_bid_ts_last, FX1M_ask_price_ts_first, 60, 10, 1, -1, 1)
OneMReturnL[is.na(OneMReturnL)] <- 0
OneMReturnS <- FXReturn(FXspot_ask_ts_last, FX1M_bid_price_ts_first, 60, 10, 1, 1, -1)
OneMReturnS[is.na(OneMReturnS)] <- 0
library(zoo)
#Functions:
# select three currencies with highest signals
# Function: select highest k signals
topk <- function(signal, K){
  top <- copy(signal) * 0

  for (i in 1 : nrow(signal)){
      indices <- WhichMaxK(coredata(signal[i]), K)
      for (j in 1:K){
          top[i, indices[j]] <- signal[i, indices[j]]
      }
  }

  return(top)
}

#Goal: reduce unecessary rebalancing
#Function: Stable Momentum signal with less trading frequency
stableMom <- function(signal, Return){
  deltaMomLong <- signal[seq_len(nmonths(signal)), seq_len(ncol(signal))]
  deltaMomLong <- data.frame(deltaMomLong)


  for (i in 2 : nrow(deltaMomLong)){
  for(j in 1 : ncol(deltaMomLong)){
    if (Return[i+1, j] > 0.01){ 
    if (deltaMomLong[i-1,j] != 0){
      deltaMomLong[i, j] <- deltaMomLong[i-1, j]
      }
      }
    }
  }

  #explain: if the new signal is postive and the difference between new and old signal is smaller than 50%, then we keep the old one.
  for (i in 1 : (nrow(deltaMomLong) - 1)){ 
    for (j in 1 : ncol(deltaMomLong)){
      if(deltaMomLong[i,j] != 0){
      if (abs((deltaMomLong[i+1,j] - deltaMomLong[i,j])/deltaMomLong[i,j]) < 0.5) {
        deltaMomLong[i+1, j] <- deltaMomLong[i,j]
      }
      }
    }
  }
  stableMom <- xts(deltaMomLong, order.by = index(Last1))

  stableMom <- stableMom / rowSums(stableMom)
  stableMom[is.nan(stableMom)] <- 0
  return(stableMom)
}

# Function: select highest values
WhichMinK <- function(x, k) {
  c <- sort(x, partial = k)[k]
  idx <- which(x <= c)
  return(idx[order(x[idx])])
}
WhichMaxK <- function(x, k) {
  n <- length(x)
  m <- n - k
  c <- sort(x, partial = m)[m]
  idx <- which(x > c)
  return(idx[order(x[idx], decreasing = T)])
}
MinK <- function(x, k) {
  return(x[WhichMinK(x, k)])
}
MaxK <- function(x, k) {
  return(x[WhichMaxK(x, k)])
}


#Data and results
XsReturn_Daily_Long_1M <- FX1M_ask_price[1 : (nrow(FX1M_ask_price) - 30), - 1] / FXspot_bid[31 : nrow(FXspot_bid), - 1] - 1
XsReturn_Daily_Long_1M[is.na(XsReturn_Daily_Long_1M)] <- 0
XsReturn_Daily_Long_1M[abs(XsReturn_Daily_Long_1M) > 0.1] <- 0
XsReturn_Daily_Long_1M <-  xts(XsReturn_Daily_Long_1M, order.by = FX1M_ask_price$Date[31 : nrow(FX1M_ask_price)])

#get first day list and last day list
First <- first_make_month(XsReturn_Daily_Long_1M)
First <- data.frame(First)
FirstIndex <- match(date(index(XsReturn_Daily_Long_1M)), as.Date(First$Date))
FirstIndex <- data.frame(FirstIndex)
FirstDay <- which(!is.na(FirstIndex)) #Find index of first day

Last1 <- last_make_month(XsReturn_Daily_Long_1M)
Last <- data.frame(Last1)
LastIndex <- match(date(index(XsReturn_Daily_Long_1M)), as.Date(Last$Date))
LastIndex <- data.frame(LastIndex)
LastDay <- which(!is.na(LastIndex)) #Find index of last day

# Long leg
#Create monthly signal with no outliers (only daily return lower than 10%)
# We long the foreign currency if its arithmetic mean of past 30 days return is positive.

MomWeightLong <- XsReturn_Daily_Long_1M[seq_len(nmonths(XsReturn_Daily_Long_1M)), seq_len(ncol(XsReturn_Daily_Long_1M))]
MomWeightLong <- xts(MomWeightLong, order.by = index(Last1))

for (i in 1 : nmonths(XsReturn_Daily_Long_1M)){
  for (j in seq_len(ncol(XsReturn_Daily_Long_1M))){
    if (mean(XsReturn_Daily_Long_1M[FirstDay[i]:LastDay[i], j]) > 0.015) {
      MomWeightLong[i, j] <- mean(XsReturn_Daily_Long_1M[FirstDay[i]:LastDay[i], j])
    }else {
      MomWeightLong[i, j] <- 0
    }
  }
}

MomWeightLong <- MomWeightLong / rowSums(MomWeightLong)
MomWeightLong[is.na(MomWeightLong)] <- 0

MomL <- rowSums(MomWeightLong[seq_len(nrow(MomWeightLong)), seq_len(ncol(MomWeightLong))] * OneMReturnL[2 : (nrow(OneMReturnL)), seq_len(ncol(OneMReturnL))])

top3long <- topk(MomWeightLong, 3)
top3long <- top3long / rowSums(top3long)
top3long[is.na(top3long)] <- 0
top3L <- rowSums(top3long[seq_len(nrow(top3long))] * OneMReturnL[2 : nrow(OneMReturnL)])


# Short leg
XsReturn_Daily_Short_1M <- 1 - FX1M_bid_price[1 : (nrow(FX1M_bid_price) - 30), - 1] / FXspot_ask[31 : nrow(FXspot_ask), - 1]
XsReturn_Daily_Short_1M[is.na(XsReturn_Daily_Short_1M)] <- 0
XsReturn_Daily_Short_1M[abs(XsReturn_Daily_Short_1M) > 0.1] <- 0
XsReturn_Daily_Short_1M <-  xts(XsReturn_Daily_Short_1M, order.by = FX1M_bid_price$Date[31 : nrow(FX1M_bid_price)])

MomWeightShort <- XsReturn_Daily_Short_1M[seq_len(nmonths(XsReturn_Daily_Short_1M)), seq_len(ncol(XsReturn_Daily_Short_1M))]
MomWeightShort <- xts(MomWeightShort, order.by = index(Last1))

#threshold would fit here:
for (i in 1 : nmonths(XsReturn_Daily_Short_1M)){
  for (j in seq_len(ncol(XsReturn_Daily_Short_1M))){
    if (mean(XsReturn_Daily_Short_1M[FirstDay[i]:LastDay[i], j]) > 0.015) {
      MomWeightShort[i, j] <- mean(XsReturn_Daily_Short_1M[FirstDay[i]:LastDay[i], j])
    }else {
      MomWeightShort[i, j] <- 0
    }
  }
}

MomWeightShort <- MomWeightShort / rowSums(MomWeightShort)
MomWeightShort[is.na(MomWeightShort)] <- 0

MomS <- rowSums(MomWeightShort[seq_len(nrow(MomWeightShort) ), seq_len(ncol(MomWeightShort))] * ( - OneMReturnS[2 : (nrow(OneMReturnS)), seq_len(ncol(OneMReturnS))]))

top3short <- topk(MomWeightShort, 3)
top3short <- top3short / rowSums(top3short)
top3short[is.na(top3short)] <- 0
top3S <- rowSums(top3short[seq_len(nrow(top3short))] * (- OneMReturnS[2 : (nrow(OneMReturnS))]))

MomLS = (MomL + MomS) / 2
top3LS <- (top3L + top3S) / 2


describe(top3L * 100)
describe(MomL*100)

describe(top3S * 100)
describe(MomS*100)

describe((top3S + top3L) / 2 * 100)
describe((MomS+MomL)/2 *100)
 
rm(list = ls())
source("FX Strategy Termpaper/0. Execution File/Source.R")

#Momentum Long
Final1_Mom_Long <- top3long[nrow(top3long)] / 2
Final1_Mom_Short <- top3short[nrow(top3short)] / 2
show(Final1_Mom_Long)
show(Final1_Mom_Short)

#Check:
show(rowSums(Final1_Mom_Long))
show(rowSums(Final1_Mom_Short))

#write.xlsx(Final1_Mom_Long, file = "long.xlsx")
#write.xlsx(Final1_Mom_Short, file = "short.xlsx")

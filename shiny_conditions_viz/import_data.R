library(tidyverse)
library(lubridate)
library(readxl)

full_bridge_df_raw <- read.csv("data/tblBridgeArchiveStudySetFinal9.21.17_historicalNBI.csv", stringsAsFactors=FALSE)

full_bridge_df_raw <- full_bridge_df_raw %>%
  mutate(
    BDEPT = ifelse(is.na(BDEPT), "NA", BDEPT)
  )
save(full_bridge_df_raw, file="data/full_bridge_df_raw.rdata")
rm(full_bridge_df_raw)

bridge_spending_df_raw <- read_excel("data/tblBridgeSpending_full.xlsx")

bridge_spending_df_raw <- bridge_spending_df_raw %>%
  mutate(
    BridgeNumbers = ifelse(is.na(BridgeNumbers), "NA", BridgeNumbers)
  )

## Need to process "bridge_spending_df" b/c BridgeNumbers often contains multiple BDEPT numbers
bridge_spending_rows <- rbind()
for(row in seq(nrow(bridge_spending_df_raw))){
  bridge_nos <- bridge_spending_df_raw$BridgeNumbers[row]
  bridge_nos_list <- unlist(strsplit(bridge_nos, ","))
  num_bridges_listed <- length(bridge_nos_list)
  
  print(paste0("new bridge list: ", bridge_nos_list))
  print(paste0("Number of bridges listed: ", num_bridges_listed))
  print(paste0("there are now this many rows: ", nrow(bridge_spending_rows)))
  
  bridge_spending_rows <- bridge_spending_rows %>%
    rbind(
      cbind(bridge_spending_df_raw[row,], bridgeID = bridge_nos_list, numBridgesFunded = num_bridges_listed)
    )
}
bridge_spending_df <- as.data.frame(bridge_spending_rows)
bridge_spending_df <- bridge_spending_df %>%
  mutate(
    spending_amt = Posting_Line_Amount / numBridgesFunded #average funding when multiple bridges are in the same obsevation
  )
save(bridge_spending_df, file="data/bridge_spending_df.rdata")
rm(bridge_spending_rows)

test <- bridge_spending_df %>%
  dplyr::select(BridgeNumbers, bridgeID, numBridgesFunded, Posting_Line_Amount, spending_amt)

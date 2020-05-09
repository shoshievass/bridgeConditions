library(tidyverse)
library(lubridate)
library(readxl)


bridge_spending_df_raw <- read_excel("raw_data/tblBridgeSpending_full.xlsx")

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

bridge_spending_by_project_and_bridge <- bridge_spending_df %>%
  arrange(bridgeID,PROJECT_NO,projectYearStart) %>%
  group_by(PROJECT_NO, bridgeID) %>%
  summarize(
    contract_no = first(CONTRACT_NO)
    ,BridgeNumbers = first(BridgeNumbers)
    ,contract_type = first(CONTRACT_TYPE_NAME)
    ,project_category = first(projectCategory)
    ,project_type = first(projectType)
    ,total_line_spending = sum(Posting_Line_Amount)
    ,bridge_spending = sum(spending_amt)
    ,calendar_year = first(CalendarYear)
    ,start_year = first(projectYearStart)
    ,end_year = last(projectYearEnd)
  )

write.table(bridge_spending_df, file="data/bridge_spending_clean.csv", sep=",")
write.table(bridge_spending_by_project_and_bridge, file="data/bridge_spending_by_proj.csv", sep=",")


bridge_spending_by_bridge_and_year <- bridge_spending_by_project_and_bridge %>%
  arrange(bridgeID, start_year) %>%
  group_by(bridgeID,start_year) %>%
  summarize(
    num_projects = n()
    ,total_bridge_spending = sum(bridge_spending)
    ,avg_bridge_spending = mean(bridge_spending)
    ,first_start_year = first(start_year)
    ,last_end_year = last(end_year)
    ,calendar_year = first(calendar_year)
    ,proj_no = first(PROJECT_NO)
  ) %>%
  mutate(
    data_year = first_start_year
  )

bridge_spending_by_bridge_and_year %>%
  dplyr::filter(bridgeID != "NA") %>%
  ggplot(aes(x=factor(data_year), y=avg_bridge_spending)) +
  geom_col(aes(binwidth = 1))

save(bridge_spending_by_bridge_and_year, file="data/bridge_spending_by_bridge_and_year.rdata")

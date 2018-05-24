library(tidyverse)
library(skimr)

# Load complete time series
load("data/bridge_timeseries_merged_by_proj.rdata")

bridge_info <- read_csv("data/bridge_info.csv")

bridge_info_sm <- bridge_info %>%
  select(
    Bridge_Department_Number,
    Latitude,
    Longitude,
    Structure_Type,
    Structure_Material,
    Structure_Length,
    Type_of_Service
  ) %>%
  mutate(
    bridgeID = Bridge_Department_Number
  ) %>%
  distinct() %>%
  group_by(bridgeID) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    Structure_Type = first(Structure_Type),
    Structure_Material = first(Structure_Material),
    Structure_Length = first(Structure_Length),
    Type_of_Service = first(Type_of_Service)
  )

bridgeIds <- unique(bridge_ts$bridgeID)
bridge_sample <- sample(bridgeIds, size = 1000, replace = F)

bridge_ts_sm <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample)

bridge_ts_sm <- bridge_ts_sm %>%
  select(
    bridgeID,
    deck,
    superstructure,
    substructure,
    spending,
    data_year,
    year_built,
    year_rebuilt,
    town
  ) %>%
  mutate(
    no_record = ifelse(is.na(deck) & is.na(superstructure) & is.na(substructure), TRUE , FALSE)
  ) %>%
  filter(!no_record)

bridge_score_timeseries <- bridge_ts_sm %>%
  left_join(bridge_info_sm)

write_csv(bridge_score_timeseries, "data/bridge_score_timeseries_sample.csv", na = "NA", col_names = T)

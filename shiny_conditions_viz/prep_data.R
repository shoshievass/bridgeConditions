library(tidyverse)
library(lubridate)

# Note: Both have BDEPT NA values coded as (the string) "NA"
load("data/bridge_spending_df.rdata") # loads spending dataframe w/ rows of bridgeNumbers split into one per row
load("data/full_bridge_df_raw.rdata") # loads bridge condition dataframe 

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

bridge_df <- full_bridge_df_raw %>%
  mutate(
    bridgeID = ifelse(is.na(BDEPT), "NA", BDEPT)
    ,bridgeKey = BRIDGE.KEY
    ,bridgeBin = BIN
    ,year_built = Year.Built
    ,year_rebuilt = pmax(Year.Rebuilt,Year.Built)
    ,data_year = dataCondYear
    ,total_health_index = HEALTHINDEX
    ,structurally_deficient = STRUCTDEF
    ,health_deck_raw = as.numeric(ITEM58)
    ,health_superstructure_raw = as.numeric(ITEM59)
    ,health_substructure_raw = as.numeric(ITEM60)
    ,health_culvert_raw = as.numeric(ITEM62)
    # ,health_deck = as.numeric( ifelse(ITEM58 %in% c("N"), 99, ITEM58) )
    # ,health_superstructure = as.numeric( ifelse(ITEM59 %in% c("N","?",""), 99, ITEM59) )
    # ,health_substructure = as.numeric( ifelse(ITEM60 %in% c("N","?",""), 99, ITEM60) )
    # ,health_culvert = as.numeric(ifelse(ITEM62 %in% c("N","?",""), 99, ITEM62) )
    ,national_highway_system = ITEM104
    ,area = areaM
    ,town = TOWN.NAME
  ) 


bridge_health <- bridge_df %>%
  dplyr::select(
    bridgeID
    ,data_year
    ,health_deck_raw
    ,health_superstructure_raw
    ,health_substructure_raw
    ,health_culvert_raw
  ) %>%
  arrange(bridgeID,data_year) %>%
  group_by(bridgeID,data_year) %>%
  mutate_all(
    funs(min,max)
  )


bridge_df_by_bridge_and_year <- bridge_df %>%
  group_by(bridgeID, data_year) %>%
  summarize(
    num_obs_in_year = n()
    ,year_built = first(year_built)
    ,year_rebuilt = first(year_rebuilt)
    ,total_health_index = max(total_health_index, na.rm =T)
    ,structurally_deficient = max(structurally_deficient), na.rm =T
    ,deck = max(health_deck_raw, na.rm =T)
    ,superstructure = max(health_superstructure_raw, na.rm =T)
    ,substructure = max(health_substructure_raw, na.rm =T)
    ,culvert = max(health_culvert_raw, na.rm =T)
    ,area = max(area)
    ,national_highway_system = max(national_highway_system)
    ,town = first(town)
  ) %>%
  mutate(
    total_health_index = ifelse((total_health_index == -Inf), NA, total_health_index)
    ,structurally_deficient = ifelse((structurally_deficient == -Inf), NA, structurally_deficient)
    ,deck = ifelse((deck == -Inf), NA, deck)
    ,superstructure = ifelse((superstructure == -Inf), NA, superstructure)
    ,substructure = ifelse((substructure == -Inf), NA, substructure)
    ,culvert = ifelse((culvert == -Inf), NA, culvert)
    ,min_health = pmin(deck, superstructure,substructure, culvert, na.rm=T)
    ,flag = ifelse(min_health < 5, 1, 0)
  ) %>%
  arrange(bridgeID, data_year) 

# test <- bridge_df_by_bridge_and_year %>%
# filter(is.na(.))

bridge_df_by_bridge_and_year %>%
  summary()

levels(factor(bridge_df_by_bridge_and_year$num_obs_in_year))

# Test whether the condition for "structural deficiency" holds in the data (it does almost always)
bridge_df_by_bridge_and_year %>%
  ggplot(aes(x = (flag == structurally_deficient), fill=factor(flag)) ) +
  geom_histogram(position="dodge", stat="count") + 
  facet_wrap(~data_year)

# Histogram of different health measures by bridge part
bridge_df_by_bridge_and_year %>%
  gather(
    key = structure,
    value = health,
    -bridgeID,
    -data_year,
    -structurally_deficient,
    -total_health_index,
    -year_built,
    -year_rebuilt,
    -flag,
    -num_obs,
    -area,
    -national_highway_system,
    -town
  ) %>%
  ggplot( aes(x=factor(structurally_deficient), fill=factor(health)) ) + 
  facet_grid(~factor(structure)) +
  geom_histogram(position="dodge", binwidth = 1, stat="count")

summary_bridge_df_by_bridge_and_year <- bridge_df_by_bridge_and_year %>%
  group_by(bridgeID) %>%
  mutate(
    num_obs = n(),
    first_year_obs = min(data_year),
    last_year_obs = max(data_year)
  ) %>%
  summarize_at(
    vars(
      num_obs
      ,deck
     ,superstructure
     ,substructure
     ,culvert
    ),
    funs(
      min = min,
      max = max,
      median = median,
      mean = mean,
      sd = sd
    )
  )

padded_bridge_times <- expand.grid(bridgeID = unique(bridge_df_by_bridge_and_year$bridgeID), data_year = unique(bridge_df_by_bridge_and_year$data_year))
bridge_timeseries_padded <- padded_bridge_times %>%
  left_join(bridge_df_by_bridge_and_year)


bridge_condition_timeseries <- bridge_timeseries_padded %>%
  arrange(bridgeID, data_year) %>%
  mutate_at(
    vars(
      deck
      ,superstructure
      ,substructure
      ,culvert
      ,min_health
      ,data_year
    ),
    funs(
      lag,
      lead
    )
  ) %>%
  mutate(
    deck_lag = ifelse(is.na(deck_lag), deck, deck_lag),
    superstructure_lag = ifelse(is.na(superstructure_lag), superstructure, superstructure_lag),
    substructure_lag = ifelse(is.na(substructure_lag), substructure, substructure_lag),
    culvert_lag = ifelse(is.na(culvert_lag), culvert, culvert_lag),
    min_health_lag = ifelse(is.na(min_health_lag), min_health, min_health_lag),
    # 
    time_diff = data_year - data_year_lag,
    deck_diff = deck - deck_lag,
    superstructure_diff = superstructure - superstructure_lag,
    substructure_diff = substructure - substructure_lag,
    culvert_diff = culvert - culvert_lag,
    min_health_diff = min_health - min_health_lag
  ) 

bridge_condition_diffs <-bridge_condition_timeseries %>%
  dplyr::select(
    bridgeID,
    data_year,
    structurally_deficient,
    ends_with("diff")
  )

# table(bridge_condition_diffs$time_diff, bridge_condition_diffs$data_year)

gathered_bridge_condition_timeseries <- bridge_timeseries_padded %>%
  dplyr::select(
    bridgeID,
    data_year,
    deck,
    superstructure,
    substructure,
    culvert,
    min_health
  ) %>%
  gather(
    key = structure,
    value = health,
    -bridgeID,
    -data_year
  )

gathered_bridge_diff_timeseries <- bridge_condition_diffs %>%
  gather(
    key = structure,
    value = health_diff,
    -bridgeID,
    -data_year,
    -structurally_deficient,
    -time_diff
  )



# gathered_bridge_condition_timeseries %>%
#   ggplot( aes(x=factor(structurally_deficient), fill=factor(health_diff)) ) + 
#   facet_grid(~factor(structure)) +
#   geom_histogram(position="dodge", binwidth = 1, stat="count")

gathered_bridge_condition_timeseries %>%
  ggplot( aes(x=data_year, y=health, group=factor(bridgeID),color=factor(bridgeID) ) ) + 
  geom_line() +
  facet_grid(~factor(structure)) + theme(legend.position="none")

gathered_bridge_diff_timeseries %>%
  ggplot( aes(x=data_year, y=health_diff, color=factor(bridgeID) ) ) +
  geom_line() +
  facet_grid(~factor(structure)) + theme(legend.position="none")

# bridge_timeseries <- ts(bridge_timeseries_padded)

bridge_ts <- bridge_timeseries_padded %>%
  left_join(bridge_spending_by_bridge_and_year, by=c("bridgeID","data_year")) %>%
  arrange(bridgeID, data_year)

bridge_ts <- bridge_ts %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    spending = ifelse(is.na(total_bridge_spending), 0, total_bridge_spending ),
    spending_in_year = ifelse( spending > 0, calendar_year, NA),
    project_end_year = ifelse(spending > 0, last_end_year, NA),
    project_init_year = ifelse(spending > 0, first_start_year, NA)
  )

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

bridge_ts <- bridge_ts %>%
  left_join(bridge_info_sm)

write_csv(bridge_ts, "data/databridge_timeseries.csv", na = "NA", col_names = T)

# write.table(bridge_ts, file="data/databridge_timeseries.csv", sep=",")

save(bridge_ts, file="data/bridge_timeseries_merged.rdata")

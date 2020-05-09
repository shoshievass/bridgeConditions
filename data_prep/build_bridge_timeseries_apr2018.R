library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(stringr)

full_bridge_df_raw <- read_csv("raw_data/tblNbiMaHistorical1.19.18.csv") %>% clean_names()
load("data/bridge_spending_by_bridge_and_year.rdata") # loads spending dataframe w/ rows of bridgeNumbers split into one per row

full_bridge_df_sm <- full_bridge_df_raw %>%
  select(
    bdept, #bridge identifier used
    bin,
    cndtn, # overall condition score
    cndtn_yr, # year of data
    area_sf, #surface area (float)
    lat_016, #latitude
    long_017, #longitude
    year_built_027, #year built (int)
    year_reconstructed_106, #year rebuilt
    adt_029, # average daily traffic (historic measurement - take a look at this) (float)
    future_adt_114, #projected future average daily traffic (float)
    percent_adt_truck_109, # percent of daily traffic from trucks (float)
    # inspect_freq_months_091, #frequency of inspection - use this to evaluate weird obs?
    design_load_031, # type of traffic the bridge was designed for (factor)
    appr_width_mt_032, #width of usable roadway (Float)
    roadway_width_mt_051, #curb to curb width of the bridge (float)
    structure_len_mt_049, #width of structure (float)
    traffic_lanes_on_028a, # number of traffic lantes
    degrees_skew_034, # degree of skew (int)
    open_closed_posted_041, # open/closed/etc. status of the bridge (factor)
    structure_kind_043a, # structure material (factor)
    structure_type_043b, # structure design type (factor)
    deck_cond_058,
    superstructure_cond_059,
    substructure_cond_060
    ) %>%
  group_by(bdept) %>%
  mutate(
    year_built_latest = pmax(year_built_027, year_reconstructed_106, na.rm=T),
    year_built_latest = min(year_built_latest, na.rm=T),
    design_load_031 = as.factor(design_load_031),
    open_closed_posted_041 = as.factor(open_closed_posted_041),
    structure_kind_043a = as.factor(structure_kind_043a),
    structure_type_043b = as.factor(structure_type_043b)
    ) %>%
  ungroup() %>%
  rename(
    bridgeID = bdept,
    data_year = cndtn_yr,
    deck = deck_cond_058,
    superstructure = superstructure_cond_059,
    substructure = substructure_cond_060
    ) %>%
  mutate(
    age = data_year - year_built_latest,
    deck = ifelse(deck == 0, NA, deck),
    superstructure = ifelse(superstructure == 0, NA, superstructure),
    substructure = ifelse(substructure == 0, NA, substructure),
    no_inspection = ifelse(is.na(deck) & is.na(superstructure) & is.na(substructure), 1, 0)
  ) %>%
  select(-year_built_027, -year_reconstructed_106) %>%
  filter( no_inspection == 0)

getMaxAdjusted <- function(vec){
  out = suppressWarnings( max(vec, na.rm=T) )
  if(is.infinite(out)){
    return(NA)
  }
  else{
    return(out)
  }
}

interpolateMissingValsWLastSeen <- function(vec){
  if(is.na(vec[1])){vec[1] = vec[2]}
  out <- zoo::na.locf(vec, na.rm = FALSE)
  return(out)
}

bridge_df_by_bridge_and_year <- full_bridge_df_sm %>%
  group_by(bridgeID, data_year) %>%
  summarize_all(funs(getMaxAdjusted(.))) %>%
  arrange(bridgeID, data_year) %>%
  ungroup()

bridge_df_by_bridge_and_year <- bridge_df_by_bridge_and_year %>%
  arrange(bridgeID, data_year) %>%
  ungroup() %>%
  group_by(bridgeID) %>%
  mutate_at(vars(-deck,-superstructure,-substructure),funs(interpolateMissingValsWLastSeen(.))) %>%
  mutate(
    num_obs = n(),
    percent_adt_truck_109 = ifelse(is.na(percent_adt_truck_109), max(percent_adt_truck_109, na.rm = T), percent_adt_truck_109), ## fill in missing gaps w/ overall max
    design_load_031 = ifelse(is.na(design_load_031), max(design_load_031, na.rm =T), design_load_031) ## fill in missing gaps w/ overall max
  ) %>%
  ungroup() %>%
  mutate(
    deck = ifelse(deck == 0 | is.na(deck), -999, deck),
    superstructure = ifelse(superstructure == 0 | is.na(superstructure), -999, superstructure),
    substructure = ifelse(substructure == 0 | is.na(substructure), -999, substructure)
  ) %>%
  filter(num_obs > 1)

## Test NAs - looks good!
bridge_df_by_bridge_and_year %>% apply(2,function(x) sum(is.na(x)))


# padded_bridge_times <- expand.grid(bridgeID = unique(bridge_df_by_bridge_and_year$bridgeID), data_year = unique(bridge_df_by_bridge_and_year$data_year))
# bridge_timeseries_padded <- padded_bridge_times %>%
#   left_join(bridge_df_by_bridge_and_year) %>%
#   arrange(bridgeID, data_year)

bridge_ts <- bridge_df_by_bridge_and_year %>% ## Note: This no longer has every year represented
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

write_csv(bridge_ts, "data/databridge_timeseries.csv", na = "NA", col_names = T)
save(bridge_ts, file="data/bridge_timeseries_merged_apr2018_wlats_longs.rdata")

bridge_ts <- bridge_ts %>%
  select(-lat_016, -long_017)
save(bridge_ts, file="data/bridge_timeseries_merged_apr2018.rdata")

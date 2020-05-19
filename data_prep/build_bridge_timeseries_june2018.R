## ---------------------------
##
## Script name: Build Bridge Time Series, June 2018
##
## Purpose of script: Create time series data for analysis
##
## Author: Shoshana Vasserman
##
## Date Created: June, 2018
##
## Revised by: Noah Jussila
##
## Dated Revised: May, 2020
##
## Email:
##
## Input: tblNbiMaHistorical1.19.18.csv
##        bridge_spending_by_bridge_and_year.rdata
##
## Output: data/bridge_timeseries_june4.rdata
##
## ------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(stringr)

#####################
#PRELIMINARY CLEANING
#####################

full_bridge_df_raw <- read_csv("raw_data/tblNbiMaHistorical1.19.18.csv") %>% clean_names()
load("clean_data/bridge_spending_by_bridge_and_year.rdata") # loads spending dataframe w/ rows of bridgeNumbers split into one per row

full_bridge_df_sm <- full_bridge_df_raw %>%
  select(
    bdept, #bridge identifier used
    bin,
    cndtn, # overall condition score
    cndtn_yr, # year of data
    area_sf, #surface area (float)
    # lat_016, #latitude
    # long_017, #longitude
    year_built_027, #year built (int)
    year_reconstructed_106, #year rebuilt
    adt_029, # average daily traffic (historic measurement - take a look at this) (float)
    future_adt_114, #projected future average daily traffic (float)
    percent_adt_truck_109, # percent of daily traffic from trucks (float)
    # inspect_freq_months_091, #frequency of inspection - use this to evaluate weird obs?
    structure_len_mt_049, # length of structure in meters (float)
    # design_load_031, # type of traffic the bridge was designed for (factor)
    appr_width_mt_032, #width of usable roadway leading up to the bridge (float)
    median_code_033, # is there a median dividing traffic (int that should be treated as a factor)
    roadway_width_mt_051, #curb to curb width of the bridge (float)
    structure_len_mt_049, #width of structure (float)
    traffic_lanes_on_028a, # number of traffic lantes
    degrees_skew_034, # degree of skew (int)
    structure_flared_035, # does the width of the bridge vary a lot (binary - 0: no; 1: yes)
    open_closed_posted_041, # open/closed/etc. status of the bridge (factor)
    structure_kind_043a, # structure material (factor)
    structure_type_043b, # structure design type (factor)
    appr_rail_036c, # is there an approach guardrail? (binary 0/1)
    appr_rail_end_036d, # is there an abrupt end of the approach guardrail? (binary 0/1)
    railings_036a, # are there bridge railings? (binary 0/1)
    left_curb_mt_050a, # width of left curb of sidewalk
    right_curb_mt_050b, # width of right curb of sidewalk
    deck_cond_058,
    superstructure_cond_059,
    substructure_cond_060,
    design_load_031
  ) %>%
  group_by(bdept) %>%
  mutate(
    year_built_latest = pmax(year_built_027, year_reconstructed_106, na.rm=T),
    year_built_latest = min(year_built_latest, na.rm=T),
    design_load_031 = as.factor(design_load_031),
    open_closed_posted_041 = as.factor(open_closed_posted_041),
    structure_kind_043a = as.factor(structure_kind_043a),
    structure_type_043b = as.factor(structure_type_043b),
    median_code_033 = as.factor(median_code_033)
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
  filter( no_inspection == 0) %>%
  select(-no_inspection)

#####################################################
#COLLAPSE DATA SO EACH OBSERVATION IS ONE BRIDGE-YEAR
#####################################################

#Multiple observations can correspond to the same bridge-year
full_bridge_df_sm %>%
  group_by(bridgeID, data_year) %>%
  add_tally %>%
  filter(n>1)

#We want to collapse the data s.t each bridge-year is represented by a single observation
#How do we determine the value taken on by each variable -- take the max!
getMaxAdjusted <- function(vec){
  out = suppressWarnings( max(vec, na.rm=T) )
  if(is.infinite(out)){
    return(NA)
  }
  else{
    return(out)
  }
}

bridge_df_by_bridge_and_year <- full_bridge_df_sm %>%
  group_by(bridgeID, data_year) %>%
  summarize_all(list(~getMaxAdjusted(.))) %>%
  arrange(bridgeID, data_year) %>%
  ungroup()

##############################################################
#GET RID OF MISSING VALUES BY REPLACING NAs WITH LAST OBSERVED
##############################################################

#Function used to count missing values
checkNAs <- function(df){
  df %>% apply(2,function(x) sum(is.na(x)))
}

#How many missing values do we initially have?
checkNAs(full_bridge_df_sm)

interpolateMissingValsWLastSeen <- function(vec){
  if(is.na(vec[1])){vec[1] = vec[2]}
  out <- zoo::na.locf(vec, na.rm = FALSE)
  return(out)
}

bridge_df_by_bridge_and_year <- bridge_df_by_bridge_and_year %>%
  arrange(bridgeID, data_year) %>%
  ungroup() %>%
  mutate_at(vars(-deck,-superstructure,-substructure),list(~interpolateMissingValsWLastSeen(.))) %>%
  group_by(bridgeID) %>%
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

## Verify that we got rid of all the missing values
checkNAs(bridge_df_by_bridge_and_year)

#########################
#MERGE WITH SPENDING DATA
#########################


bridge_ts2 <- bridge_df_by_bridge_and_year %>% ## Note: This no longer has every year represented
  left_join(bridge_spending_by_bridge_and_year, by=c("bridgeID","data_year")) %>%
  arrange(bridgeID, data_year)

bridge_ts2 <- bridge_ts2 %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    spending = ifelse(is.na(total_bridge_spending), 0, total_bridge_spending ),
    spending_in_year = ifelse( spending > 0, calendar_year, NA),
    project_end_year = ifelse(spending > 0, last_end_year, NA),
    project_init_year = ifelse(spending > 0, first_start_year, NA)
  )

###########
#SAVE FILES
###########

write_csv(bridge_ts, "data/databridge_timeseries.csv", na = "NA", col_names = T)
save(bridge_ts, file="clean_data/bridge_timeseries_june4.rdata")


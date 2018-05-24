library(tidyverse)
library(skimr)
library(ggridges)


# Load complete time series
load("data/bridge_timeseries_merged_apr2018_wlats_longs.rdata")

# vectorStringConcat <- function(df, var){
#   vec <= df[vec]
#   out <- paste(vec, ",")
#   
#   df <- df %>%
#     mutate(
#       agg
#     )
#   return(out)
# }

bridges <- bridge_ts %>%
  select(
    bridgeID,
    area_sf,
    lat_016,
    long_017,
    data_year,
    age,
    spending_in_year,
    cndtn,
    deck,
    superstructure,
    substructure,
    age
    ) %>%
  mutate(
    spending_in_year = ifelse(is.na(spending_in_year), 0, spending_in_year)
  )

save(bridges, file="data/bridge_ts_for_crashdata.rdata")


# bridges <- bridges %>%
#   group_by(bridgeID) %>%
#   mutate(
#     agg_years = vectorStringConcat(spending_in_year),
#     agg_cntn = vectorStringConcat(cndtn)
#   )

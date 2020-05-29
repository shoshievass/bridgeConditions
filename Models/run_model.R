library(tidyverse)
library(rstan)
library(data.table)
databridge_timeseries <- read_csv("clean_data/databridge_timeseries.csv")

datalist <- list(
  N = nrow(databridge_timeseries),

  B = databridge_timeseries %>%
        count(bridgeID) %>%
        nrow,

  M = 0, #Run with no covariates to begin with

  H = 9,

  deck_health = databridge_timeseries$deck,

  superstructure_health = databridge_timeseries$superstructure,

  substructure_health = databridge_timeseries$substructure,

  last_observed_deck_index = (databridge_timeseries %>%
                                group_by(bridgeID) %>%
                                do(tail(., 1)))[,20],

  last_observed_superstructure_index = (databridge_timeseries %>%
                                          group_by(bridgeID) %>%
                                          do(tail(., 1)))[,21],

  last_observed_substructure_index = (databridge_timeseries %>%
                                        group_by(bridgeID) %>%
                                        do(tail(., 1)))[,22],

  spending = databridge_timeseries$spending,

  num_periods_lapsed = (databridge_timeseries %>%
                          group_by(bridgeID) %>%
                          mutate(data_year-lag(data_year)))[,39], #This will give NAs for the initial observations

  T_b = databridge_timeseries %>%
          group_by(bridgeID) %>%
          tally() %>%
          nrow,

  N_b = databridge_timeseries %>%
          group_by(bridgeID) %>%
          tally() %>%
          nrow,

  X = matrix(rep(0,nrow(databridge_timeseries))) #No covariates, so just use zero...?
)


setwd("~/GitHub/bridgeConditions/Models")
fit1 <- stan(
  file = "msm_bridge_decay_v8_msw.stan",
  data = datalist,
  chains = 1,
  iter = 2000
)

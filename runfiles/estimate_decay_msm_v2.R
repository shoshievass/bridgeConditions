library(tidyverse)
library(skimr)

# Load complete time series
load("data/bridge_timeseries_merged_by_proj.rdata")

bridgeIds <- unique(bridge_ts$bridgeID)
bridge_sample <- sample(bridgeIds, size = 400, replace = F)

bridge_ts_sm <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample)

bridge_ts_sm <- bridge_ts_sm %>%
  select(
    bridgeID,
    data_year,
    deck,
    superstructure,
    substructure,
    year_rebuilt,
    year_built,
    spending,
    Structure_Type,
    Structure_Material,
    Structure_Length,
    Type_of_Service
  ) %>%
  mutate( # get rid of zero measurements
    deck = ifelse(deck == 0 | is.na(deck), -999, deck),
    superstructure = ifelse(superstructure == 0 | is.na(superstructure), -999, superstructure),
    substructure = ifelse(substructure == 0 | is.na(substructure), -999, substructure),
  ) %>%
  group_by(bridgeID) %>%
  mutate(
    year_built_latest = cummax(ifelse(is.na(year_rebuilt), -Inf, year_rebuilt)),
    cum_spending = cumsum(spending)
  ) %>%
  ungroup() %>%
  filter(
    complete.cases(.)
  ) %>%
  select(
    -year_built,
    -year_rebuilt
  ) %>%
  mutate( # get rid of zero measurements
    deck = ifelse(deck == -999, NA, deck),
    superstructure = ifelse(superstructure == -999, NA, superstructure),
    substructure = ifelse(substructure == -999, NA, substructure),
  ) %>%
  arrange(bridgeID, data_year) %>%
  group_by(
    bridgeID
  ) %>%
  mutate(
    time_lag = lag(data_year)
  ) %>%
  mutate(
    time_laps = data_year - time_lag,
    age = pmax(data_year - year_built_latest, 0)
  ) %>%
  ungroup() %>%
  filter(
    complete.cases(.)
  ) %>%
  mutate(
    bridgeID_seq = as.numeric(factor(bridgeID)),
    periodID_seq = as.numeric(factor(data_year))
    )

bridge_ts_sm <- bridge_ts_sm %>%
  group_by(bridgeID, cum_spending) %>%
  mutate(
    periods_since_spending = cumsum(time_laps) - 1
  ) %>%
  ungroup()

t <- bridge_ts_sm %>% select(bridgeID, data_year, time_lag, time_laps, periods_since_spending, spending, cum_spending)

skim(bridge_ts_sm)

table(bridge_ts_sm$superstructure)
table(bridge_ts_sm$substructure)
table(bridge_ts_sm$deck)

T_b_df <- bridge_ts_sm %>%
  mutate(rn = row_number()) %>%
  group_by(bridgeID) %>%
  summarize(
    T_b = n(),
    n_b = first(rn)
  )

B <- length(unique(bridge_ts_sm$bridgeID))
T_max <- length(unique(bridge_ts_sm$data_year))

bridge_ts_sm <- bridge_ts_sm %>%
  left_join(T_b_df)

X_bridge_sm <- model.matrix(~ scale(age), data = bridge_ts_sm)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dgp_model <- stan_model("Models/msm_bridge_decay_v2.stan")

w <- 1e6 # for scaling spending

real_data_list <- list(
  N = nrow(bridge_ts_sm),
  B = length(unique(bridge_ts_sm$bridgeID)),
  T = length(unique(bridge_ts_sm$data_year)),
  M = ncol(X_bridge_sm),
  H = max(unique(bridge_ts_sm$deck)),
  bridge_ID = bridge_ts_sm$bridgeID_seq,
  period_ID = bridge_ts_sm$periodID_seq,
  deck_health = bridge_ts_sm$deck,
  superstructure_health = bridge_ts_sm$superstructure,
  substructure_health = bridge_ts_sm$substructure,
  has_observation = rep(1, nrow(bridge_ts_sm)),
  spending = (bridge_ts_sm$spending / w),
  num_periods_since_spending = bridge_ts_sm$periods_since_spending,
  num_periods_lapsed = bridge_ts_sm$time_laps,
  T_b = T_b_df$T_b,
  N_b = T_b_df$n_b,
  X = X_bridge_sm,
  run_estimation = 1
)

system.time(model_fit_opt <- optimizing(dgp_model, data = real_data_list, verbose = T))

get_opt_est <- function(x, par) {
  x$par[grepl(par, names(x$par))]
}

mle_beta_deck <- get_opt_est(model_fit_opt, "\\bbeta_deck\\b")
mle_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")
arm::invlogit(mle_discount_scalar)

expose_stan_functions("Models/msm_bridge_decay_v2.stan")


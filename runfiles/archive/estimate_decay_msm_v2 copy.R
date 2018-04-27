library(tidyverse)
library(skimr)

# Load complete time series
load("data/bridge_timeseries_merged_by_proj.rdata")

bridgeIds <- unique(bridge_ts$bridgeID)
bridge_sample <- sample(bridgeIds, size = 200, replace = F)

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

bridge_ts_sm <- bridge_ts_sm %>%
  left_join(T_b_df)

X_bridge_sm <- model.matrix(~ scale(age) + factor(Type_of_Service) - 1, data = bridge_ts_sm)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dgp_model <- stan_model("Models/msm_bridge_decay_v2.stan")

w <- 1e6 # for scaling spending

real_data_list <- list(
  N = nrow(bridge_ts_sm),
  B = length(unique(bridge_ts_sm$bridgeID)),
  M = ncol(X_bridge_sm),
  H = max(unique(bridge_ts_sm$deck)),
  deck_health = bridge_ts_sm$deck,
  superstructure_health = bridge_ts_sm$superstructure,
  substructure_health = bridge_ts_sm$substructure,
  spending = (bridge_ts_sm$spending / w),
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
mle_beta_superstructure <- get_opt_est(model_fit_opt, "\\bbeta_superstructure\\b")
mle_beta_substructure <- get_opt_est(model_fit_opt, "\\bbeta_substructure\\b")
mle_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")
arm::invlogit(mle_discount_scalar)

expose_stan_functions("Models/msm_bridge_decay_v2.stan")

N_ts_sm = nrow(bridge_ts_sm)
B_ts_sm = length(unique(bridge_ts_sm$bridgeID))
M_ts_sm = ncol(X_bridge_sm)
H_ts_sm = max(unique(bridge_ts_sm$deck))

convertMatrixArrayParam <- function(vectorized_mat, d1, d2, d3){
  #Assumes the matrix array representation is matrix[d2, d3] Mat[d1] in Stan
  vectorized_array <- array(vectorized_mat, dim = c(d1, d2, d3))
  matrix_array <- lapply(1:d1, function(i) vectorized_array[i,,])
  return(matrix_array)
}

mle_beta_deck_list <- convertMatrixArrayParam(mle_beta_deck, H_ts_sm, H_ts_sm, (M_ts_sm+1))
mle_beta_superstructure_list <- convertMatrixArrayParam(mle_beta_superstructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))
mle_beta_substructure_list <- convertMatrixArrayParam(mle_beta_substructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))

propegateX <- function(df, t){

  new_age <- df$age + 1:t
  print(new_age)
  new_data_year <- df$data_year + 1:t
  print(new_data_year)
  
  fac_df <- df %>% select(-age, -data_year)
  
  new_vals <- data.frame(
    age = new_age,
    data_year = new_data_year
  ) %>% 
    merge(fac_df)
  
  new_df <- rbind(df, new_vals)
  return(new_df)
}


## get forward looking X_f
bridge_ts_sm_f <- bridge_ts_sm %>%
  select(bridgeID, data_year, age, Type_of_Service) %>%
  group_by(bridgeID) %>%
  summarize(
    age = last(age) + 1,
    data_year = last(data_year) + 1,
    Type_of_Service = last(Type_of_Service)
  ) %>%
  group_by(bridgeID) %>%
  do(propegateX(.,3))

X_f <- model.matrix(~ scale(age) + factor(Type_of_Service) - 1, data = bridge_ts_sm_f)

spending_f <- c(500, rep(0,(B-1)))

forecasted_states <- forecast_health_state_rng( N_ts_sm,
                           B_ts_sm,
                           H_ts_sm,
                           M_ts_sm,
                           as.array(bridge_ts_sm$deck),
                           as.array(bridge_ts_sm$superstructure),
                           as.array(bridge_ts_sm$substructure),
                           T_b_df$T_b,
                           T_b_df$n_b,
                           X_bridge_sm,
                           (bridge_ts_sm$spending / w),
                           mle_beta_deck_list,
                           mle_beta_superstructure_list,
                           mle_beta_substructure_list,
                           mle_discount_scalar,
                           4, # number of periods forward = 1
                           X_f, # projected X_f for T_f periods for each bridge
                           spending_f # spending in first period of sim
)

forecast_df <- bridge_ts_sm %>%
  select(bridgeID, data_year, age, Type_of_Service) %>% as.tibble() %>%
  mutate(
    type = "data"
  )

bridge_ts_sm_f <- bridge_ts_sm_f %>% ungroup() %>% as.tibble() %>% mutate( type = "forecast")

forecast_df <- rbind(forecast_df,bridge_ts_sm_f ) %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    deck_health = as.vector(unlist(forecasted_states[1])),
    superstructure_health = as.vector(unlist(forecasted_states[2])),
    substructure_health = as.vector(unlist(forecasted_states[3]))
  ) %>%
  ungroup() %>%
  mutate(
    min_health = pmin(deck_health, superstructure_health, substructure_health)
  )

sample_bridgeIDs <- unique(forecast_df$bridgeID)

forecast_df %>%
  filter(bridgeID %in% forecast_df$bridgeID[1]) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID)


forecast_df %>%
  filter(bridgeID %in% sample_bridgeIDs[1:10]) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID)

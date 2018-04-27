library(tidyverse)
library(skimr)
library(ggridges)

# Load complete time series
load("data/bridge_timeseries_merged_apr2018.rdata")

set.seed(4242)

bridgeIds <- unique(bridge_ts$bridgeID)
bridge_sample_train <- sample(bridgeIds, size = 400, replace = F)
bridge_sample_test <- sample(bridgeIds[(!(bridgeIds %in% bridge_sample_train))], size = 200, replace = F)
intersect(bridge_sample_train,bridge_sample_test)

bridge_ts %>% apply(2,function(x) sum(is.na(x)))


bridge_ts_train <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample_train)

bridge_ts_train <- bridge_ts_train %>%
  ungroup() %>%
  arrange(bridgeID, data_year) %>%
  group_by(
    bridgeID
  ) %>%
  mutate(
    time_lag = lag(data_year, default = 1990),
    time_laps = data_year - time_lag,
    age = pmax(data_year - year_built_latest, 0)
  ) %>%
  ungroup() %>%
  mutate(
    rn = row_number(),
    deck_nm_rn = ifelse(deck == -999, NA, rn),
    superstructure_nm_rn = ifelse(superstructure == -999, NA, rn),
    substructure_nm_rn = ifelse(substructure == -999, NA, rn),
    ) %>%
  group_by(bridgeID) %>%
  mutate(
    deck_nm_rn_lag = lag(deck_nm_rn, na.rm = T),
    superstructure_nm_rn_lag = lag(superstructure_nm_rn, na.rm = T),
    substructure_nm_rn_lag = lag(substructure_nm_rn, na.rm = T)
  ) %>%
  mutate(
    deck_nm_rn_lag = zoo::na.locf(deck_nm_rn_lag, na.rm = F),
    superstructure_nm_rn_lag = zoo::na.locf(superstructure_nm_rn_lag, na.rm = F),
    substructure_nm_rn_lag = zoo::na.locf(substructure_nm_rn_lag, na.rm = F)
  ) %>%
  mutate(
    deck_nm_rn_lag = ifelse(is.na(deck_nm_rn_lag), -999, deck_nm_rn_lag),
    superstructure_nm_rn_lag = ifelse(is.na(superstructure_nm_rn_lag), -999, superstructure_nm_rn_lag),
    substructure_nm_rn_lag = ifelse(is.na(substructure_nm_rn_lag), -999, substructure_nm_rn_lag),
  ) %>%
  select(
    -deck_nm_rn,
    -superstructure_nm_rn,
    -substructure_nm_rn
  ) %>%
  ungroup()

test <- bridge_ts_train %>%
  select(
    bridgeID,
    data_year,
    deck,
    rn,
    deck_nm_rn,
    deck_nm_rn_lag
  )

T_b_df <- bridge_ts_train %>%
  select(-rn) %>%
  mutate(rn = row_number()) %>%
  group_by(bridgeID) %>%
  summarize(
    T_b = n(),
    n_b = first(rn)
  )

B <- length(unique(bridge_ts_train$bridgeID))

bridge_ts_train <- bridge_ts_train %>%
  left_join(T_b_df)

## Get rid of non-bridge-feature cols
bridge_ts_train_features <- bridge_ts_train %>%
  select(
    -bridgeID,
    -data_year,
    -bin,
    -deck,
    -superstructure,
    -substructure,
    -year_built_latest,
    -spending_in_year,
    -spending,
    -start_year,
    -calendar_year,
    -time_lag,
    -time_laps,
    -first_start_year,
    -last_end_year,
    -project_init_year,
    -project_end_year,
    -num_obs,
    -T_b,
    -n_b,
    -num_projects,
    -no_inspection,
    -total_bridge_spending,
    -avg_bridge_spending,
    -proj_no,
    -deck_nm_rn_lag,
    -superstructure_nm_rn_lag,
    -substructure_nm_rn_lag,
    -rn
  )

X_bridge_train <- model.matrix(~ . - 1, data = bridge_ts_train_features)
X_bridge_train <- scale(X_bridge_train)

## get test data
bridge_ts_test <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample_test)

bridge_ts_test <- bridge_ts_test %>%
  ungroup() %>%
  arrange(bridgeID, data_year) %>%
  group_by(
    bridgeID
  ) %>%
  mutate(
    time_lag = lag(data_year, default = 1990),
    time_laps = data_year - time_lag,
    age = pmax(data_year - year_built_latest, 0)
  ) %>%
  ungroup() %>%
  mutate(
    rn = row_number(),
    deck_nm_rn = ifelse(deck == -999, NA, rn),
    superstructure_nm_rn = ifelse(superstructure == -999, NA, rn),
    substructure_nm_rn = ifelse(substructure == -999, NA, rn),
  ) %>%
  group_by(bridgeID) %>%
  mutate(
    deck_nm_rn_lag = lag(deck_nm_rn, na.rm = T),
    superstructure_nm_rn_lag = lag(superstructure_nm_rn, na.rm = T),
    substructure_nm_rn_lag = lag(substructure_nm_rn, na.rm = T)
  ) %>%
  mutate(
    deck_nm_rn_lag = zoo::na.locf(deck_nm_rn_lag, na.rm = F),
    superstructure_nm_rn_lag = zoo::na.locf(superstructure_nm_rn_lag, na.rm = F),
    substructure_nm_rn_lag = zoo::na.locf(substructure_nm_rn_lag, na.rm = F)
  ) %>%
  mutate(
    deck_nm_rn_lag = ifelse(is.na(deck_nm_rn_lag), -999, deck_nm_rn_lag),
    superstructure_nm_rn_lag = ifelse(is.na(superstructure_nm_rn_lag), -999, superstructure_nm_rn_lag),
    substructure_nm_rn_lag = ifelse(is.na(substructure_nm_rn_lag), -999, substructure_nm_rn_lag),
  ) %>%
  select(
    -deck_nm_rn,
    -superstructure_nm_rn,
    -substructure_nm_rn
  ) %>% 
  ungroup()

T_b_df_test <- bridge_ts_test %>%
  mutate(rn = row_number()) %>%
  group_by(bridgeID) %>%
  summarize(
    T_b = n(),
    n_b = first(rn)
  )

B_test <- length(unique(bridge_ts_test$bridgeID))

bridge_ts_test <- bridge_ts_test %>%
  left_join(T_b_df_test)

bridge_ts_test_features <- bridge_ts_test %>%
  select(
    -bridgeID,
    -data_year,
    -bin,
    -deck,
    -superstructure,
    -substructure,
    -year_built_latest,
    -spending_in_year,
    -spending,
    -start_year,
    -calendar_year,
    -time_lag,
    -time_laps,
    -first_start_year,
    -last_end_year,
    -project_init_year,
    -project_end_year,
    -num_obs,
    -T_b,
    -n_b,
    -num_projects,
    -no_inspection,
    -total_bridge_spending,
    -avg_bridge_spending,
    -proj_no,
    -deck_nm_rn_lag,
    -superstructure_nm_rn_lag,
    -substructure_nm_rn_lag,
    -rn
  )

X_bridge_test <- model.matrix(~ . -1, data = bridge_ts_test_features)
X_bridge_test <- scale(X_bridge_test)


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dgp_model <- stan_model("Models/msm_bridge_decay_v6.stan")

w <- 1e6 # for scaling spending

real_data_list <- list(
  N = nrow(bridge_ts_train),
  B = length(unique(bridge_ts_train$bridgeID)),
  M = ncol(X_bridge_train),
  H = max(unique(bridge_ts_train$deck)),
  deck_health = bridge_ts_train$deck,
  superstructure_health = bridge_ts_train$superstructure,
  substructure_health = bridge_ts_train$substructure,
  last_observed_deck_index = bridge_ts_train$deck_nm_rn_lag,
  last_observed_superstructure_index = bridge_ts_train$superstructure_nm_rn_lag,
  last_observed_substructure_index = bridge_ts_train$substructure_nm_rn_lag,
  spending = (bridge_ts_train$spending / w),
  num_periods_lapsed = bridge_ts_train$time_laps,
  T_b = T_b_df$T_b,
  N_b = T_b_df$n_b,
  X = X_bridge_train,
  # 
  N_new = nrow(bridge_ts_test),
  B_new = length(unique(bridge_ts_test$bridgeID)),
  M_new = ncol(X_bridge_test),
  H_new = max(unique(bridge_ts_test$deck)),
  deck_health_new = bridge_ts_test$deck,
  superstructure_health_new = bridge_ts_test$superstructure,
  substructure_health_new = bridge_ts_test$substructure,
  last_observed_deck_index_new = bridge_ts_test$deck_nm_rn_lag,
  last_observed_superstructure_index_new = bridge_ts_test$superstructure_nm_rn_lag,
  last_observed_substructure_index_new = bridge_ts_test$substructure_nm_rn_lag,
  spending_new = (bridge_ts_test$spending / w),
  num_periods_lapsed_new = bridge_ts_test$time_laps,
  T_b_new = T_b_df_test$T_b,
  N_b_new = T_b_df_test$n_b,
  X_new = X_bridge_test,
  # 
  run_estimation = 1
)

system.time(model_fit_opt <- optimizing(dgp_model, data = real_data_list, init=1, verbose = T))

#
get_opt_est <- function(x, par) {
  x$par[grepl(par, names(x$par))]
}

mle_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")
# mle_X_aug <- get_opt_est(model_fit_opt, "\\bX_aug\\b")
arm::invlogit(mle_discount_scalar)
mle_beta <- get_opt_est(model_fit_opt, "\\bbeta")


estimated_model <- sampling(dgp_model, data = real_data_list, iter = 500, chains = 4, cores = 4)

print(estimated_model, pars="discount_scalar")
print(estimated_model, pars = "beta")

log_posterior_mass <- get_posterior_mean(estimated_model, pars = "log_posterior_mass")[, 5]
sum(log_posterior_mass)

mean(exp(log_posterior_mass/3))
# plot(exp(log_posterior_mass/3))

data.frame(mean_prob = exp(log_posterior_mass/3),
           deck_health = factor(bridge_ts_train$deck)) %>%
  ggplot(aes(x = mean_prob, y = deck_health, fill = deck_health)) + geom_density_ridges(alpha = 0.3)

oos_log_posterior_mass <- get_posterior_mean(estimated_model, pars = "out_of_sample_lpm")[, 5]
sum(oos_log_posterior_mass)
mean(exp(oos_log_posterior_mass/3))

data.frame(oos_mean_prob = exp(oos_log_posterior_mass/3),
           deck_health = factor(bridge_ts_test$deck)) %>%
  ggplot(aes(x = oos_mean_prob, y = deck_health, fill = deck_health)) + geom_density_ridges(alpha = 0.3)


oos_log_posterior_mass_deck <- get_posterior_mean(estimated_model, pars = "out_of_sample_lpm_deck")[, 5]
oos_log_posterior_mass_superstructure <- get_posterior_mean(estimated_model, pars = "out_of_sample_lpm_superstructure")[, 5]
oos_log_posterior_mass_substructure <- get_posterior_mean(estimated_model, pars = "out_of_sample_lpm_substructure")[, 5]


oos_lpm_df <- 
  data.frame(oos_mean_prob = exp(oos_log_posterior_mass/3),
                         oos_prob_deck = exp(oos_log_posterior_mass_deck),
                         oos_prob_superstructure = exp(oos_log_posterior_mass_superstructure),
                         oos_prob_substructure = exp(oos_log_posterior_mass_substructure),
                         deck_health = (bridge_ts_test$deck),
                         superstructure_health = (bridge_ts_test$superstructure),
                         substructure_health = bridge_ts_test$substructure
                         ) %>%
  mutate(
    min_health = pmin(deck_health,superstructure_health, substructure_health),
    min_health_state = factor(min_health)
  ) %>%
  select(
    -deck_health,
    -superstructure_health,
    -substructure_health,
    -min_health
  ) %>%
  rename(
    deck = oos_prob_deck,
    superstructure = oos_prob_superstructure,
    substructure = oos_prob_substructure
  ) %>%
  gather(
    key = "structure",
    value = "model_probability",
    - min_health_state,
    -oos_mean_prob
  )

oos_lpm_df %>%
  ggplot(aes(x=model_probability, y=min_health_state, fill = structure)) + 
  geom_density_ridges(position="dodge", alpha=0.3, from = 0, to =1 ) + theme_ridges() +
  labs(
    title = "Model Probability of Observed State in Test Data",
    x = "Model Probability of Realized Outcome State",
    y = "Minimum Outcome State"
  )

ggsave("graphs/cross_val_demo_v3.png", height = 8, width = 12)
save.image("cross_val_jan12_v3.rdata")

expose_stan_functions("Models/msm_bridge_decay_v2.stan")

N_ts_sm = nrow(bridge_ts_test)
B_ts_sm = length(unique(bridge_ts_test$bridgeID))
M_ts_sm = ncol(X_bridge_test)
H_ts_sm = max(unique(bridge_ts_test$deck))

convertMatrixArrayParam <- function(vectorized_mat, d1, d2, d3){
  #Assumes the matrix array representation is matrix[d2, d3] Mat[d1] in Stan
  vectorized_array <- array(vectorized_mat, dim = c(d1, d2, d3))
  matrix_array <- lapply(1:d1, function(i) vectorized_array[i,,])
  return(matrix_array)
}

bayes_beta_deck <- get_posterior_mean(estimated_model, pars = "beta_deck")[, 5]
bayes_beta_superstructure <- get_posterior_mean(estimated_model, pars = "beta_superstructure")[, 5]
bayes_beta_substructure <- get_posterior_mean(estimated_model, pars = "beta_substructure")[, 5]
bayes_discount_scalar <- get_posterior_mean(estimated_model, pars = "discount_scalar")[, 5]

arm::invlogit(bayes_discount_scalar)

bayes_beta_deck_list <- convertMatrixArrayParam(bayes_beta_deck, H_ts_sm, H_ts_sm, (M_ts_sm+1))
bayes_beta_superstructure_list <- convertMatrixArrayParam(bayes_beta_superstructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))
bayes_beta_substructure_list <- convertMatrixArrayParam(bayes_beta_substructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))

propegateX <- function(df, t){
  
  new_age <- df$age + 1:t
  # print(new_age)
  new_data_year <- df$data_year + 1:t
  # print(new_data_year)
  
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
bridge_ts_sm_f <- bridge_ts_test %>%
  select(bridgeID, data_year, age, Structure_Length) %>%
  group_by(bridgeID) %>%
  summarize(
    age = last(age) + 1,
    data_year = last(data_year) + 1,
    Structure_Length = last(Structure_Length)
  ) %>%
  group_by(bridgeID) %>%
  do(propegateX(.,3))

X_f <- model.matrix(~ scale(age) + scale(Structure_Length), data = bridge_ts_sm_f)

spending_f <- c(5, rep(0,(B-1)))

forecasted_states <- forecast_health_state_rng( N_ts_sm,
                                                B_ts_sm,
                                                H_ts_sm,
                                                M_ts_sm,
                                                as.array(bridge_ts_test$deck),
                                                as.array(bridge_ts_test$superstructure),
                                                as.array(bridge_ts_test$substructure),
                                                T_b_df_test$T_b,
                                                T_b_df_test$n_b,
                                                X_bridge_test,
                                                (bridge_ts_test$spending / w),
                                                bayes_beta_deck_list,
                                                bayes_beta_superstructure_list,
                                                bayes_beta_substructure_list,
                                                bayes_discount_scalar,
                                                4, # number of periods forward = 1
                                                X_f, # projected X_f for T_f periods for each bridge
                                                spending_f # spending in first period of sim
)

forecast_df <- bridge_ts_test %>%
  select(bridgeID, data_year, age, Structure_Length) %>% as.tibble() %>%
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
  filter(bridgeID %in% sample_bridgeIDs[1:5]) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID)


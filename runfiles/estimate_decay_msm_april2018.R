library(tidyverse)
library(skimr)
library(ggridges)
library(rstan)

# Load complete time series
load("data/bridge_timeseries_merged_apr2018.rdata")

set.seed(4242)

bridgeIds <- unique(bridge_ts$bridgeID)
bridge_sample_train <- sample(bridgeIds, size = 400, replace = F)
bridge_sample_test <- sample(bridgeIds[(!(bridgeIds %in% bridge_sample_train))], size = 200, replace = F)
intersect(bridge_sample_train,bridge_sample_test)


### Verify feature matrix is complete ##
bridge_ts %>% apply(2,function(x) sum(is.na(x)))

### Prepare feature matrix + other inputs ###

## Get rid of non-bridge-feature cols (except bridgeID for sample slicing)
bridge_ts_features <- bridge_ts %>%
  select(
    # -bridgeID,
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
    # -time_lag,
    # -time_laps,
    -first_start_year,
    -last_end_year,
    -project_init_year,
    -project_end_year,
    -num_obs,
    # -T_b,
    # -n_b,
    -num_projects,
    -no_inspection,
    -total_bridge_spending,
    -avg_bridge_spending,
    -proj_no
    # -deck_nm_rn_lag,
    # -superstructure_nm_rn_lag,
    # -substructure_nm_rn_lag,
    # -rn
  )

X_full <- model.matrix(~ . - 1 - bridgeID, data = bridge_ts_features)
X_full <- scale(X_full)

X_full_aug <- as.tibble(X_full) %>%
  mutate(
    bridgeID = bridge_ts_features$bridgeID
  )

##### Select training data #######

bridge_ts_train <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample_train)

X_train <- as.matrix(X_full_aug %>% filter(bridgeID %in% bridge_sample_train) %>% select(-bridgeID))

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

T_b_df <- bridge_ts_train %>%
  select(-rn) %>%
  mutate(rn = row_number()) %>%
  group_by(bridgeID) %>%
  summarize(
    T_b = n(),
    n_b = first(rn)
  )

B_train <- length(unique(bridge_ts_train$bridgeID))

bridge_ts_train <- bridge_ts_train %>%
  left_join(T_b_df)

##### Select test data #######
bridge_ts_test <- bridge_ts %>%
  filter(bridgeID %in% bridge_sample_test)

X_test <- as.matrix(X_full_aug %>% filter(bridgeID %in% bridge_sample_test) %>% select(-bridgeID))

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


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dgp_model <- stan_model("Models/msm_bridge_decay_v6_skinny.stan")

w <- 1e6 # for scaling spending

real_data_list <- list(
  N = nrow(bridge_ts_train),
  B = length(unique(bridge_ts_train$bridgeID)),
  M = ncol(X_train),
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
  X = X_train,
  #
  N_new = nrow(bridge_ts_test),
  B_new = length(unique(bridge_ts_test$bridgeID)),
  M_new = ncol(X_test),
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
  X_new = X_test,
  #
  run_estimation = 1
)

system.time(model_fit_opt <- optimizing(dgp_model, data = real_data_list, init=1, verbose = T))

save.image("data/mle_estimation_may10.rdata")

#
get_opt_est <- function(x, par) {
  x$par[grepl(par, names(x$par))]
}

mle_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")
# mle_X_aug <- get_opt_est(model_fit_opt, "\\bX_aug\\b")
arm::invlogit(mle_discount_scalar)
mle_beta <- get_opt_est(model_fit_opt, "\\bbeta")
log_posterior_mass <- get_opt_est(model_fit_opt, "\\blog_posterior_mass")
oos_log_posterior_mass <- get_opt_est(model_fit_opt, "\\bout_of_sample_lpm")


estimated_model <- sampling(dgp_model, data = real_data_list, iter = 500, chains = 4, cores = 4)

save.image("data/mle_estimation_may10.rdata")


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


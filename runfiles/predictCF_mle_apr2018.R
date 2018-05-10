load("data/mle_estimation_apr27.rdata")
library(rstan); library(tidyverse)
expose_stan_functions("Models/msm_bridge_decay_v6.stan")

N_ts_sm = nrow(bridge_ts_train)
B_ts_sm = length(unique(bridge_ts_train$bridgeID))
M_ts_sm = ncol(X_bridge_train)
H_ts_sm = max(unique(bridge_ts_train$deck))

convertMatrixArrayParam <- function(vectorized_mat, d1, d2, d3){
  #Assumes the matrix array representation is matrix[d2, d3] Mat[d1] in Stan
  vectorized_array <- array(vectorized_mat, dim = c(d1, d2, d3))
  matrix_array <- lapply(1:d1, function(i) vectorized_array[i,,])
  return(matrix_array)
}

get_opt_est <- function(x, par) {
  x$par[grepl(par, names(x$par))]
}

bayes_beta_deck <- get_opt_est(model_fit_opt, "\\bbeta_deck\\b")
bayes_beta_superstructure <- get_opt_est(model_fit_opt, "\\bbeta_superstructure\\b")
bayes_beta_substructure <- get_opt_est(model_fit_opt, "\\bbeta_substructure\\b")
bayes_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")

# bayes_beta_deck <- get_posterior_mean(estimated_model, pars = "beta_deck")[, 5]
# bayes_beta_superstructure <- get_posterior_mean(estimated_model, pars = "beta_superstructure")[, 5]
# bayes_beta_substructure <- get_posterior_mean(estimated_model, pars = "beta_substructure")[, 5]
# bayes_discount_scalar <- get_posterior_mean(estimated_model, pars = "discount_scalar")[, 5]

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
  df$future_obs_index <- 0

  new_vals <- data.frame(
    age = new_age,
    data_year = new_data_year,
    future_obs_index = 1:t
  ) %>%
    merge(fac_df)

  new_df <- rbind(df, new_vals)
  return(new_df)
}


## get forward looking X_f
bridge_ts_sm_f <- bridge_ts_train_features %>%
  bind_cols(data.frame(bridgeID = bridge_ts_train$bridgeID, data_year = bridge_ts_train$data_year)) %>%
  arrange(bridgeID, data_year) %>%
  group_by(bridgeID) %>%
  summarize_all(last) %>%
    mutate(
    age = (age) + 1,
    data_year = (data_year) + 1
  ) %>%
  group_by(bridgeID) %>%
  do(propegateX(.,3))

bridge_ts_sm_f_features <- bridge_ts_sm_f %>%
  ungroup() %>%
  bind_rows(bridge_ts_train_features %>%
              bind_cols(data.frame(bridgeID = bridge_ts_train$bridgeID, data_year = bridge_ts_train$data_year, future_obs_index = 0))) %>%
  select(-bridgeID, -data_year)

X_f <- model.matrix(~ . -1, data = bridge_ts_sm_f_features)
X_f <- scale(X_f)
X_f <- as.data.frame(X_f) %>% mutate(future_obs_index = bridge_ts_sm_f_features$future_obs_index) %>% dplyr::filter(future_obs_index > 0) %>% select(-future_obs_index)
X_f <- as.matrix(X_f)

# spending_f <- c(5, rep(0,(B_ts_sm-1)))
spending_f <- rbinom(B_ts_sm, 1, 0.3) * 15

forecasted_states <- forecast_health_state_rng( N_ts_sm,
                                                B_ts_sm,
                                                H_ts_sm,
                                                M_ts_sm,
                                                as.array(bridge_ts_train$deck),
                                                as.array(bridge_ts_train$superstructure),
                                                as.array(bridge_ts_train$substructure),
                                                T_b_df$T_b,
                                                T_b_df$n_b,
                                                X_bridge_train,
                                                (bridge_ts_train$spending / w),
                                                bayes_beta_deck_list,
                                                bayes_beta_superstructure_list,
                                                bayes_beta_substructure_list,
                                                bayes_discount_scalar,
                                                3, # number of periods forward = 1
                                                X_f, # projected X_f for T_f periods for each bridge
                                                spending_f # spending in first period of sim
)

forecast_df <- bridge_ts_train_features %>%
  mutate(
    bridgeID = bridge_ts_train$bridgeID,
    data_year = bridge_ts_train$data_year,
    future_obs_index = 0,
    type = "data",
    spending = bridge_ts_train$spending,
    spending_in_year = bridge_ts_train$spending_in_year
  ) %>%
  mutate(
    spending = ifelse(spending == 0, NA, spending)
  )

spending_cf_df <- data.frame(spending = spending_f, spending_in_year = 2018, bridgeID = unique(bridge_ts_train$bridgeID), data_year = 2018)
forecast_df_cf <- bridge_ts_sm_f %>%
  ungroup() %>%
  left_join(spending_cf_df, by = c("bridgeID", "data_year")) %>%
  mutate(
    type = "forecast",
    spending = ifelse(spending == 0, NA, spending)
    ) %>%
  filter(future_obs_index > 0)

forecast_df <- rbind(forecast_df,forecast_df_cf ) %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    deck_health = as.vector(unlist(forecasted_states[1])),
    superstructure_health = as.vector(unlist(forecasted_states[2])),
    substructure_health = as.vector(unlist(forecasted_states[3]))
  ) %>%
  mutate(
    deck_health = ifelse(deck_health == -999, NA, deck_health),
    superstructure_health = ifelse(superstructure_health == -999, NA, superstructure_health),
    substructure_health = ifelse(substructure_health == -999, NA, substructure_health)
  ) %>%
  ungroup() %>%
  mutate(
    min_health = pmin(deck_health, superstructure_health, substructure_health, na.rm=T)
  )

unique_bridgeIDs <- unique(forecast_df$bridgeID)

forecast_df %>%
  filter(bridgeID %in% forecast_df$bridgeID[1]) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID) +
  geom_vline(data = spending_cf_df %>% filter(bridgeID %in% forecast_df$bridgeID[1]), aes(xintercept = spending_in_year), linetype = "dashed")


sample_bridges <- sample(unique_bridgeIDs, 12)

sample_bridge_spending <- forecast_df %>%
  filter(bridgeID %in% sample_bridges) %>%
  select(bridgeID, spending, spending_in_year) %>%
  filter(complete.cases(.)) %>%
  mutate(
    cf_spending = ifelse(spending_in_year == 2018, "Counterfactual Spending", "Historical Spending")
  )


forecast_df %>%
  filter(bridgeID %in% sample_bridges) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID) +
  geom_vline(data =  sample_bridge_spending, aes(xintercept = spending_in_year, color = cf_spending), linetype = "dashed") +
  labs(title = "Historic + Counterfactual Minimum Health Score Projections Given Random Spending")

sample_bridges
spending_f[match(sample_bridges,unique_bridgeIDs)]

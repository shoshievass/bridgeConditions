load("data/estimation_june4.rdata")
library(rstan); library(tidyverse)
expose_stan_functions("Models/msm_bridge_decay_v6.stan")

N_ts_sm = nrow(bridge_ts_train)
B_ts_sm = length(unique(bridge_ts_train$bridgeID))
M_ts_sm = ncol(X_train)
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

# arm::invlogit(bayes_discount_scalar)

bayes_beta_deck_list <- convertMatrixArrayParam(bayes_beta_deck, H_ts_sm, H_ts_sm, (M_ts_sm+1))
bayes_beta_superstructure_list <- convertMatrixArrayParam(bayes_beta_superstructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))
bayes_beta_substructure_list <- convertMatrixArrayParam(bayes_beta_substructure, H_ts_sm, H_ts_sm, (M_ts_sm+1))

# list_of_draws <- rstan::extract(estimated_model)

# Ah-- this answers my question from before. Worth doing in Stan (this is what I do for our MSM model and it's like a gazillion times faster)?
propegateX <- function(df, t){

  new_age <- df$age + 1:t
  # print(new_age)
  new_data_year <- df$data_year + 1:t
  # print(new_data_year)

  fac_df <- df %>% select(-age, -data_year)
  df$future_obs_index <- 1

  new_vals <- data.frame(
    age = new_age,
    data_year = new_data_year,
    future_obs_index = 2:(t+1)
  ) %>%
    merge(fac_df)

  new_df <- rbind(df, new_vals)
  return(new_df)
}

bridge_ts_train_features <- data.frame(bridgeID = bridge_ts_train$bridgeID, data_year = bridge_ts_train$data_year) %>%
  bind_cols(as.data.frame(X_train))

## get forward looking X_f
bridge_ts_sm_f <- bridge_ts_train_features %>%
  # bind_cols(data.frame(bridgeID = bridge_ts_train$bridgeID, data_year = bridge_ts_train$data_year)) %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    age = bridge_ts_train$age
  ) %>%
  group_by(bridgeID) %>%
  summarize_all(last) %>%
    mutate(
    age = (age) + 1,
    data_year = (data_year) + 1
  ) %>%
  group_by(bridgeID) %>%
  do(propegateX(.,5)) %>%
  ungroup()

bridge_ts_sm_f_features <- bridge_ts_sm_f %>%
  ungroup() %>%
  bind_rows(bridge_ts_train_features %>%
              mutate(
                future_obs_index = 0,
                age = bridge_ts_train$age
              )
  ) %>%
  arrange(bridgeID, data_year) %>%
  mutate(age = scale(age))

X_f <- as.matrix(bridge_ts_sm_f_features %>% ungroup()  %>% dplyr::filter(future_obs_index > 0) %>%  dplyr::select(-bridgeID, -data_year, -future_obs_index))

# spending_f <- c(5, rep(0,(B_ts_sm-1)))
spending_f <- rbinom(B_ts_sm, 1, 0.3) * 15

bridge_ts_train <- bridge_ts_train %>%
  arrange(bridgeID, data_year)

forecasted_states <- forecast_health_state_rng( N_ts_sm,
                                                B_ts_sm,
                                                H_ts_sm,
                                                M_ts_sm,
                                                as.array(bridge_ts_train$deck),
                                                as.array(bridge_ts_train$superstructure),
                                                as.array(bridge_ts_train$substructure),
                                                T_b_df$T_b,
                                                T_b_df$n_b,
                                                X_train,
                                                (bridge_ts_train$spending / w),
                                                bayes_beta_deck_list,
                                                bayes_beta_superstructure_list,
                                                bayes_beta_substructure_list,
                                                bayes_discount_scalar,
                                                6, # number of periods forward = 1
                                                X_f, # projected X_f for T_f periods for each bridge
                                                spending_f # spending in first period of sim
)


get_forecast_at_draw <- function(i, draws){
  bayes_beta_deck_list <- convertMatrixArrayParam(draws$beta_deck[i,,,], H_ts_sm, H_ts_sm, (M_ts_sm+1))
  bayes_beta_superstructure_list <- convertMatrixArrayParam(draws$beta_superstructure[i,,,], H_ts_sm, H_ts_sm, (M_ts_sm+1))
  bayes_beta_substructure_list <- convertMatrixArrayParam(draws$beta_substructure[i,,,], H_ts_sm, H_ts_sm, (M_ts_sm+1))

  bayes_discount_scalar <- draws$discount_scalar[i]

  forecasted_states <- forecast_health_state_rng( N_ts_sm,
                                                  B_ts_sm,
                                                  H_ts_sm,
                                                  M_ts_sm,
                                                  as.array(bridge_ts_train$deck),
                                                  as.array(bridge_ts_train$superstructure),
                                                  as.array(bridge_ts_train$substructure),
                                                  T_b_df$T_b,
                                                  T_b_df$n_b,
                                                  X_train,
                                                  (bridge_ts_train$spending / w),
                                                  bayes_beta_deck_list,
                                                  bayes_beta_superstructure_list,
                                                  bayes_beta_substructure_list,
                                                  bayes_discount_scalar,
                                                  4, # number of periods forward = 1
                                                  X_f, # projected X_f for T_f periods for each bridge
                                                  spending_f # spending in first period of sim
  )

  return(forecasted_states)
}
#
# forecast_list2 <- pmap(.f = get_forecast_at_draw, list(1:1000), list_of_draws)
#
# test <- forecast_list2 %>%
#   reduce(.f = mean)
#
#
#
# sum_lists <- function(lists){
#   out <- lapply(lists, function(x) mean(x))
# }
#
# mean_forecasts <- apply(forecast_list, 2, function(x) mean(x))

forecast_df <- bridge_ts_train_features %>%
  mutate(
    future_obs_index = 0,
    type = "data",
    spending = bridge_ts_train$spending,
    spending_in_year = bridge_ts_train$spending_in_year
  ) %>%
  mutate(
    spending = ifelse(spending == 0, NA, spending)
  )

spending_cf_df <- data.frame(spending = spending_f, spending_in_year = 2017, bridgeID = unique(bridge_ts_train$bridgeID), data_year = 2017)
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
    cf_spending = ifelse(spending_in_year == 2017, "Counterfactual Spending", "Historical Spending")
  )


forecast_df %>%
  filter(bridgeID %in% sample_bridges) %>%
  ggplot(aes(x = data_year, y = min_health, color = type)) + geom_line() + facet_wrap(~bridgeID) +
  geom_vline(data =  sample_bridge_spending, aes(xintercept = spending_in_year, color = cf_spending), linetype = "dashed") +
  labs(title = "Historic + Counterfactual Minimum Health Score Projections Given Random Spending")

sample_bridges
spending_f[match(sample_bridges,unique_bridgeIDs)]

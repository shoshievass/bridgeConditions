library(tidyverse)

B <- 100
T_max <- 16

simulated_bridges <- data.frame(bridgeID = 1:B, age = runif(B, 0, 100))
bridge_dates <- expand.grid(bridgeID = 1:B, data_year = 1:T_max) %>% arrange(bridgeID)

simulated_bridges <- simulated_bridges %>%
  right_join(bridge_dates)

T_b_df <- simulated_bridges %>%
  mutate(rn = row_number()) %>%
  group_by(bridgeID) %>%
  summarize(
    T_b = n(),
    n_b = first(rn)
  )

simulated_bridges <- simulated_bridges %>%
  left_join(T_b_df)

simulated_bridges <- simulated_bridges %>%
  group_by(bridgeID) %>%
  mutate(
    spending_ind = rbinom(T_max, 1, 0.1),
    spending = rgamma(T_max, 15, 5) * spending_ind,
    cum_spending = cumsum(spending),
    tacit_work_ind = rbinom(T_max, 1, 0.05)
  ) %>%
  ungroup() %>%
  mutate(
    time_laps = 1,
    work_done = ifelse(spending_ind > 0, 1 ,0)
  )

simulated_bridges <- simulated_bridges %>%
  group_by(bridgeID, cum_spending) %>%
  mutate(
    periods_since_spending = cumsum(time_laps) - 1
  ) %>%
  ungroup()

X_bridge <- model.matrix(~ scale(age), data = simulated_bridges)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# expose_stan_functions("Models/msm_bridge_decay_v2.stan")
# test_triangular <- getLowerTriangularMatrix(5)
# test_discounting_mat <- getDiscountingMatrix(5, (0.95))

dgp_model <- stan_model("Models/msm_bridge_decay_v8_msw.stan")

# w = 1e4 # for scaling spending
w <- 1

raw_data_list <- list(
  N = nrow(simulated_bridges),
  B = length(unique(simulated_bridges$bridgeID)),
  T = length(unique(simulated_bridges$data_year)),
  M = ncol(X_bridge),
  H = 7,
  bridge_ID = simulated_bridges$bridgeID,
  period_ID = simulated_bridges$data_year,
  deck_health = sample(4:7, nrow(simulated_bridges), replace = T), # intialize randomly
  superstructure_health = sample(4:7, nrow(simulated_bridges), replace = T), # intialize randomly
  substructure_health = sample(4:7, nrow(simulated_bridges), replace = T), # intialize randomly
  last_observed_deck_index = c(-999,1:(nrow(simulated_bridges)-1)), ## no gaps here, so just previous index
  last_observed_superstructure_index = c(-999,1:(nrow(simulated_bridges)-1)),
  last_observed_substructure_index = c(-999,1:(nrow(simulated_bridges)-1)),
  spending = (simulated_bridges$spending / w),
  num_periods_since_spending = simulated_bridges$periods_since_spending,
  # work_done = simulated_bridges$work_done,
  num_periods_lapsed = simulated_bridges$time_laps,
  T_b = T_b_df$T_b,
  N_b = T_b_df$n_b,
  X = X_bridge,
  run_estimation = 0
)


dgp_sample <- sampling(dgp_model, data = raw_data_list, iter = 40, chains = 1, seed = 242)

dgp_beta <- as.data.frame(dgp_sample, pars = "beta_deck")
dgp_beta <- t(dgp_beta[1, ])
dgp_beta

dgp_discount_scalar <- as.data.frame(dgp_sample, pars = "discount_scalar")
dgp_discount_scalar <- t(dgp_discount_scalar[1, ])
dgp_discount_scalar

dgp_sim_deck_health <- as.data.frame(dgp_sample, pars = "deck_health_sim")
dgp_sim_deck_health <- t(dgp_sim_deck_health[1, ])
summary(dgp_sim_deck_health)

dgp_sim_superstructure_health <- as.data.frame(dgp_sample, pars = "superstructure_health_sim")
dgp_sim_superstructure_health <- t(dgp_sim_superstructure_health[1, ])
summary(dgp_sim_superstructure_health)

dgp_sim_substructure_health <- as.data.frame(dgp_sample, pars = "substructure_health_sim")
dgp_sim_substructure_health <- t(dgp_sim_substructure_health[1, ])
summary(dgp_sim_substructure_health)


simulated_data_list <- list(
  N = nrow(simulated_bridges),
  B = length(unique(simulated_bridges$bridgeID)),
  T = length(unique(simulated_bridges$data_year)),
  M = ncol(X_bridge),
  H = 7,
  bridge_ID = simulated_bridges$bridgeID,
  period_ID = simulated_bridges$data_year,
  deck_health = as.vector(dgp_sim_deck_health),
  superstructure_health = as.vector(dgp_sim_superstructure_health),
  substructure_health = as.vector(dgp_sim_substructure_health),
  last_observed_deck_index = c(-999,1:(nrow(simulated_bridges)-1)), ## no gaps here, so just previous index
  last_observed_superstructure_index = c(-999,1:(nrow(simulated_bridges)-1)),
  last_observed_substructure_index = c(-999,1:(nrow(simulated_bridges)-1)),
  spending = (simulated_bridges$spending / w),
  num_periods_since_spending = simulated_bridges$periods_since_spending,
  work_done = simulated_bridges$work_done,
  num_periods_lapsed = simulated_bridges$time_laps,
  T_b = T_b_df$T_b,
  N_b = T_b_df$n_b,
  X = X_bridge,
  run_estimation = 1
)


system.time(model_fit_opt <- optimizing(dgp_model, data = simulated_data_list, verbose = T))

get_opt_est <- function(x, par) {
  x$par[grepl(par, names(x$par))]
}

mle_beta <- get_opt_est(model_fit_opt, "\\bbeta_deck\\b")
plot(mle_beta, dgp_beta)

mle_discount_scalar <- get_opt_est(model_fit_opt, "\\bdiscount_scalar\\b")
plot(mle_discount_scalar, dgp_discount_scalar)

estimated_model <- sampling(dgp_model, data = simulated_data_list, iter = 500, chains = 2, cores = 2)

bayes_beta <- as.numeric(colMeans(as.data.frame(estimated_model, pars = "beta_deck")))
plot(bayes_beta,dgp_beta)

bayes_discount_scalar <- as.numeric(colMeans(as.data.frame(estimated_model, pars = "discount_scalar")))

print(estimated_model, pars = "discount_scalar")

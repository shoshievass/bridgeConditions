functions{
  matrix getLowerTriangularMatrix(int T){
    matrix[T,T] lower_triangular_mat;
    
    for(t in 1:T){
      lower_triangular_mat[t] = append_col(rep_row_vector(1, t), rep_row_vector(0, T-t));
    }
    return(lower_triangular_mat);
  }
  matrix getDiscountingMatrix(int T,
                              real delta){
    matrix[T,T] lower_triangular_mat;
    matrix[T,T] discounting_mat;
    vector[T] discounts;
    
    lower_triangular_mat = getLowerTriangularMatrix(T);
    for(t in 1:T){
      discounts[t] = delta^(t-1);
    }
    
    discounting_mat = diag_matrix(discounts) * lower_triangular_mat;
    
    return(discounting_mat);
  }
  vector[] forecast_health_state_rng( int N,
                           int B,
                           int H,
                           int M,
                           int[] deck_health,
                           int[] superstructure_health,
                           int[] substructure_health,
                           int[] T_b,
                           int[] N_b,
                           matrix X,
                           vector spending,
                           matrix[] beta_deck,
                           matrix[] beta_superstructure,
                           matrix[] beta_substructure,
                           real discount_factor,
                           int T_f, // number of periods forward
                           matrix X_f, // projected X_f for T_f periods for each bridge
                           vector spending_f // spending in first period of sim
                          ){
    vector[N+(T_f*B)] out[3];
    int deck_health_sim_f[N+(T_f*B)];
    int superstructure_health_sim_f[N+(T_f*B)];
    int substructure_health_sim_f[N+(T_f*B)];
    vector[N+(T_f*B)] discounted_spending_f;
    matrix[N+(T_f*B), M+1] X_aug_f;
  
    int t_b;
    int n_b;
    int n_b_f;
    int n;
    
    n_b_f = 1;
    for(b in 1:B){
      t_b = T_b[b];
      n_b = N_b[b];

      deck_health_sim_f[n_b_f : n_b_f + t_b -1] = deck_health[n_b : n_b + t_b -1];
      superstructure_health_sim_f[n_b_f : n_b_f + t_b -1] = superstructure_health[n_b : n_b + t_b -1];
      substructure_health_sim_f[n_b_f : n_b_f + t_b -1] = substructure_health[n_b : n_b + t_b -1];
      
      discounted_spending_f[n_b_f:(n_b_f + t_b - 1 + T_f)] = getDiscountingMatrix((t_b+T_f), discount_factor) * 
                      append_row( spending[n_b:(n_b + t_b - 1)], append_row(spending_f[b],rep_vector(0, T_f - 1)));
      
      X_aug_f[n_b_f:(n_b_f + t_b - 1 + T_f)] = append_col(discounted_spending_f[n_b_f:(n_b_f + t_b - 1 + T_f)], 
                                            append_row(X[n_b:(n_b + t_b - 1)], X_f[(T_f*(b-1) + 1) : T_f*b ])
                                           );
      
      
      for(t in 1:T_f){
        n = n_b_f + t_b - 1 + t;
        // print("deck health sim: ", [deck_health_sim_f[n-1]])
        // print("X_aug_f: ", [X_aug_f[n]])
        // // print("dims beta deck: ", dims(beta_deck[deck_health_sim_f[n-1]]))
        // print("dims X_aug_f: ", dims(X_aug_f[n]'))
        // print("deck soft maxes inputs: ", (beta_deck[deck_health_sim_f[n-1]] * X_aug_f[n]'))
        if(deck_health_sim_f[n-1] != -999){
          deck_health_sim_f[n] = categorical_rng(softmax(beta_deck[deck_health_sim_f[n-1]] * X_aug_f[n]'));
        }
        else{deck_health_sim_f[n] = -999;}
        
        if(superstructure_health_sim_f[n-1] != -999){
          superstructure_health_sim_f[n] = categorical_rng(softmax(beta_superstructure[superstructure_health_sim_f[n-1]] * X_aug_f[n]'));
        }
        else{superstructure_health_sim_f[n] = -999;}
        
        if(substructure_health_sim_f[n-1] != -999){
          substructure_health_sim_f[n] = categorical_rng(softmax(beta_substructure[substructure_health_sim_f[n-1]] * X_aug_f[n]'));
        }
        else{substructure_health_sim_f[n] = -999;}
      }
      // augment rolling index
      n_b_f = n_b_f + t_b + T_f;
      
    }
  
  out[1] = to_vector(deck_health_sim_f);
  out[2] = to_vector(superstructure_health_sim_f);
  out[3] = to_vector(substructure_health_sim_f);
  return(out);
  
  }
}
data {
 int N; // number observations
 int B; // number unique bridges
 int M; // number of bridge covariates
 int H; // max health score possible
 int deck_health[N];
 int superstructure_health[N];
 int substructure_health[N];
 int last_observed_deck_index[N];
 int last_observed_superstructure_index[N];
 int last_observed_substructure_index[N];
 
 vector[N] spending;
 int num_periods_lapsed[N]; // number of years since last observation of this bridge
 int T_b[B]; // number of periods observed for each bridge
 int N_b[B]; // number of periods observed for each bridge
 matrix[N, M] X;
 //
 int N_new; // number observations
 int B_new; // number unique bridges
 int M_new; // number of bridge covariates
 int H_new; // max health score possible
 int deck_health_new[N_new];
 int superstructure_health_new[N_new];
 int substructure_health_new[N_new];
int last_observed_deck_index_new[N_new];
 int last_observed_superstructure_index_new[N_new];
 int last_observed_substructure_index_new[N_new];
 
 vector[N_new] spending_new;
 int num_periods_lapsed_new[N_new]; // number of years since last observation of this bridge
 int T_b_new[B_new]; // number of periods observed for each bridge
 int N_b_new[B_new]; // number of periods observed for each bridge
 matrix[N_new, M_new] X_new;
 //
 int<lower=0,upper=1> run_estimation;
}
parameters {
  // matrix[H,M+1] beta_deck[H];
  // matrix[H,M+1] beta_superstructure[H];
  // matrix[H,M+1] beta_substructure[H];
  matrix[H,M+1] beta[H];
  matrix[H,M+1] z_deck[H];
  matrix[H,M+1] z_superstructure[H];
  matrix[H,M+1] z_substructure[H];
  matrix<lower = 0>[H,M+1] sigma[H];
  real discount_scalar;
}
transformed parameters{
  real<lower=0> discount_factor;
  vector[N] discounted_spending;
  matrix[N, M+1] X_aug;
  matrix[N_new, M_new+1] X_aug_new;
  vector[N_new] discounted_spending_new;
  matrix[H,M+1] beta_deck[H];
  matrix[H,M+1] beta_superstructure[H];
  matrix[H,M+1] beta_substructure[H];
  
  discount_factor = inv_logit(discount_scalar);
  
  for(b in 1:B){
    discounted_spending[N_b[b]:(N_b[b] + T_b[b] - 1)] = getDiscountingMatrix(T_b[b], discount_factor) * spending[N_b[b]:(N_b[b] + T_b[b] - 1)];
  }
  
  for(b in 1:B_new){
    discounted_spending_new[N_b_new[b]:(N_b_new[b] + T_b_new[b] - 1)] = getDiscountingMatrix(T_b_new[b], discount_factor) * spending_new[N_b_new[b]:(N_b_new[b] + T_b_new[b] - 1)];
  }
  
  X_aug = append_col(discounted_spending, X);
  X_aug_new = append_col(discounted_spending_new, X_new);
  
  for(i in 1:H){
    beta_deck[i] = beta[i] + sigma[i] .* z_deck[i];
    beta_superstructure[i] = beta[i] + sigma[i] .* z_superstructure[i];
    beta_substructure[i] = beta[i] + sigma[i] .* z_substructure[i];
  }

}
model {
  int t_b;
  int n_b;
  int n;
  int temp_lag_deck_index;
  int temp_lag_superstructure_index;
  int temp_lag_substructure_index;

  discount_scalar ~ normal(2,1);
  
  for(i in 1:H){
    to_vector(beta[i]) ~ normal(0, 1);
    to_vector(sigma[i]) ~ normal(0, 1);
    to_vector(z_deck[i]) ~ normal(0,1);
    to_vector(z_superstructure[i]) ~ normal(0,1);
    to_vector(z_substructure[i]) ~ normal(0,1);
    // to_vector(beta_deck[i]) ~ normal(0, 3);
    // to_vector(beta_superstructure[i]) ~ normal(0, 3);
    // to_vector(beta_substructure[i]) ~ normal(0, 3);
  }
    
  for(b in 1:B){
    t_b = T_b[b];
    n_b = N_b[b];

    if(run_estimation==1) {

      for(t in 1:(t_b-1)){
        n = n_b + t;

        if(deck_health[n] != -999 && last_observed_deck_index[n] != -999){
          temp_lag_deck_index  = last_observed_deck_index[n];
          // print("obs index, deck helth, lag index, lag health: ", {n, deck_health[n], temp_lag_deck_index, deck_health[temp_lag_deck_index]});
          // print("deck betas times X: ", beta_deck[deck_health[temp_lag_deck_index]] * X_aug[n]');
          target += log(softmax(beta_deck[deck_health[temp_lag_deck_index]] * X_aug[n]')[deck_health[n]]);
        }
        if(superstructure_health[n] != -999 && last_observed_superstructure_index[n] != -999){
          temp_lag_superstructure_index  = last_observed_superstructure_index[n];
          target += log(softmax(beta_superstructure[superstructure_health[temp_lag_superstructure_index]] * X_aug[n]')[superstructure_health[n]]);
        }
        if(substructure_health[n] != -999 && last_observed_substructure_index[n] != -999){
          temp_lag_substructure_index  = last_observed_substructure_index[n];
          target += log(softmax(beta_substructure[substructure_health[temp_lag_substructure_index]] * X_aug[n]')[substructure_health[n]]);
        }
      }
    }
  }
}
generated quantities {
  // int deck_health_sim[N];
  // int superstructure_health_sim[N];
  // int substructure_health_sim[N];
  vector[N] lpm_deck;
  vector[N] lpm_superstructure;
  vector[N] lpm_substructure;
  vector[N] log_posterior_mass;
  vector[N_new] out_of_sample_lpm;
  vector[N_new] out_of_sample_lpm_deck;
  vector[N_new] out_of_sample_lpm_superstructure;
  vector[N_new] out_of_sample_lpm_substructure;

  int t_b;
  int n_b;
  int n;

  for(b in 1:B){
    t_b = T_b[b];
    n_b = N_b[b];

    log_posterior_mass[n_b] = 0;
    lpm_deck[n_b] = 0;
    lpm_superstructure[n_b] = 0;
    lpm_substructure[n_b] = 0;

    for(t in 1:(t_b-1)){
      n = n_b + t;
      
      if(deck_health[n] != -999 && last_observed_deck_index[n] != -999){
        lpm_deck[n] = log(softmax(beta_deck[deck_health[ last_observed_deck_index[n] ] ] * X_aug[n]')[deck_health[n]]);
      }
      else{
        lpm_deck[n] = 0;
      }
      if(superstructure_health[n] != -999 && last_observed_superstructure_index[n] != -999){
        lpm_superstructure[n] = log(softmax(beta_superstructure[superstructure_health[ last_observed_superstructure_index[n] ] ] * X_aug[n]')[superstructure_health[n]]);
      }
      else{
        lpm_superstructure[n] = 0;
      }
      if(substructure_health[n] != -999 && last_observed_substructure_index[n] != -999){
        lpm_substructure[n] = log(softmax(beta_substructure[substructure_health[ last_observed_substructure_index[n] ] ] * X_aug[n]')[substructure_health[n]]);
      }
      else{
        lpm_substructure[n] = 0;
      }
      
      log_posterior_mass[n] = lpm_deck[n] + lpm_superstructure[n] + lpm_substructure[n];
    }
  }

  for(b in 1:B_new){
    t_b = T_b_new[b];
    n_b = N_b_new[b];

    out_of_sample_lpm[n_b] = 0;
    out_of_sample_lpm_deck[n_b] = 0;
    out_of_sample_lpm_superstructure[n_b] = 0;
    out_of_sample_lpm_substructure[n_b] = 0;

    for(t in 1:(t_b-1)){
      n = n_b + t;

      if(deck_health_new[n] != -999 && last_observed_deck_index_new[n] != -999){
        out_of_sample_lpm_deck[n] = log(softmax(beta_deck[deck_health_new[ last_observed_deck_index_new[n] ] ] * X_aug_new[n]')[deck_health_new[n]]);
      }
      else{
        out_of_sample_lpm_deck[n] = 0;
      }
      if(superstructure_health_new[n] != -999 && last_observed_superstructure_index_new[n] != -999){
        out_of_sample_lpm_superstructure[n] = log(softmax(beta_superstructure[superstructure_health_new[ last_observed_superstructure_index_new[n] ] ] * X_aug_new[n]')[superstructure_health_new[n]]);
      }
      else{
        out_of_sample_lpm_superstructure[n] = 0;
      }
      if(substructure_health_new[n] != -999 && last_observed_substructure_index_new[n] != -999){
        out_of_sample_lpm_substructure[n] = log(softmax(beta_substructure[substructure_health_new[ last_observed_substructure_index_new[n] ] ] * X_aug_new[n]')[substructure_health_new[n]]);
      }
      else{
        out_of_sample_lpm_substructure[n] = 0;
      }

      out_of_sample_lpm[n] = out_of_sample_lpm_deck[n] + out_of_sample_lpm_superstructure[n] + out_of_sample_lpm_substructure[n];

    }
  }

}
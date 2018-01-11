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
  vector[] health_state_rng( int N,
                           int B,
                           int H,
                           int M,
                           int[] deck_health,
                           int[] superstructure_health,
                           int[] substructure_health,
                           int[] T_b,
                           int[] N_b,
                           matrix X_aug,
                           matrix[] beta_deck,
                           matrix[] beta_superstructure,
                           matrix[] beta_substructure
                          ){
    vector[N] out[3];
    int deck_health_sim[N];
    int superstructure_health_sim[N];
    int substructure_health_sim[N];
  
    int t_b;
    int n_b;
    int n;
    
    for(b in 1:B){
      t_b = T_b[b];
      n_b = N_b[b];
      
      deck_health_sim[n_b] = deck_health[n_b];
      superstructure_health_sim[n_b] = superstructure_health[n_b];
      substructure_health_sim[n_b] = substructure_health[n_b];
      
      for(t in 1:(t_b-1)){
        n = n_b + t;
        deck_health_sim[n] = categorical_rng(softmax(beta_deck[deck_health_sim[n-1]] * X_aug[n]'));
        superstructure_health_sim[n] = categorical_rng(softmax(beta_superstructure[superstructure_health_sim[n-1]] * X_aug[n]'));
        substructure_health_sim[n] = categorical_rng(softmax(beta_substructure[substructure_health_sim[n-1]] * X_aug[n]'));
  
      }
    }
  
  out[1] = to_vector(deck_health_sim);
  out[2] = to_vector(superstructure_health_sim);
  out[3] = to_vector(substructure_health_sim);
  return(out);
  
  }
}
data {
 int N; // number observations
 int B; // number unique bridges
 int T; // max number of periods
 int M; // number of bridge covariates
 int H; // max health score possible
 int bridge_ID[N]; // identify the bridge in each observation
 int period_ID[N]; // identify the period in each observation
 int deck_health[N];
 int superstructure_health[N];
 int substructure_health[N];
 vector[N] spending;
 vector[N] num_periods_since_spending;
 int num_periods_lapsed[N]; // number of years since last observation of this bridge
 int T_b[B]; // number of periods observed for each bridge
 int N_b[B]; // number of periods observed for each bridge
 matrix[N, M] X;
 int<lower=0,upper=1> run_estimation;
}
parameters {
  matrix[H,M+1] beta_deck[H];
  matrix[H,M+1] beta_superstructure[H];
  matrix[H,M+1] beta_substructure[H];
  real discount_scalar;
}
transformed parameters{
  real<lower=0> discount_factor;
  vector[N] discounted_spending;
  matrix[N, M+1] X_aug;

  discount_factor = inv_logit(discount_scalar);
  
  for(b in 1:B){
    discounted_spending[N_b[b]:(N_b[b] + T_b[b] - 1)] = getDiscountingMatrix(T_b[b], discount_factor) * spending[N_b[b]:(N_b[b] + T_b[b] - 1)];
  }
  
  X_aug = append_col(discounted_spending, X);
}
model {
  int t_b;
  int n_b;
  int n;
  // vector[N] discounted_spending;
  // matrix[N, M+1] X_aug;

  discount_scalar ~ normal(3,1);
  
  for(i in 1:H){
    to_vector(beta_deck[i]) ~ normal(0, 1);
    to_vector(beta_superstructure[i]) ~ normal(0, 1);
    to_vector(beta_substructure[i]) ~ normal(0, 1);
  }
    
  for(b in 1:B){
    t_b = T_b[b];
    n_b = N_b[b];
      
    // discounted_spending[n_b:(n_b + t_b - 1)] = getDiscountingMatrix(t_b, discount_factor) * spending[n_b:(n_b + t_b - 1)];
    // X_aug[n_b:(n_b + t_b - 1)] = append_col(discounted_spending[n_b:(n_b + t_b - 1)], X[n_b:(n_b + t_b - 1)]);
    // 
    // 
    if(run_estimation==1) {

      for(t in 1:(t_b-1)){
        n = n_b + t;

        target += log(softmax(beta_deck[deck_health[n-1]] * X_aug[n]')[deck_health[n]]);
        target += log(softmax(beta_superstructure[superstructure_health[n-1]] * X_aug[n]')[superstructure_health[n]]);
        target += log(softmax(beta_substructure[substructure_health[n-1]] * X_aug[n]')[substructure_health[n]]);

      }
    }
  }
}
generated quantities {
  int deck_health_sim[N];
  int superstructure_health_sim[N];
  int substructure_health_sim[N];

  int t_b;
  int n_b;
  int n;
  
  for(b in 1:B){
    t_b = T_b[b];
    n_b = N_b[b];
    
    deck_health_sim[n_b] = deck_health[n_b];
    superstructure_health_sim[n_b] = superstructure_health[n_b];
    substructure_health_sim[n_b] = substructure_health[n_b];
    
    for(t in 1:(t_b-1)){
      n = n_b + t;
      deck_health_sim[n] = categorical_rng(softmax(beta_deck[deck_health_sim[n-1]] * X_aug[n]'));
      superstructure_health_sim[n] = categorical_rng(softmax(beta_superstructure[superstructure_health_sim[n-1]] * X_aug[n]'));
      substructure_health_sim[n] = categorical_rng(softmax(beta_substructure[substructure_health_sim[n-1]] * X_aug[n]'));

    }
  }
}
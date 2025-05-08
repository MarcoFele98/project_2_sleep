data {
  int<lower=0> N; // n observations (edges)
  int<lower=0> N_ind; // number of individuals
  int<lower=0> N_sex_pairs; // number of individuals
  
  vector[N] rho; // response
  vector[N] dom_diff; // difference in dominance of the pair
  int sex_pair[N]; // pair sex composition
  int<lower=0, upper=N_ind> first_ind[N]; 
  int<lower=0, upper=N_ind> second_ind[N];
}

generated quantities {
  
  // fixed effects
  real alpha = normal_rng(0, 1); 
  real beta = normal_rng(0, 1); 
  vector[N_sex_pairs] beta_sex;
  for(i in 1:N_sex_pairs) {
    beta_sex[i] = normal_rng(0, 1); 
  }
  // random effects 
  vector[N_ind] z_gamma;
  for(i in 1:N_ind) {
    z_gamma[i] = normal_rng(0, 1); 
  }
  real sigma_gamma = exponential_rng(1); 
  // error variance
  real sigma_e = exponential_rng(1);
  
  // Generate prior predictive simulations for rho
  real rho_sim[N];
  for(n in 1:N) {
    // Back transform non-centered parametrization
    vector[N_ind] gamma = z_gamma * sigma_gamma; 
    
    // Create vectors of parameters for vecotrized sampling in model block
    vector[N] vec_gamma_first_ind = gamma[first_ind]; // random effects
    vector[N] vec_gamma_second_ind = gamma[second_ind]; // random effects
    vector[N] vec_beta_sex = beta_sex[sex_pair]; // fixed effects
    
    rho_sim = normal_rng(alpha + beta * dom_diff + vec_beta_sex + 
    vec_gamma_first_ind + 
    vec_gamma_second_ind, 
    sigma_e);
  }
}

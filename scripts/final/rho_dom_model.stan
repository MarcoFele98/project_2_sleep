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

parameters {
  // fixed effects
  real alpha;
  real beta;
  vector[N_sex_pairs] beta_sex;
  // random effects
  vector[N_ind] z_gamma;
  real mean_gamma; // hyper parameters
  real<lower=0> sigma_gamma; // hyper parameters
  // error
  real<lower=0> sigma_e;
}

transformed parameters {
  // Back transform non-centered parametrization
  vector[N_ind] gamma = z_gamma * sigma_gamma + mean_gamma;
  
  // Create vectors of parameters for vectorized sampling in model block
  vector[N] vec_gamma_first_ind = gamma[first_ind]; // random effects
  vector[N] vec_gamma_second_ind = gamma[second_ind]; // random effects
  vector[N] vec_beta_sex = beta_sex[sex_pair]; // fixed effects
}

model {
  // priors
  alpha ~ normal(0, 1); // fixed effects
  beta ~ normal(0, 1); // fixed effects
  beta_sex ~ normal(0, 1); // fixed effects
  
  z_gamma ~ normal(0, 1); // random effects (non-centered parametrization)
  sigma_gamma ~ exponential(1); // random effects hyperparameter variance (non-centered parametrization)
  mean_gamma ~ normal(0, 1); // random effects hyperparameter mean (non-centered parametrization)
  sigma_e ~ exponential(1); // error variance
  
  // likelihood
  rho ~ normal(alpha + beta * dom_diff + vec_beta_sex + // fixed effects
  vec_gamma_first_ind + vec_gamma_second_ind, // random effects
  sigma_e);
}

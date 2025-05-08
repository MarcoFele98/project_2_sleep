data {
  int<lower=0> N; // n observations (edges)
  int<lower=0> N_ind; // number of individuals
  int<lower=0> N_sex_pairs; // number of individuals
  
  
  vector[N] rho; // response
  vector[N] proximity; // predictor
  int sex_pair[N]; // pair sex composition
  int<lower=0, upper=N_ind> first_ind[N]; 
  int<lower=0, upper=N_ind> second_ind[N];
  //vector[N] weights; // predictor
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
  for(n in 1:N) {
    target += normal_lpdf(rho[n] | alpha + beta * proximity[n] + beta_sex[sex_pair[n]] + // fixed effects
    gamma[first_ind[n]] + gamma[second_ind[n]], // random effects
    sigma_e); //* weights[n];
  }
}



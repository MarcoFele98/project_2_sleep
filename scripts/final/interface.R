# model 1 rh0 ~ dominance ----
# Data
model_rho_dom_data <- data_model_mechanism_3[, c("peak_rho", "dominance_focus", "dominance_other", "sex_pair", "id_focus", "id_other", "id_pair")
][, .SD[1], by = id_pair]
model_rho_dom_data_stan <- list(N = nrow(model_rho_dom_data),
                                N_ind = length(unique(c(model_rho_dom_data$id_focus,
                                                        model_rho_dom_data$id_other))),
                                N_sex_pairs = 3,
                                rho = as.vector(scale(model_rho_dom_data$peak_rho)),
                                dom_first = as.vector(scale(model_rho_dom_data$dominance_focus)),
                                dom_second = as.vector(scale(model_rho_dom_data$dominance_other)),
                                sex_pair = as.numeric(as.factor(model_rho_dom_data$sex_pair)),
                                first_ind = as.character(model_rho_dom_data$id_focus) |>
                                  factor(levels = unique(c(model_rho_dom_data$id_focus,
                                                           model_rho_dom_data$id_other))) |>
                                  as.numeric(),
                                second_ind = as.character(model_rho_dom_data$id_other) |>
                                  factor(levels = unique(c(model_rho_dom_data$id_focus,
                                                           model_rho_dom_data$id_other))) |>
                                  as.numeric())

# Compile
model_rho_dom <- rstan::stan_model("scripts/stan/model_rho_dom.stan")

# Prior predictive simulations 
posterior <- rstan::sampling(model_rho_dom,
                             data = model_rho_dom_data_stan,
                             chains = 4,
                             iter = 1000) |> 
  as.data.table()

# Diagnostics
bayesplot::mcmc_trace(posterior_draws, pars = "beta")
bayesplot::rhat(posterior_draws)

# Predictions
ggplot(model_rho_dom_data) +
  geom_point(aes(scale(dominance_focus), scale(peak_rho))) +
  geom_point(aes(scale(dominance_other), scale(peak_rho))) +
  geom_abline(aes(intercept = median(posterior$alpha),
                  slope = median(posterior$beta))) 

ggplot(posterior) +
  geom_histogram(aes(beta)) +
  geom_vline(aes(xintercept = quantile(beta, 
                          probs = 0.025))) +
  geom_vline(aes(xintercept = quantile(beta, 
                                       probs = 0.975)))

quantile(posterior$beta, 
         probs = 0.025)
quantile(posterior$beta, 
         probs = 0.975)

ggsave("figures/test/tests.png",
       bg = "white",
       width = 5,
       height = 5)

# model 2 time peak correlation ~ dominance ----
# Data
model_dist_dom_diff_data <- data_model_mechanism_3[, c("gran_median", "dom_diff", "sex_pair", "id_focus", "id_other", "id_pair", "weight_dist")
][, .SD[1], by = id_pair]
model_dist_dom_diff_data_stan <- list(N = nrow(model_dist_dom_diff_data),
                                      N_ind = length(unique(c(model_dist_dom_diff_data$id_focus,
                                                              model_dist_dom_diff_data$id_other))),
                                      N_sex_pairs = 3,
                                      median_distance = as.vector(scale(model_dist_dom_diff_data$gran_median)),
                                      dom_diff = as.vector(scale(model_dist_dom_diff_data$dom_diff  )),
                                      sex_pair = as.numeric(as.factor(model_dist_dom_diff_data$sex_pair)),
                                      first_ind = as.character(model_dist_dom_diff_data$id_focus) |>
                                        factor(levels = unique(c(model_dist_dom_diff_data$id_focus,
                                                                 model_dist_dom_diff_data$id_other))) |>
                                        as.numeric(),
                                      second_ind = as.character(model_dist_dom_diff_data$id_other) |>
                                        factor(levels = unique(c(model_dist_dom_diff_data$id_focus,
                                                                 model_dist_dom_diff_data$id_other))) |>
                                        as.numeric()) # model_dist_dom_diff_data$weight_dist/sum(model_dist_dom_diff_data$weight_dist)

# Compile
model_dist_dom_diff <- rstan::stan_model("scripts/stan/model_dist_dom_diff.stan")

# Prior predictive simulations 
posterior_dist_dom_diff <- rstan::sampling(model_dist_dom_diff,
                             data = model_dist_dom_diff_data_stan,
                             chains = 4,
                             iter = 1000) 

# Diagnostics
bayesplot::mcmc_trace(posterior_dist_dom_diff, pars = "beta")
bayesplot::mcmc_trace(posterior_dist_dom_diff, pars = "alpha")
bayesplot::rhat(posterior_dist_dom_diff)

# Predictions
ggplot(model_dist_dom_diff_data) +
  geom_point(aes(scale(dom_diff), scale(gran_median), 
                 size = weight_dist)) +
  geom_abline(aes(intercept = median(as.data.table(posterior_dist_dom_diff)$alpha),
                  slope = median(as.data.table(posterior_dist_dom_diff)$beta))) 

ggplot(posterior_dist_dom_diff|> 
         as.data.table()) +
  geom_histogram(aes(beta)) +
  geom_vline(aes(xintercept = quantile(beta, 
                                       probs = 0.025))) +
  geom_vline(aes(xintercept = quantile(beta, 
                                       probs = 0.975)))

median(as.data.table(posterior_dist_dom_diff)$beta)
quantile(as.data.table(posterior_dist_dom_diff)$beta,
         probs = 0.025)
quantile(as.data.table(posterior_dist_dom_diff)$beta, 
         probs = 0.975) ()

# model 3 time peak correlation ~ proximity ----
# Data
model_rho_prox_data <- data_model_mechanism_3[, c("proximity", "peak_rho", "sex_pair", "id_focus", "id_other", "id_pair")
][, .SD[1], by = id_pair]
model_rho_prox_data_stan <- list(N = nrow(model_rho_prox_data),
                                 N_ind = length(unique(c(model_rho_prox_data$id_focus,
                                                         model_rho_prox_data$id_other))),
                                 N_sex_pairs = 3,
                                 proximity = as.vector(scale(log(model_rho_prox_data$proximity))),
                                 rho = as.vector(scale(log(model_rho_prox_data$peak_rho))),
                                 sex_pair = as.numeric(as.factor(model_rho_prox_data$sex_pair)),
                                 first_ind = as.character(model_rho_prox_data$id_focus) |>
                                   factor(levels = unique(c(model_rho_prox_data$id_focus,
                                                            model_rho_prox_data$id_other))) |>
                                   as.numeric(),
                                 second_ind = as.character(model_rho_prox_data$id_other) |>
                                   factor(levels = unique(c(model_rho_prox_data$id_focus,
                                                            model_rho_prox_data$id_other))) |>
                                   as.numeric()) # model_dist_dom_diff_data$weight_dist/sum(model_dist_dom_diff_data$weight_dist)

# Compile
model_rho_proximity <- rstan::stan_model("scripts/stan/model_rho_proximity.stan")

# Prior predictive simulations 
posterior_rho_proximity <- rstan::sampling(model_rho_proximity,
                                           data = model_rho_prox_data_stan,
                                           chains = 4,
                                           iter = 4000) 

# Diagnostics
bayesplot::mcmc_trace(posterior_rho_proximity, pars = "beta")
bayesplot::mcmc_trace(posterior_rho_proximity, pars = "alpha")
bayesplot::rhat(posterior_rho_proximity)

# Predictions
ggplot(model_rho_prox_data) +
  geom_point(aes(scale(log(proximity)), scale(log(peak_rho)))) +
  geom_abline(aes(intercept = median(as.data.table(posterior_rho_proximity)$alpha),
                  slope = median(as.data.table(posterior_rho_proximity)$beta))) 

ggplot(posterior_rho_proximity |> 
         as.data.table()) +
  geom_histogram(aes(beta)) +
  geom_vline(aes(xintercept = quantile(beta, 
                                       probs = 0.025))) +
  geom_vline(aes(xintercept = quantile(beta, 
                                       probs = 0.975)))

median(as.data.table(posterior_rho_proximity)$beta)
quantile(as.data.table(posterior_rho_proximity)$beta,
         probs = 0.025)
quantile(as.data.table(posterior_rho_proximity)$beta, 
         probs = 0.975) 


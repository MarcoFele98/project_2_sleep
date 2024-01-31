## Compare with bootstrapped method ----

data_boot <- data.table()
data_n_sleeping_b <- data.table()
for(i in 1:20) { # takes a bit 
  print(i)
  # randomize 
  random_data <- data.table()
  for(prob_sleep in null_model_components$prob_sleep) {
    random_data <- random_data |>
      cbind(rbinom(n = unique(null_model_components$tot), 
                   size = 1,
                   prob = prob_sleep))
  }
  colnames(random_data) <- as.character(null_model_components$id)
  
  random_data[, ":="(number_sleeping = apply(random_data, 1, sum))]
  
  data_n_sleeping_b <- cbind(data_n_sleeping_b, random_data[, ":="(tot = .N)
  ][, .(freq_number_sleeping = .N / unique(tot)), 
    by = list(number_sleeping)
  ])
  
  quourm_sleep_b <- data.frame()
  for(id in null_model_components$id) {
    print(id)
    quourm_sleep_b <- rbind(quourm_sleep_b, 
                            random_data[, .(count = .N),
                                        by = list(number_sleeping, get(id))
                            ][, ":="(id = id)] |>
                              setnames("get", "is_sleeping"))
  }
  
  quourm_sleep_b <- quourm_sleep_b[, ':='(is_sleeping = case_when(is_sleeping == T ~ "Sleeping",
                                                                  is_sleeping == F ~ "Awake"))
  ][is_sleeping == "Sleeping",
    ":="(number_sleeping = number_sleeping - 1) # exclude focus individual
    # likelihood calculations
  ][, ":="(tot_id = sum(count)), 
    by = list(is_sleeping, id)
  ][, ":="(freq_sleep_by_id = count / tot_id), 
    by = list(is_sleeping, id, number_sleeping)
    # posterior calculations
  ][, ":="(tot_sleeping = sum(count)),
    by = list(is_sleeping, number_sleeping)
  ][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
    by = is_sleeping
  ][, ":="(freq_id_by_sleep = count / tot_sleeping), 
    by = list(is_sleeping, number_sleeping, id)
  ][, ":="(check = sum(freq_sleep_by_id)), 
    by = list(is_sleeping, id)
  ][, ":="(check2 = sum(freq_id_by_sleep)), 
    by = list(is_sleeping, number_sleeping)] |>
    merge(dominance_id, on = "id") |>
    mutate(id = factor(id, levels = idsRanked),
           rep = i)
  
  data_boot <- rbind(data_boot, quourm_sleep_b)
}

### A

data_n_sleeping_b_s <- data_n_sleeping_b[, .(freq_number_sleeping_avg = mean(freq_number_sleeping),
                                             freq_number_sleeping_sd = sd(freq_number_sleeping)),
                                         by = number_sleeping]

ggplot() +
  geom_line(data = data_n_sleeping,
            aes(number_sleeping, freq_number_sleeping), 
            linewidth = 2) +
  geom_line(data = null_n_simultaneously_sleeping,
            aes(n, prob_sleep), 
            linewidth = 2, color = "red") +
  geom_line(data = data_n_sleeping_b_s,
            aes(number_sleeping, freq_number_sleeping_avg), 
            color = "blue", lty = "dotted",
            linewidth = 2) +
  background_grid()

### All the rest

data_boot_s <- data_boot[, .(freq_number_sleeping = mean(freq_number_sleeping),
                             freq_number_sleeping_sd = sd(freq_number_sleeping),
                             freq_sleep_by_id = mean(freq_sleep_by_id),
                             freq_sleep_by_id_sd = sd(freq_sleep_by_id)), 
                         by = list(rep, id, number_sleeping)] |>
  merge(info_baboons, 
        on = "id") |>
  mutate()

ggplot(data_boot_s) +
  geom_line(aes(number_sleeping, freq_sleep_by_id, color = dominance, group = id)) +
  geom_ribbon(aes(number_sleeping, 
                  ymin = freq_sleep_by_id - freq_sleep_by_id_sd,
                  ymax = freq_sleep_by_id + freq_sleep_by_id_sd, 
                  color = dominance, group = id)) +
  geom_line(aes(number_sleeping, freq_number_sleeping), linewidth = 1, color = "darkred") +
  geom_ribbon(aes(number_sleeping, 
                  ymin = freq_number_sleeping - freq_number_sleeping_sd,
                  ymax = freq_number_sleeping + freq_number_sleeping_sd), 
              color = "darkred") + facet_wrap(~id) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~id) +
  background_grid()

ggsave(here("figures", "sleep", "quorum", "likelihood.png"),
       height = 3,
       width = 7,
       bg = "white")

ggplot(quourm_sleep_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = dominance, group = id),
            linewidth = 1) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggsave(here("figures", "sleep", "quorum", "posterior.png"),
       height = 3,
       width = 7,
       bg = "white")

ggplot(quourm_sleep_m) +
  geom_point(aes(number_sleeping, freq_id_by_sleep, color = dominance),
             size = 1, alpha = 0.5) +
  geom_smooth(aes(number_sleeping, freq_id_by_sleep, color = dominance, 
                  group = id, weight = tot_sleeping),
              method="glm", #linewidth = 2,
              method.args=list(family="binomial")) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggsave(here("figures", "sleep", "quorum", "posterior_model.png"),
       height = 3,
       width = 7,
       bg = "white")

ggplot(quourm_sleep_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, 
                color = is_sleeping),
            linewidth = 1 #alpha = 0.5
  ) +
  # geom_smooth(aes(number_sleeping, freq_id_by_sleep, color = is_sleeping, 
  #                 group = id, weight = tot_sleeping),
  #             method="glm", #linewidth = 2,
  #             method.args=list(family="binomial")) +
  ylab("Probability") +
  facet_wrap(~id) +
  scale_color_discrete(name = "") +
  background_grid()

ggsave(here("figures", "sleep", "quorum", "posterior_comparison.png"),
       height = 10,
       width = 10,
       bg = "white")





# Bootstrap null model for P(ID|N) not correct ----
data_boot <- data.table()
data_n_sleeping_b <- data.table()
for(i in 1:500) { # takes a bit 
  print(i)
  # randomize 
  random_data <- data.table()
  for(prob_sleep in null_model_components$prob_sleep) {
    random_data <- random_data |>
      cbind(rbinom(n = unique(null_model_components$tot), 
                   size = 1,
                   prob = prob_sleep))
  }
  colnames(random_data) <- as.character(null_model_components$id)
  
  random_data[, ":="(number_sleeping = apply(random_data, 1, sum))]
  
  data_n_sleeping_b <- cbind(data_n_sleeping_b, random_data[, ":="(tot = .N)
  ][, .(freq_number_sleeping = .N / unique(tot)), 
    by = list(number_sleeping)
  ])
  
  quourm_sleep_b <- data.frame()
  for(id in null_model_components$id) {
    print(id)
    quourm_sleep_b <- rbind(quourm_sleep_b, 
                            random_data[, .(count = .N),
                                        by = list(number_sleeping, get(id))
                            ][, ":="(id = id)] |>
                              setnames("get", "is_sleeping"))
  }
  
  quourm_sleep_b <- quourm_sleep_b[is_sleeping == "Sleeping",
                                   ":="(number_sleeping = number_sleeping - 1) # exclude focus individual
                                   # likelihood calculations
  ][, ":="(tot_id = sum(count)), 
    by = list(is_sleeping, id)
  ][, ":="(freq_sleep_by_id = count / tot_id), 
    by = list(is_sleeping, id, number_sleeping)
    # posterior calculations
  ][, ":="(tot_sleeping = sum(count)),
    by = list(is_sleeping, number_sleeping)
  ][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
    by = is_sleeping
  ][, ":="(freq_id_by_sleep = count / tot_sleeping), 
    by = list(is_sleeping, number_sleeping, id)
  ][, ":="(check = sum(freq_sleep_by_id)), 
    by = list(is_sleeping, id)
  ][, ":="(check2 = sum(freq_id_by_sleep)), 
    by = list(is_sleeping, number_sleeping)] |>
    merge(dominance_id, on = "id") |>
    mutate(id = factor(id, levels = idsRanked),
           rep = i)
  
  data_boot <- rbind(data_boot, quourm_sleep_b)
}

quourm_sleep_b_s <- data_boot[, .(freq_id_by_sleep_avg = mean(freq_id_by_sleep),
                                  freq_id_by_sleep_sd = sd(freq_id_by_sleep)),
                              by = list(number_sleeping, id)]

ggplot(quourm_sleep_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = dominance, group = id),
            linewidth = 1) +
  ylab("Probability") +
  geom_line(data = quourm_sleep_b_s,
            aes(number_sleeping, freq_id_by_sleep_avg),
            lty = "dashed", linewidth = 1) +
  geom_ribbon(data = quourm_sleep_b_s,
              aes(number_sleeping, 
                  ymin = freq_id_by_sleep_avg - freq_id_by_sleep_sd,
                  ymax = freq_id_by_sleep_avg + freq_id_by_sleep_sd),
              alpha = 0.3) +
  facet_grid(cols = vars(id), row = vars(is_sleeping)) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

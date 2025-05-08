# Modify ----
combined_m <- copy(combined)[, ":="(number_individuals = apply(.SD[, -c("dateTime")], 1, 
                                                                function(x) {sum(!is.na(x))}),
                                    number_sleeping = apply(.SD[, -c("dateTime")], 1, 
                                                            sum, na.rm = T))
                             ][, ":="(number_awake = number_individuals - number_sleeping)
                               ][, ":="(simpsons_diversity = 1 - number_sleeping * (number_sleeping - 1) / 
                                        (number_individuals * (number_individuals - 1)) + 
                                        (number_individuals - number_sleeping) * (number_individuals - number_sleeping - 1) / 
                                        (number_individuals * (number_individuals - 1)),
                                      freq_sleeping = number_sleeping / number_individuals,
                                      freq_awake = number_awake / number_individuals)
                               ][, ":="(date = date(dateTime),
                                        time = as_hms(dateTime))
                                 ][number_individuals == 12
                                   ][, ":="(is_early = ifelse(time > as_hms("12:00:00"), T, F)) # thing to identify night identity
                                     ][, ":="(id_night = rleid(is_early))
                                 ][, ":="(id_night = ifelse(!(id_night %% 2), 
                                                            id_night - 1, 
                                                            id_night))
                                 ][, -c("is_early")
                                   ][, ":="(date_start_night = min(unique(date))),
                                     by = id_night
                                   ][number_individuals == 12]

# Quorum ----
data_n_awake <- combined_m[number_individuals == 12
][, ":="(tot = .N)
][, .(freq_number_awake = .N / unique(tot)), 
  by = number_awake
][, ":="(check = sum(freq_number_awake),
         avg = sum(freq_number_awake * number_awake))]

data_n_sleeping <- combined_m[number_individuals == 12
][, ":="(tot = .N)
][, .(freq_number_sleeping = .N / unique(tot)), 
  by = number_sleeping
  ][, ":="(check = sum(freq_number_sleeping),
           avg = sum(freq_number_sleeping * number_sleeping))]

quourm_sleep <- data.frame()
for(id in ids) {
  print(id)
  quourm_sleep <- rbind(quourm_sleep, 
                        combined_m[, .(count = .N),
                          by = list(number_sleeping, get(id))
                        ][, ":="(id = id)] |>
                          setnames("get", "is_sleeping"))
}

quourm_sleep_m <- copy(quourm_sleep)[, ':='(id = case_when(id == "Sheb" ~"Shebeleza",
                                                           T ~ id))
][, ':='(is_sleeping = case_when(is_sleeping == T ~ "Sleeping",
                                 is_sleeping == F ~ "Awake"))
][is_sleeping == "Sleeping",
 ":="(number_sleeping = number_sleeping - 1), # exclude focus individual
  #":="(number_sleeping = number_sleeping ), # dont exclude focus individual
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
  merge(dominance_id) |>
  mutate(id = factor(id, levels = idsRanked))

ggplot(quourm_sleep_m) +
  geom_line(aes(number_sleeping, freq_sleep_by_id, color = dominance, group = id)) +
  geom_line(aes(number_sleeping, freq_number_sleeping), linewidth = 1, color = "darkred") +
  # geom_line(data = null_n_simultaneously_sleeping,
  #           aes(n, prob_sleep), 
  #           linewidth = 2, lty = "dotted") +
  # geom_line(data = data_n_sleeping,
  #           aes(number_sleeping, freq_number_sleeping), 
  #           linewidth = 2, lty = "dashed") +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  scale_color_viridis_c(option = "viridis") +
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

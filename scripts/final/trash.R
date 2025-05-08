# freq_by_number_inactive
ggplot(quourm[point == "start"],
       aes(number_inactive, freq_by_number_inactive, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, freq_by_number_inactive, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, freq_by_number_inactive, color = dominance, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, freq_by_number_inactive, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()

# posterior
ggplot(quourm3[point == "start"],
       aes(number_inactive, likelihood, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, posterior3, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm3[point == "start"],
       aes(number_inactive, p, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm3[point == "start"],
       aes(number_inactive, posterior, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm3[point == "start"],
       aes(number_inactive, likelihood, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, likelihood, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, posterior, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, posterior, color = dominance, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, posterior, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()

ggplot(quourm,
       aes(number_inactive, likelihood, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()

# freq_by_ni_id
ggplot(quourm[point == "start"],
       aes(number_inactive, freq_by_ni_id, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, freq_by_ni_id, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, freq_by_ni_id, color = dominance, lty = sex, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, freq_by_ni_id, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()

ggplot(quourm,
       aes(number_inactive, freq_by_ni_id, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()

# cum freq_by_number_inactive
ggplot(quourm[point == "start"],
       aes(number_inactive, cum_freq1, lty = sex, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, cum_freq1, color = dominance, lty = sex, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

# cum freq_by_number_inactive
ggplot(quourm[point == "start"],
       aes(number_inactive, cum_freq2, lty = sex, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, cum_freq2, lty = sex, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, cum_freq2, color = dominance, lty = sex, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, cum_freq2, color = dominance, lty = sex, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, cum_freq2, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()


# analyse quourm response [, .SD[.N], by = repeated_start # takes 30 seconds] P(fall asleep | number sleeping) = P(fall asleep) * P(number sleeping | fall asleep) / p(number sleeping)
quourm <- behaviours_inActive_m[number_individuals == 12
][order(Timestamp, id)
][, ":="(repeated_start = rleid(Timestamp)) # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][, ":="(number_inactive = rep(tail(number_inactive, n = 1), l = .N)), 
  by = repeated_start
][point == "end", ":="(number_inactive = number_inactive + 1) # before it counted the number inactive remaining after stop inactive
][, ":="(tot = .N),
  by = point
][, ":="(tot_id = .N),
  by = list(point, id)
][, ":="(p_data = .N/tot),
  by = list(point, number_inactive)
][, .(likelihood = .N/unique(tot_id),
      p_data = unique(p_data),
      prior = 1/12),
  by = list(point, id, number_inactive)
][, ":="(posterior = likelihood * prior / p_data)] |> 
  merge(dominance_id,
        by = "id") |>
  mutate(id = factor(id, levels = idsRanked))

# check stuff
test <- copy(quourm)[, ":="(check_individual_integration = sum(posterior)), 
                     by = list(point, number_inactive)
][, ":="(check_individual_integration2 = sum(posterior),
         check_individual_integration3 = sum(likelihood)), 
  by = list(point, id)]

# posterior
ggplot(quourm[point == "start"],
       aes(number_inactive, posterior, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, posterior, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"],
       aes(number_inactive, posterior, color = dominance, group = id, lty = sex)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggsave("figures/nice/quourm.png",
       width = 5,
       height = 4,
       bg = "white")

ggplot(quourm[point == "end"],
       aes(number_inactive, posterior, color = dominance, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, posterior, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~ id, ncol = 6) +
  background_grid()

ggsave("figures/nice/quourm_comparison.png",
       width = 10,
       height = 4,
       bg = "white")

# likelihood
ggplot(quourm[point == "start"]) +
  geom_line(aes(number_inactive, likelihood, color = id), linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "end"],
       aes(number_inactive, likelihood, color = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(quourm[point == "start"]) +
  geom_line(aes(number_inactive, likelihood, color = dominance, group = id)) + 
  geom_line(aes(number_inactive, p_data), linewidth = 1, lty = "dashed") + 
  geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggsave("figures/nice/likelihood.png",
       width = 5,
       height = 4,
       bg = "white")

ggplot(quourm[point == "end"],
       aes(number_inactive, likelihood, color = dominance, group = id)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~point) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(quourm,
       aes(number_inactive, likelihood, lty = sex, color = point)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~id) +
  background_grid()


# p_data
ggplot(quourm,
       aes(number_inactive, p_data, color = point)) +
  geom_line(linewidth = 1) +
  background_grid()



# just test  bit
test <- behaviours_inActive_m[number_individuals == 12
][order(Timestamp, id)
][, ":="(repeated_start = rleid(Timestamp)) # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][, ":="(number_inactive = rep(tail(number_inactive, n = 1), l = .N)), 
  by = repeated_start
][point == "end", ":="(number_inactive = number_inactive + 1) # before it counted the number inactive remaining after stop inactive
][, ":="(tot = .N),
  by = point
][, ":="(tot_id = .N,
         prior = .N/tot),
  by = list(point, id)
][, ":="(p_data = .N/tot),
  by = list(point, number_inactive)
][, .(likelihood = .N/unique(tot_id),
      prior = unique(prior),
      #prior = 1/12,
      tot = unique(tot),
      tot_id = unique(tot_id),
      p_data = unique(p_data)),
  by = list(point, id, number_inactive)
][, ":="(posterior = likelihood * prior / p_data)] |> 
  merge(dominance_id,
        by = "id")

ggplot(test[point == "start"]) +
  geom_line(aes(number_inactive, posterior, color = id), linewidth = 1) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

test2 <- behaviours_inActive_m[number_individuals == 12
][order(Timestamp, id)
][, ":="(repeated_start = rleid(Timestamp)) # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][, ":="(number_inactive = rep(tail(number_inactive, n = 1), l = .N)), 
  by = repeated_start
][point == "end", ":="(number_inactive = number_inactive + 1) # before it counted the number inactive remaining after stop inactive
][, ":="(tot = .N),
  by = point
][, ":="(tot_number_inactive = .N,
         prior = .N/tot),
  by = list(point, number_inactive)
][, ":="(p_data = .N/tot),
  by = list(point, id)
][, .(likelihood = .N/unique(tot_number_inactive),
      prior = unique(prior),
      #prior = 1/12,
      tot = unique(tot),
      tot_number_inactive = unique(tot_number_inactive),
      p_data = unique(p_data)),
  by = list(point, id, number_inactive)
][, ":="(posterior = likelihood * prior / p_data)] |> 
  merge(dominance_id,
        by = "id")

ggplot(test2[point == "start"]) +
  geom_line(aes(number_inactive, posterior, color = id), linewidth = 1) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

test3 <- behaviours_inActive_m[number_individuals == 12
][order(Timestamp, id)
][, ":="(repeated_start = rleid(Timestamp)) # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][, ":="(number_inactive = rep(tail(number_inactive, n = 1), l = .N)), 
  by = repeated_start
][point == "end", ":="(number_inactive = number_inactive + 1) # before it counted the number inactive remaining after stop inactive
][, ":="(tot_number_inactive = .N),
  by = list(point, number_inactive)
][, .(posterior = .N/unique(tot_number_inactive)),
  by = list(point, id, number_inactive)
] |> 
  merge(dominance_id,
        by = "id")

ggplot(test3[point == "start"]) +
  geom_line(aes(number_inactive, posterior, color = id), linewidth = 1) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()




quourm_dominance_control <- behaviours_inActive_m[number_individuals == 12  & !is.na(adding_dominance)
                                                  # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][point == "end", ":="(number_inactive_excluding_dominance = number_inactive_excluding_dominance + 1,
                       dominance_inactive = dominance_inactive - adding_dominance) # before it counted the number inactive remaining after stop inactive. adding dominance is negative in this case so I have to minus to plus
][point == "start", ":="(number_inactive_excluding_dominance = number_inactive_excluding_dominance - 1,
                         dominance_inactive = dominance_inactive - adding_dominance) # before it counted the number inactive remaining after start beeing inactive
  # calculate variable of interest
][, ":="(avg_dominance_inactive = dominance_inactive / number_inactive_excluding_dominance)
  # take car of repeated start
][order(Timestamp, id)
  # when multiple individuals have the same start or end of behaviour the real number of inactive individuals and avg_dominance is going to be the last in order
][, ":="(repeated_start = rleid(Timestamp)) 
][, ":="(avg_dominance_inactive = rep(tail(avg_dominance_inactive, n = 1), l = .N),
         number_inactive_excluding_dominance = rep(tail(number_inactive_excluding_dominance, n = 1), l = .N)), 
  by = repeated_start
  # calculate values
][, ":="(tot = .N),
  by = list(point, number_inactive_excluding_dominance)
][, ":="(tot_id = .N,
         prior = .N/tot),
  by = list(point, number_inactive_excluding_dominance, id)
][, ":="(p_data = .N/tot),
  by = list(point, number_inactive_excluding_dominance, avg_dominance_inactive)
][, .(likelihood = .N/unique(tot_id),
      prior = unique(prior),
      #prior = 1/12,
      tot = unique(tot),
      tot_id = unique(tot_id),
      p_data = unique(p_data)),
  by = list(point, number_inactive_excluding_dominance, id, avg_dominance_inactive)
][, ":="(posterior = likelihood * prior / p_data)] |> 
  merge(dominance_id,
        by = "id") |>
  mutate(id = factor(id, levels = idsRanked))


quourm_dominance <- behaviours_inActive_m[number_individuals == 12 & !is.na(adding_dominance)
                                          # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
][point == "end", ":="(number_inactive_excluding_dominance = number_inactive_excluding_dominance + 1,
                       dominance_inactive = dominance_inactive - adding_dominance) # before it counted the number inactive remaining after stop inactive. adding dominance is negative in this case so I have to minus to plus
][point == "start", ":="(number_inactive_excluding_dominance = number_inactive_excluding_dominance - 1,
                         dominance_inactive = dominance_inactive - adding_dominance) # before it counted the number inactive remaining after start beeing inactive
  # calculate variable of interest
][, ":="(avg_dominance_inactive = dominance_inactive / number_inactive_excluding_dominance)
  # take car of repeated start
][order(Timestamp, id)
][, ":="(repeated_start = rleid(Timestamp)) 
][, ":="(avg_dominance_inactive = rep(tail(avg_dominance_inactive, n = 1), l = .N)), 
  by = repeated_start
  # calculate probabilities
][, ":="(tot = .N),
  by = point
][, ":="(tot_id = .N,
         prior = .N/tot),
  by = list(point, id)
][, ":="(p_data = .N/tot),
  by = list(point, avg_dominance_inactive)
][, .(likelihood = .N/unique(tot_id),
      prior = unique(prior),
      #prior = 1/12,
      tot = unique(tot),
      tot_id = unique(tot_id),
      p_data = unique(p_data)),
  by = list(point, id, avg_dominance_inactive)
][, ":="(posterior = likelihood * prior / p_data)] |> 
  merge(dominance_id,
        by = "id") |>
  mutate(id = factor(id, levels = idsRanked))


# posterior
ggplot(quourm_dominance[point == "start"]) +
  geom_line(aes(avg_dominance_inactive, posterior, color = dominance, group = id, lty = sex), linewidth = 1) + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_dominance[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, posterior, color = dominance, group = id, lty = sex), linewidth = 1) + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_dominance) +
  geom_line(aes(avg_dominance_inactive, posterior, lty = sex, color = point), linewidth = 1) + 
  facet_wrap(~ id, ncol = 6) +
  background_grid()

ggplot(quourm_dominance[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, posterior, lty = sex)) + 
  # geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  facet_wrap(~id) +
  background_grid()

# likelihood
ggplot(quourm_dominance[point == "start"]) +
  geom_line(aes(avg_dominance_inactive, likelihood, color = dominance, group = id, lty = sex)) + 
  geom_line(aes(avg_dominance_inactive, p_data), color = "darkred", linewidth = 1, lty = "dashed") + 
  # geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_dominance[point == "start"]) +
  geom_line(aes(avg_dominance_inactive, likelihood, group = point, lty = sex)) + 
  # geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  facet_wrap(~id) +
  background_grid()

ggplot(quourm_dominance[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, likelihood, lty = sex)) + 
  # geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  facet_wrap(~id) +
  background_grid()




# Bootstrap ----
# to standardize such that out-degree is the same accounting for the fact that some individuals sleep less 
# 
# observations <- apply(combined_m[number_individuals == 12][, ids, with = F], 2, 
#                       FUN = table, useNA = "always") |>
#   rbind(tot_observations = apply(combined_m[number_individuals == 12][, ids, with = F], 2, FUN = function(x){sum(!is.na(x))}))
#   
# min_obs <- min(observations[2,]) #  minimum number of times an individual was seen sleeping (it is Lola)
# 
# boot_sleep <- combined_m[number_individuals == 12][, ids, with = F]
# for(id in ids) {
#   is_sleeping <- which(combined_m[number_individuals == 12][, ids, with = F] == T) |>
#     sample(observations[2, id] - min_obs, replace = F)
#   boot_sleep[is_sleeping, id] <- NA
# }
# 
# apply(boot_sleep, 2, FUN = sum, na.rm = T)
# 
# 
# id_behvaiour <- combined_m[, ids, with = F]
# result_syncrony <- data.frame()
# for(i in 1:length(idsRanked)) {
#   for(j in 1:length(idsRanked)) {
#     #if(i != j) {
#     print(i)
#     print(j)
#     result_syncrony <- rbind(result_syncrony, 
#                              data.frame(id = colnames(id_behvaiour)[i], id_pair = colnames(id_behvaiour)[j]) |>
#                                cbind((id_behvaiour[, ..i] + id_behvaiour[, ..j])[, .(tot = .N), 
#                                                                                  by = eval(colnames(id_behvaiour)[i])] |>
#                                        setnames(old = colnames(id_behvaiour)[i], 
#                                                 new = "syncrony")))
#     #}
#   }
# }
# 
# result_syncrony_m <- as.data.table(result_syncrony)[!is.na(syncrony), ":="(coocurrences = sum(tot)), 
#                                                     by = list(id, id_pair)][, ":="(association = tot/coocurrences)
#                                                     ][, ':='(id = case_when(id == "Sheb" ~"Shebeleza",
#                                                                             T ~ id))
#                                                     ][, ':='(id_pair = case_when(id_pair == "Sheb" ~"Shebeleza",
#                                                                                  T ~ id_pair))]



# Quorum ----
data_n_sleeping <- combined_m[number_individuals == 12
][, ":="(tot = .N)
][, .(freq_number_sleeping = .N / unique(tot)), 
  by = number_sleeping]

quourm_sleep <- data.frame()
for(id in ids) {
  print(id)
  quourm_sleep <- rbind(quourm_sleep, 
                        combined_m[number_individuals == 12
                        ][, .(count = .N),
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
  mutate(id = factor(id, levels = idsRanked))

ggplot(quourm_sleep_m) +
  geom_line(aes(number_sleeping, freq_sleep_by_id, color = dominance, group = id)) +
  geom_line(aes(number_sleeping, freq_number_sleeping), linewidth = 1, color = "darkred") +
  geom_line(data = null_n_simultaneously_sleeping,
            aes(n, prob_sleep), 
            linewidth = 2, lty = "dotted") +
  geom_line(data = data_n_sleeping,
            aes(number_sleeping, freq_number_sleeping), 
            linewidth = 2, lty = "dashed") +
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



null_id_n_sleeping <- expand.grid(id = null_model_components$id, number_sleeping = 0:11) |> # exclude the focus individual so 11 not 12
  mutate(null_probability = 0, is_sleeping = "Sleeping") |> as.data.table() |>
  merge(null_model_components, on = "id") |>
  arrange(number_sleeping, id)
null_id_n_awake <- expand.grid(id = null_model_components$id, number_sleeping = 0:11) |> # exclude the focus individual so 11 not 12
  mutate(null_probability = 0, is_sleeping = "Awake") |> as.data.table() |>
  merge(null_model_components, on = "id") |>
  arrange(number_sleeping, id)
for(i in 1:nrow(null_id_n_sleeping)) { # every possible n choose i, meaning combinations of i baboons simultaneously sleeping
  focus_id <- null_id_n_sleeping$id[i] |> as.numeric()
  null_id_n_sleeping[i, "null_probability"] <- sum(apply(combn((1:12)[-focus_id], # create all combinations of others individuals sleeping without considering focus individual
                                                               null_id_n_sleeping$number_sleeping[i]), 
                                                         2, # in the rows is the individuals selected, in the colums one specific combination of individuals
                                                         FUN = function(chosen_individuals_sleeping) { 
                                                           #browser()
                                                           prob_ind_sleep[focus_id] * # probability individual sleeps
                                                             prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
                                                             ) 
                                                         }))
  null_id_n_awake[i, "null_probability"] <- sum(apply(combn((1:12)[-focus_id], # create all combinations of others individuals sleeping without considering focus individual
                                                            null_id_n_awake$number_sleeping[i]), 
                                                      2, # in the rows is the individuals selected, in the colums one specific combination of individuals
                                                      FUN = function(chosen_individuals_sleeping) { 
                                                        #browser()
                                                        prob_ind_awake[focus_id] * # probability individual sleeps
                                                          prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
                                                          ) 
                                                      }))
}

quourm_sleep_null_model <- quourm_sleep_m |>
  merge(rbind(null_id_n_sleeping, null_id_n_awake)[, c("id", "number_sleeping", "null_probability", "is_sleeping")],
        by = c("id", "number_sleeping", "is_sleeping"))

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = dominance, group = id),
            linewidth = 1) +
  geom_line(aes(number_sleeping, null_probability, color = dominance, group = id),
            linewidth = 1, lty = "dashed") +
  ylab("Probability") +
  scale_color_viridis_c(option = "viridis") +
  facet_grid(cols = vars(id), row = vars(is_sleeping)) +
  background_grid()



# use Bayes theorem: P(sleeping|N) = (P(sleeping)P(N|sleeping))/P(N)
null_id_n_sleeping[i, "null_probability"] <- prob_ind_sleep[focus_id] * # P(sleeping)
  sum(apply(combn((1:12)[-focus_id], # P(N|sleeping)
                  Nsleep-1), 
            2, 
            FUN = function(chosen_individuals_sleeping) { 
              prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
              ) 
            })) /
  sum(apply(combn((1:12), # P(N)
                  Nsleep), 
            2, 
            FUN = function(chosen_individuals_sleeping) { 
              prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[chosen_individuals_sleeping]]
              ) 
            }))

# null_id_n_awake[i, "null_probability"] <- prob_ind_awake[focus_id] * # P(awake)
#   sum(apply(combn((1:12)[-focus_id], # P(N|awake)
#                   Nawake), 
#             2, 
#             FUN = function(chosen_individuals_sleeping) { 
#               prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
#               ) 
#             })) /
#   sum(apply(combn((1:11), # P(N)
#                   Nawake), 
#             2, 
#             FUN = function(chosen_individuals_sleeping) { 
#               prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[chosen_individuals_sleeping]]
#               ) 
#             }))


null_id_n_awake <- expand.grid(id = null_model_components$id, number_sleeping = 0:11) |> # possible number of individuals sleeping goes from 0 to 11. It is impossible that P(N = 12|awake)
  mutate(null_probability = 0, is_sleeping = "Awake") |> as.data.table() |>
  merge(null_model_components, on = "id") |>
  arrange(number_sleeping, id)


quourm_sleep_null_model <- quourm_sleep_m |>
  merge(rbind(null_id_n_sleeping, null_id_n_awake)[, c("id", "number_sleeping", "null_probability", "is_sleeping")],
        by = c("id", "number_sleeping", "is_sleeping"))

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = dominance, group = id),
            linewidth = 1) +
  geom_line(aes(number_sleeping, null_probability, color = dominance, group = id),
            linewidth = 1, lty = "dashed") +
  ylab("Probability") +
  scale_color_viridis_c(option = "viridis") +
  facet_grid(cols = vars(id), row = vars(is_sleeping)) +
  background_grid()







# Calculate synchrony ----
id_behvaiour <- combined_m[number_individuals == 12][, ids, with = F]
result_syncrony <- data.frame()
for(i in 1:length(idsRanked)) {
  print(i)
  for(j in 1:length(idsRanked)) {
    print(j)
    #if(i != j) {
    result_syncrony <- rbind(result_syncrony, 
                             data.frame(id = colnames(id_behvaiour)[i], id_pair = colnames(id_behvaiour)[j]) |>
                               cbind((id_behvaiour[, ..i] + id_behvaiour[, ..j])[, .(tot = .N), 
                                                                                 by = eval(colnames(id_behvaiour)[i])] |>
                                       setnames(old = colnames(id_behvaiour)[i], 
                                                new = "syncrony")))
    #}
  }
}

result_syncrony_m <- (as.data.table(result_syncrony)[!is.na(syncrony), ":="(coocurrences = sum(tot)), 
                                                     by = list(id, id_pair)
][, ":="(association = tot/coocurrences)
][, ':='(id = case_when(id == "Sheb" ~"Shebeleza",
                        T ~ id))
][, ':='(id_pair = case_when(id_pair == "Sheb" ~"Shebeleza",
                             T ~ id_pair))
] |> #[id != id_pair][order(syncrony, association, id, id_pair) # keep only one of the symmetric interactions that are present][seq(1, nrow(result_syncrony), by = 2)]
  merge(dominance_id |> rename(dominance_id = dominance, sex_id = sex), 
        by = "id",
        all.x = TRUE) |>
  merge(dominance_id |> rename(dominance_pair = dominance, sex_pair = sex), 
        by.x = "id_pair",
        by.y = "id",
        all.x = TRUE))[, ":="(id = factor(id, levels = idsRanked),
                              id_pair = factor(id_pair, levels = idsRanked))
        ][, ":="(test = sum(association)),
          by = list(id, id_pair)
        ]


network_matrix_sleep <- result_syncrony_m[syncrony == 2][, c("id", "id_pair", "association")] |>
  pivot_wider(names_from = "id_pair",
              values_from = "association")


# Network ----
# sleep
net_sleep <- as_tbl_graph(result_syncrony_m[syncrony == 2 & id != id_pair
][, c("id", "id_pair", "association")] |> 
  rename(from = id, to = id_pair)) |>
  activate(nodes) |>
  mutate(node_number = row_number()) |>
  left_join(dominance_id |> rename(name = id), by = "name")

ggraph(net_sleep |>   
         activate(edges)  |>
         filter(association > quantile(association, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    aes(color = as.factor(association),
        width = association)) + 
  scale_edge_color_manual(values = viridis(n = 65, option = "C")) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      # fill = as.factor(dominance),
      size = dominance), 
    #size = 8,
    fill = "black",
    color = "black", shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  #scale_fill_viridis_d(option = "viridis") +
  geom_node_label(
    aes(label = name),
    nudge_y = 0.05
  ) +
  ggtitle("Sleep") +
  theme(legend.position = 'none')

ggsave(here("figures", "sleep", "syncrony", "net_sleep_syncro.png"),
       height = 10,
       width = 10,
       bg = "white")

# Awake
net_awake <- as_tbl_graph(result_syncrony_m[syncrony == 0 & id != id_pair
][, c("id", "id_pair", "association")] |> 
  rename(from = id, to = id_pair)) |>
  activate(nodes) |>
  mutate(node_number = row_number()) |>
  left_join(dominance_id |> rename(name = id), by = "name")

ggraph(net_awake |>   
         activate(edges)  |>
         filter(association > quantile(association, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    aes(color = as.factor(association),
        width = association)) + 
  scale_edge_color_manual(values = viridis(n = 65, option = "C")) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      # fill = as.factor(dominance),
      size = dominance), 
    #size = 8,
    fill = "black",
    color = "black", shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  #scale_fill_viridis_d(option = "viridis") +
  geom_node_label(
    aes(label = name),
    nudge_y = 0.05
  ) +
  ggtitle("Awake") +
  theme(legend.position = 'none')

ggsave(here("figures", "sleep", "syncrony", "net_awake_syncro.png"),
       height = 10,
       width = 10,
       bg = "white")

# Syncro
net_asyncrony <- as_tbl_graph(result_syncrony_m[syncrony == 1 & id != id_pair
][, c("id", "id_pair", "association")
][, ":="(association = 1 - association)] |> 
  rename(from = id, to = id_pair)) |>
  activate(nodes) |>
  mutate(node_number = row_number()) |>
  left_join(dominance_id |> rename(name = id), by = "name")

ggraph(net_asyncrony |>   
         activate(edges)  |>
         filter(association > quantile(association, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    aes(color = as.factor(association),
        width = association)) + 
  scale_edge_color_manual(values = viridis(n = 65, option = "C")) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      # fill = as.factor(dominance),
      size = dominance), 
    #size = 8,
    fill = "black",
    color = "black", shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  #scale_fill_viridis_d(option = "viridis") +
  geom_node_label(
    aes(label = name),
    nudge_y = 0.05
  ) +
  ggtitle("Syncro") +
  theme(legend.position = 'none')

ggsave(here("figures",  "sleep", "syncrony", "net_syncro_syncro.png"),
       height = 10,
       width = 10,
       bg = "white")

# Stats and plots ----
ggplot(result_syncrony_m[id != id_pair]) +
  geom_point(aes(dominance_pair, association, color = as.factor(syncrony))) +
  geom_smooth(aes(dominance_pair, association, color = as.factor(syncrony), weight = coocurrences),
              method="glm",
              method.args=list(family="binomial")) +
  scale_color_discrete(labels = c("Both awake", "Alternated", "Both sleeping"),
                       name = "") +
  facet_wrap(~id) +
  background_grid()

ggsave(here("figures", "sleep", "syncrony", "association~dominance_pair_TIMES_syncrony_type.png"),
       height = 10,
       width = 10,
       bg = "white")

ggplot(result_syncrony_m[id != id_pair & syncrony == 1
][, ":="(association = 1 - association)]) +
  geom_point(aes(dominance_pair, association, color = dominance_id, group = id)) +
  geom_smooth(aes(dominance_pair, association, color = dominance_id, group = id, weight = coocurrences),
              method="glm", linewidth = 2,
              method.args=list(family="binomial")) +
  scale_color_viridis_c() +
  facet_wrap(~sex_id) +
  background_grid()

ggsave(here("figures", "sleep", "syncrony", "association~dominance_pair_TIMES_dominance_id.png"),
       height = 5,
       width = 7,
       bg = "white")

glm_coordination <- glm(data = result_syncrony_m[id != id_pair][syncrony == 1],
                        association ~ dominance_id * dominance_pair + sex_id + sex_pair,
                        weights = coocurrences)
summary(glm_coordination)

ggplot(result_syncrony_m[id != id_pair & syncrony == 1][, ":="(association = 1 - association)]) +
  geom_tile(aes(id, id_pair, fill = association)) +
  scale_fill_viridis(option = "viridis") +
  background_grid()





# Load datasets ----
## For random forest ----
# for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
#   print(file)
#   assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
#          fill_empty_time(as.data.table(readRDS(here::here("data", # fill each second 
#                                                           "Baboon behaviour from acceleration (1)", 
#                                                           file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
#   
#   
# }
## For mean.VeBDAs ----
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
         fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second 
                                                                      "Baboon behaviour from acceleration (1)", 
                                                                      file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
  
  
}
allForagingFileNames <- grep("foraging", ls(), value = TRUE)

# Filter datasets ----
env <- .GlobalEnv
## For random forest ----
# for(obj in allForagingFileNames) {
#   print(obj)
#   env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
#                              rename(dateTime = Timestamp) |>
#                              merge(data_astronomical[, c("sunrise", "sunset", "date")],
#                                    all.x = TRUE,
#                                    by.x = "Date", 
#                                    by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
#                                    ][is_night == T][, -c("is_night")
#                                                     ][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))]
# }
## For mean.VeBDAs ----
for(obj in allForagingFileNames) {
  print(obj)
  env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
                   rename(dateTime = Timestamp) |>
                   merge(data_astronomical[, c("sunrise", "sunset", "date")],
                         all.x = TRUE,
                         by.x = "Date", 
                         by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
                         ][is_night == T][, -c("is_night")
                         ][, ":="(inactive = if_else(mean.VeDBAs < 0.03, T, F))]
}
rm(env)


# Load datasets ----
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
         fill_empty_time(as.data.table(readRDS(here::here("data", # fill each second 
                                                          "Baboon behaviour from acceleration (1)", 
                                                          file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
  
  
}
allForagingFileNames <- grep("foraging", ls(), value = TRUE)

# Filter datasets ----
env <- .GlobalEnv
for(obj in allForagingFileNames) {
  print(obj)
  env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
                   rename(dateTime = Timestamp) |>
                   merge(data_astronomical[, c("sunrise", "sunset", "date")],
                         all.x = TRUE,
                         by.x = "Date", 
                         by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
                         ][Date %in% unique(combined_m[number_individuals == 12][["date"]])][, -c("is_night")
                         ][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))]
}



# Combine datasets ----
env <- .GlobalEnv
combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "inactive")] |>
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  id_name <- str_replace(obj, "foraging", "")
  combined <- env[[obj]][is_night == T][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined, by = "dateTime", all = TRUE)
}
rm(env)



# Combine datasets ----
env <- .GlobalEnv
combined_night <- env[[allForagingFileNames[1]]][, c("dateTime", "inactive")] |>
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  id_name <- str_replace(obj, "foraging", "")
  combined_night <- env[[obj]][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined_night, by = "dateTime", all = TRUE)
}
rm(env)

# Modify ----
combined_night_m <- copy(combined_night)[, ":="(number_individuals = apply(.SD[, -c("dateTime")], 1, 
                                                                           function(x) {sum(!is.na(x))}),
                                                number_sleeping = apply(.SD[, -c("dateTime")], 1, 
                                                                        sum, na.rm = T))
][, ":="(simpsons_diversity = 1 - number_sleeping * (number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)) + 
           (number_individuals - number_sleeping) * (number_individuals - number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)),
         freq_sleeping = number_sleeping / number_individuals)
][, ":="(date = date(dateTime),
         time = as_hms(dateTime))
]




## All togehter
night_sleep <- copy(dominance_id)[, ":="(sleep = apply((combined_m[, ids, with = F] |>
                                                          rename(Shebeleza = Sheb))[, idsRanked, with = F],
                                                       2, FUN = sum),
                                         tot = apply((combined_m[, ids, with = F] |>
                                                        rename(Shebeleza = Sheb))[, idsRanked, with = F],
                                                     2, FUN = length))
][, ":="(freq_sleep = sleep / tot,
         time_sleep = sleep / (60*60) / 25)]

ggplot(night_sleep) +
  geom_point(aes(dominance, freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, freq_sleep, 
                  group = sex,
                  lty = sex,
                  weight = tot ), 
              method = "glm", color = "red",
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggsave(here("figures", "sleep", "freq-sleep~dominance.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 5,
       bg = "white")

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "freq_sleep~dominance_night.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 5,
       bg = "white")

ggplot(night_sleep) +
  geom_point(aes(dominance, time_sleep, color = id), size = 3) +
  # geom_smooth(aes(dominance, freq_sleep, 
  #                 #group = sex,
  #                 #lty = sex,
  #                 weight = tot ), 
  #             method = "glm", color = "red",
  #             method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()





generative_data_prep <- combined_m[, c(ids, "dateTime", "id_night"), with = F
                                   # next is to find consecutive moments to calculate transitions
][, ":="(diff = c(NA, diff(dateTime)))
][, ":="(unit = rleid(diff))
][, ":="(is_transition = ifelse(diff == 1, T, F))
][is_transition == T
][, -c("is_transition", "diff")
  # create group sleep configuration
][, ":="(group_configuration = apply(.SD[, ids, with = F] * 1, 
                                     1, 
                                     paste, collapse = ""))]

id <- "Trinity"
generative_data <- rename(generative_data_prep[, c(id, "unit", "group_configuration"), with = F], 
                          state = id
)[, ":="(next_state = c(tail(.SD[[1]], -1), NA)), 
  by = unit
][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                state == 1 & next_state == 0 ~ "p_sa",
                                state == 0 & next_state == 0 ~ "p_aa",
                                state == 0 & next_state == 1 ~ "p_as",
                                T ~ NA))
][, ":="(tot_configuration = .N),
  by = c("group_configuration")
][, .(tot_transition = .N,
      probability = .N / unique(tot_configuration),
      tot_configuration = unique(tot_configuration)),
  by = c("state", "group_configuration", "transition")
][, ":="(tot_state = sum(tot_configuration)), 
  by = state
][, ":="(id = id)
][, c("id", "state", "tot_state", "group_configuration", "tot_configuration", "transition", "tot_transition", "probability")]

for(id in ids[-1]) {
  print(id)
  generative_data <- rbind(generative_data,
                           rename(generative_data_prep[, c(id, "unit", "group_configuration"), with = F], 
                                  state = id
                           )[, ":="(next_state = c(tail(.SD[[1]], -1), NA)), 
                             by = unit
                           ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                                           state == 1 & next_state == 0 ~ "p_sa",
                                                           state == 0 & next_state == 0 ~ "p_aa",
                                                           state == 0 & next_state == 1 ~ "p_as",e
                                                           T ~ NA))
                           ][, ":="(tot_configuration = .N),
                             by = c("group_configuration")
                           ][, .(tot_transition = .N,
                                 probability = .N / unique(tot_configuration),
                                 tot_configuration = unique(tot_configuration)),
                             by = c("state", "group_configuration", "transition")
                           ][, ":="(tot_state = sum(tot_configuration)), 
                             by = state
                           ][, ":="(id = id)
                           ][, c("id", "state", "tot_state", "group_configuration", "tot_configuration", "transition", "tot_transition", "probability")])
}

generative_data[, ":="(number_sleeping = sapply(strsplit(group_configuration, ""), # find how many are sleeping
                                                function(x) { 
                                                  sum(as.numeric(x))
                                                }))
][, ":="(sleeping_numerator = sum(tot_transition)),
  by = list(id, number_sleeping, transition)
][, ":="(sleeping_denominator = sum(tot_transition)),
  by = list(id, number_sleeping, state)
][, ":="(sleeping_probability = sleeping_numerator / sleeping_denominator) # normalize just based on how many are sleeping 
][, ":="(independent_numerator = sum(tot_transition)),
  by = list(id, transition)
][, ":="(independent_denominator = sum(tot_transition)),
  by = list(id, state)
][, ":="(independent_probability = independent_numerator / independent_denominator) # normalize just based on how many are sleeping 
] 

ggplot(generative_data[transition %in% c("p_aa", "p_ss")]) +
  geom_point(aes(probability, 
                 sleeping_probability, 
                 color = number_sleeping)) +
  facet_grid(cols = vars(id),
             rows = vars(state)) +
  background_grid()

ggsave(here("figures", "test", "generative_model.png"),
       width = 40,
       height = 10,
       bg = "white")

ggplot(generative_data[transition %in% c("p_aa", "p_ss")]) +
  geom_point(aes(probability, 
                 independent_probability)) +
  facet_grid(cols = vars(id),
             rows = vars(state)) +
  background_grid()

ggsave(here("figures", "test", "generative_model_2.png"),
       width = 40,
       height = 10,
       bg = "white")




# find event probabilities
change_probability <- sapply(1:12, FUN = function(id_number) {
  if(current_group_configuration[id_number]) {
    calculate_proportion_dominance_sleep_excluding_focus <- (current_generative_data["sum_dominance_sleep"] - dominance_ranked[id_number]) / sum(dominance_ranked[-id_number]) 
    #calculate_proportion_dominance_sleep_excluding_focus <- (current_generative_data["number_sleeping"] - 1) / 11
    
    return(predictions[id == idsRanked[id_number] &
                         number_sleeping_excluding_focus == current_generative_data["number_sleeping"] - 1 &
                         (proportion_dominance_sleep_excluding_focus > (calculate_proportion_dominance_sleep_excluding_focus - 0.001)) & # do this way for numeric stability
                         (proportion_dominance_sleep_excluding_focus < (calculate_proportion_dominance_sleep_excluding_focus + 0.001)),
                       "p_sa"][[1]])
  }
  else {
    calculate_proportion_dominance_sleep_excluding_focus <- current_generative_data["sum_dominance_sleep"] / sum(dominance_ranked[-id_number]) 
    #calculate_proportion_dominance_sleep_excluding_focus <- current_generative_data["number_sleeping"] / 11
    
    return(predictions[id == idsRanked[id_number] &
                         number_sleeping_excluding_focus == current_generative_data["number_sleeping"] &
                         (proportion_dominance_sleep_excluding_focus > (calculate_proportion_dominance_sleep_excluding_focus - 0.001)) & # do this way for numeric stability
                         (proportion_dominance_sleep_excluding_focus < (calculate_proportion_dominance_sleep_excluding_focus + 0.001)),
                       "p_as"][[1]])
  }
})


change_probability <- sapply(1:12, FUN = function(id_number) {
  if(current_group_configuration[id_number]) {
    print(idsRanked[id_number])
    return(predict(m_rest, 
                   newdata = data.frame(id = idsRanked[id_number], 
                                        id_night = 1111, # with unknown new level best guess is 0 effect
                                        dominance = dominance_ranked[id_number],
                                        number_sleeping_excluding_focus = current_generative_data["number_sleeping"] - 1,
                                        proportion_dominance_sleep_excluding_focus = (current_generative_data["sum_dominance_sleep"] - dominance_ranked[id_number]) / 
                                          sum(dominance_ranked[-id_number]),
                                        precipitation_mm = 0,
                                        min_temp = 6.713561, # which is mean(mc_data_covariates_rest$min_temp),
                                        phase = 0),
                   type = "response"))
  }
  else {
    print(idsRanked[id_number])
    return(predict(m_active, 
                   newdata = data.frame(id = idsRanked[id_number], 
                                        id_night = 1111, # with unknown new level best guess is 0 effect 
                                        dominance = dominance_ranked[id_number],
                                        number_sleeping_excluding_focus = current_generative_data["number_sleeping"],
                                        proportion_dominance_sleep_excluding_focus = current_generative_data["sum_dominance_sleep"] / 
                                          sum(dominance_ranked[-id_number]),
                                        precipitation_mm = 0,
                                        min_temp = 6.713561, # which is mean(mc_data_covariates_rest$min_temp),
                                        phase = 0),
                   type = "response"))
  }
})



ggplot(mc_data_covariates[transition %in% c("p_aa", "p_ss")]) +
  geom_point(aes(dominance, freq_transition, color = id_night)) +
  # geom_smooth(aes(dominance, freq_transition, color = id_night,
  #                 group = id_night, weight = tot_state),
  #             method = "glm",
  #             method.args = list(family = "binomial")) +
  # geom_smooth(aes(dominance, freq_transition, 
  #                 group = state, weight = tot_state),
  #             method = "glm", color = "red",
  #             method.args = list(family = "binomial")) +
  facet_wrap(~state) +
  background_grid()







# markov chain analysis ----
## Verify markov property ----
verifyMarkovProperty(combined_m$Sheb, verbose = TRUE)
verifyHomogeneity(inputList=kullback, verbose = TRUE)

test <- combined_m[, "Sheb"
][, ":="(continuous_behaviour = rleid(Sheb))
][, ":="(length_continuous_behaviour = rep(rle(continuous_behaviour)[[1]],
                                           rle(continuous_behaviour)[[1]]))]

ggplot(test) +
  geom_histogram(aes(length_continuous_behaviour)) +
  facet_wrap(~Sheb)

ggplot(test) +
  geom_histogram(aes(length_continuous_behaviour)) +
  scale_y_log10() +
  facet_wrap(~Sheb)

ggplot(test) +
  geom_histogram(aes(log(length_continuous_behaviour))) +
  facet_wrap(~Sheb)


## Create chain ----
markov_data <- data.table(id = c(), previous_state = c(), next_state = c())
for(id in ids) {
  print(id)
  markov_data <- rbind(markov_data,
                       (table(previous_state = combined_m[[id]], 
                              next_state = c(combined_m[[id]][-1], NA),
                              useNA = "always") |>
                          as.data.table())[, ":="(id = id)])
}

markov_data_m <- markov_data[, ":="(tot = sum(N),
                                    id = case_when(id == "Sheb" ~ "Shebeleza",
                                                   T ~ id)), 
                             by = list(id, previous_state)
][, ":="(probability = N/tot,
         id = factor(id, levels = idsRanked),
         transition = case_when(previous_state == T & next_state == T ~ "p1", # keep resting
                                previous_state == T & next_state == F ~ "p3", # wake up
                                previous_state == F & next_state == T ~ "p2", # fall asleep
                                previous_state == F & next_state == F ~ "p4"))] |> # remain active
  merge(dominance_id, by = "id")

ggplot(markov_data_m[!is.na(transition)]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  background_grid()

ggplot(markov_data_m[transition %in% c("p1", "p4")]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  background_grid()

ggplot(markov_data_m[transition %in% c("p2", "p3")]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  background_grid()

ggplot(markov_data_m[transition == "p1"]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  facet_wrap(~transition) +
  background_grid()

ggplot(markov_data_m[transition == "p2"]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  facet_wrap(~transition) +
  background_grid()

ggplot(markov_data_m[transition == "p3"]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  facet_wrap(~transition) +
  background_grid()

ggplot(markov_data_m[transition == "p4"]) +
  geom_point(aes(dominance, probability, color = transition)) +
  geom_smooth(aes(dominance, probability, color = transition, weight = tot),
              method="glm",
              method.args=list(family="binomial")) +
  facet_wrap(~transition) +
  background_grid()

glm_markov_chain <- glm(data = markov_data_m[!is.na(transition)],
                        probability ~ dominance*transition,
                        weights = tot)
summary(glm_markov_chain)



# calculate likelihood of sequence







# if(current_group_configuration[id_number]) {
#   calculate_proportion_dominance_sleep_excluding_focus <- (current_generative_data["sum_dominance_sleep"] - dominance_ranked[id_number]) / sum(dominance_ranked[-id_number]) 
#   
#   return(look_up[id == idsRanked[id_number] & # when is sleeping find the precalculated probabilites of changing state from the model
#                    number_sleeping_excluding_focus == current_generative_data["number_sleeping"] - 1 &
#                    # (proportion_dominance_sleep_excluding_focus > (calculate_proportion_dominance_sleep_excluding_focus - 0.001)) & # do this way for numeric stability
#                    # (proportion_dominance_sleep_excluding_focus < (calculate_proportion_dominance_sleep_excluding_focus + 0.001)
#                     (avg_dominance_sleep > (avg_dominance_sleep - 0.001)) & # do this way for numeric stability
#                       (avg_dominance_sleep < (avg_dominance_sleep + 0.001)),
#                  "p_sa"][[1]] |> round(digits = 5) |> unique())
# }
# else {
#   calculate_proportion_dominance_sleep_excluding_focus <- current_generative_data["sum_dominance_sleep"] / sum(dominance_ranked[-id_number]) 
#   
#   return(look_up[id == idsRanked[id_number] & # when is awake find the precalculated probabilites of changing state from the model
#                    number_sleeping_excluding_focus == current_generative_data["number_sleeping"] &
#                    # (proportion_dominance_sleep_excluding_focus > (calculate_proportion_dominance_sleep_excluding_focus - 0.001)) & # do this way for numeric stability
#                    # (proportion_dominance_sleep_excluding_focus < (calculate_proportion_dominance_sleep_excluding_focus + 0.001)
#                    (avg_dominance_sleep > (avg_dominance_sleep - 0.001)) & # do this way for numeric stability
#                    (avg_dominance_sleep < (avg_dominance_sleep + 0.001)),
#                  "p_as"][[1]] |> round(digits = 5) |> unique())
# }



m_active <- glmmTMB(cbind(p_as, p_aa) ~ 
                      # random effects
                      (1|id) + (1|id_night) +
                      # temporal check
                      dominance * period +
                      # inference covariates
                      dominance * previous_sleep.z + 
                      dominance * avg_dominance_sleep + 
                      dominance * number_sleeping_excluding_focus + 
                      # control covariates
                      precipitation_mm + min_temp + phase, 
                    family = binomial,
                    data = mc_data_covariates_active)
summary(m_active)



new_data_dominance_median <- expand.grid(id = 666, 
                                         id_night = 666, # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                                         previous_sleep.z = 0,
                                         period = c("night_time"),
                                         number_sleeping_excluding_focus = weightedMedian(x = mc_data_covariates$number_sleeping_excluding_focus, 
                                                                                          w = mc_data_covariates$tot_obs),
                                         avg_dominance_sleep = weightedMedian(x = mc_data_covariates$avg_dominance_sleep, 
                                                                              w = mc_data_covariates$tot_obs,
                                                                              na.rm = T), #
                                         precipitation_mm = 0,
                                         min_temp = mean(mc_data_covariates_rest$min_temp),
                                         phase = 0,
                                         dominance = dominance_ranked) |>
  as.data.table()

predictions_dominance_median <- as.data.table(cbind(new_data_dominance_median, # confidence intervals need bootstrapping for random effects
                                                    p_as = predict(object = m_active, 
                                                                   newdata = new_data_dominance_median, 
                                                                   type = "response",
                                                                   allow.new.levels = T),
                                                    p_sa = predict(object = m_rest, 
                                                                   newdata = new_data_dominance_median, 
                                                                   type = "response",
                                                                   allow.new.levels = T),
                                                    partial_p_as = predict(object = m_active_partial, 
                                                                           newdata = new_data_dominance_median, 
                                                                           type = "response",
                                                                           allow.new.levels = T),
                                                    partial_p_sa = predict(object = m_rest_partial, 
                                                                           newdata = new_data_dominance_median, 
                                                                           type = "response",
                                                                           allow.new.levels = T)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                                                          p_active = p_sa / (p_as + p_sa))]

ggplot(predictions_dominance_median) +
  # geom_point(data = mc_data_covariates_active,
  #            aes(residuals(m_active_partial),
  #                residuals(m_active),
  #                size = tot_obs),
  #            alpha = 0.1) +
  geom_line(#data = predictions_dominance_median,
    aes(dominance, p_as),color = "red", linewidth = 1) +
  background_grid()

ggplot(predictions_dominance_median) +
  geom_line(aes(dominance, p_sa)) +
  background_grid()





model_check <- rbind(residuals_predictions_rest[, -c("p_sa", "p_ss")] |>
                       rename(freq_transition = freq_sa,
                              freq_remain = freq_ss,
                              predict_transition = predict_sa), 
                     residuals_predictions_active[, -c("p_as", "p_aa")] |>
                       rename(freq_transition = freq_as,
                              freq_remain = freq_aa,
                              predict_transition = predict_as))



# create model for partial regression plot
# m_active_partial <- glmmTMB(dominance ~ #(1|id) + 
#                               (1|id_night) + 
#                               avg_dominance_sleep + 
#                               number_sleeping_excluding_focus +
#                               precipitation_mm + min_temp + phase,
#                             data = mc_data_covariates_active)
# summary(m_active_partial)
# residuals(m_active_partial)
# 
# 
# m_rest_partial <- glmmTMB(dominance ~ #(1|id) + 
#                             (1|id_night) + 
#                             avg_dominance_sleep + 
#                             number_sleeping_excluding_focus +
#                             precipitation_mm + min_temp + phase,
#                           data = mc_data_covariates_rest)
# summary(m_active_partial)




# night variation 

#  ALL OF THIS IS WRONG BECUASE IF THE TRANSITION DOES NOT HAPPEN I AM EXCLUDING IT, BUT SHOULD BE PRESENT. GO IN MODEL CHECK FOR THE TRUE CORRECT APPROACH WITH WIDE FORMAT!!
# probability transition
# ggplot(mc_data_covariates[transition %in% c("p_sa", "p_as")
#                           ][, .(mean_freq_transition = sum(freq_transition * tot_obs / sum(tot_obs))),
#                             by = list(transition, id, id_night)]) +
#   geom_line(aes(as.factor(id_night), mean_freq_transition, 
#                 color = transition, group = transition), linewidth = 1) +
#   facet_wrap(~id) +
#   background_grid()
# 
# ggplot(mc_data_covariates[transition %in% c("p_sa", "p_as")
# ][, .(mean_freq_transition = sum(freq_transition * tot_obs / sum(tot_obs))),
#   by = list(transition, id, dominance, id_night)]) +
#   geom_line(aes(as.factor(id_night), mean_freq_transition, 
#                 color = dominance, group = id), linewidth = 1) +
#   facet_wrap(~transition) +
#   scale_color_viridis_c(option = "plasma") +
#   background_grid()
# 
# # probability state
# ggplot(mc_data_covariates[transition %in% c("p_sa", "p_as")
# ][, .(mean_freq_transition = sum(freq_transition * tot_obs / sum(tot_obs))),
#   by = list(transition, id, dominance, id_night)] |>
#   pivot_wider(names_from = transition,
#               values_from = mean_freq_transition) |>
#   mutate(prob_sleep = p_as / (p_as + p_sa))) +
#   geom_line(aes(as.factor(id_night), prob_sleep, color = dominance, group = id), linewidth = 1) +
#   scale_color_viridis_c(option = "plasma") +
#   background_grid()
# 
# ggsave(here("figures", "mc_results", "overview", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
#        height = 4,
#        width = 8,
#        bg = "white")
# 
# # compare to real data 
# ggplot(mc_data_covariates[transition %in% c("p_sa", "p_as")
# ][, .(mean_freq_transition = sum(freq_transition * tot_obs / sum(tot_obs))),
#   by = list(transition, id, dominance, id_night)] |>
#   pivot_wider(names_from = transition,
#               values_from = mean_freq_transition) |>
#   mutate(prob_sleep = p_as / (p_as + p_sa))) +
#   geom_line(aes(as.factor(id_night), prob_sleep, color = dominance, group = id), linewidth = 1) +
#   geom_point(data = night_sleep,
#              aes(as.factor(id_night), freq_sleep, color = dominance, group = id), linewidth = 1) +
#   scale_color_viridis_c(option = "plasma",
#                         name = "Dominance") +
#   facet_wrap(~id) +
#   background_grid()
# 
# ggsave(here("figures", "mc_results", "overview", "p_rest_data.png"), #"freq-sleep~dominance_INT_sex.png"
#        height = 7,
#        width = 20,
#        bg = "white")

# ggplot(mc_data_covariates[, .(freq_transition = weighted.mean(freq_transition, tot_obs), # Check if there is a quourm threshold
#                               tot_obs = sum(tot_obs)),
#                           by = list(state, period, id, transition, number_sleeping_excluding_focus)
#                           ][transition %in% c("p_sa", "p_as")]) +
#   geom_line(aes(number_sleeping_excluding_focus, freq_transition, 
#                  color = id)) +
#   geom_point(aes(number_sleeping_excluding_focus, freq_transition, 
#                 color = id, size = tot_obs)) +
#   # geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
#   #               color = avg_dominance_sleep), lty = "dashed",
#   #           linewidth = 1) +
#   facet_grid(cols = vars(period),
#              rows = vars(transition)) +
#   background_grid()


(p4.1 <- ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
    geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
    geom_point(aes(shift_peak_prob, baboon_focus, color = baboon_other)) +
    geom_point(aes(shift_peak_prob, baboon_focus, color = baboon_other)) +
    # geom_line(aes(shift, prob, 
    #               color = baboon_other, group = interaction(is_night, id_other))) +
    scale_color_viridis_d(option = "turbo",
                          name = "") +
    #ylab("Probability of both being active") +
    xlab("Time series shift") +
    background_grid())

ggsave(plot = p4.1,
       filename = here("figures", "mc_results", "tests", "p4.1.png"),
       height = 6,
       width = 4,
       bg = "white")

(p4.1 <- ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
    geom_hline(aes(yintercept = 0), linewidth = 1, lty = "dashed") +
    geom_point(aes(dominance_focus, shift_peak_prob, color = baboon_other,
                   size = peak_ratio)) +
    # geom_line(aes(shift, prob, 
    #               color = baboon_other, group = interaction(is_night, id_other))) +
    scale_color_viridis_d(option = "turbo",
                          name = "") +
    scale_size_continuous(option = "turbo",
                          name = "") +
    guides(color = "none") +
    ylab("Time shift (seconds)") +
    xlab("Dominance focus individual") +
    background_grid())


(p4.2 <- time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T
][, .(peak_ratio = unique(peak_ratio),
      shift_peak_prob = unique(shift_peak_prob)),
  by = list(id_focus, id_other, dominance_focus)
][order(peak_ratio)] |>
    ggplot() + # & shift <= 100 & shift >= -100
    geom_hline(aes(yintercept = 0), linewidth = 1, lty = "dashed") +
    geom_point(aes(
      #size = peak_prob,
      color = peak_ratio,
      dominance_focus, shift_peak_prob, 
    ), size = 2) +
    # geom_point(data = time_correlation_l[is_night == T & syncro == 2
    # ][, .(shift_peak_prob = mean(shift_peak_prob, na.rm = T)), 
    #   by = list(id_focus, dominance_focus)],
    # aes(dominance_focus, shift_peak_prob),
    # color = "red", shape = 4, 
    # size = 2, stroke = 1) +
    geom_smooth(data = time_correlation_l[is_night == T & syncro == 2
    ][, .(shift_peak_prob = unique(shift_peak_prob, na.rm = T),
          weight = max(N)), 
      by = list(id_focus, id_other, dominance_focus)
    ][!(id_focus %in% c("Shebeleza", "Kangela"))],
    aes(dominance_focus, shift_peak_prob, 
        weight = weight),
    color = "red",
    method = "lm", linewidth = 1, se = T) +
    scale_color_viridis_c(option = "viridis",
                          name = "Peak\ninfluence") +
    guides(size = "none") +
    ylab("Time shift (s)") +
    xlab("Dominance focus individual") +
    ggtitle("B)") +
    background_grid())

(p4.1 <- ggplot(time_correlation_l[id_focus == "Kangela" & syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
    geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
    geom_line(aes(shift, ratio, 
                  color = baboon_other, group = id_other)) +
    geom_point(aes(shift_peak_prob, peak_ratio, color = baboon_other), size = 2) +
    scale_color_viridis_d(option = "turbo",
                          name = "Individual") +
    annotate("text", x = 10, y = 1.2, label = "Leads") +
    annotate("text", x = -12, y = 1.2, label = "Follows") +
    guides(color = guide_legend(ncol = 2)) +
    theme(legend.position = c(0.05, 0.61),
          legend.background = element_rect(fill = "white")) +
    #facet_wrap(~baboon_focus) +
    ylab("Influence") +
    xlab("Time shift (s)") +
    ggtitle("A)") +
    background_grid())


generative_data_prep_check_s <- Reduce(function(x, y) merge(x, y, all = T), 
                                       lapply(generative_data_prep_check[, paste0("length_", names(generative_data_prep)[1:12]),
                                                                         with = F],
                                              FUN = function(x) {
                                                rle(x)[[1]] |> table()
                                              }))


generative_data_prep_check_s <- (lapply(generative_data_prep_check[, paste0("length_", names(generative_data_prep)[1:12]),
                                                                   with = F],
                                        FUN = function(x) {
                                          rle(x)[[1]] |> table() |> as.data.table() 
                                        }) |>
                                   rbindlist(idcol = "file") |>
                                   rename(duration = V1,
                                          frequency = N,
                                          id = file))[, ":="(duration = as.numeric(duration),
                                                             frequency = as.numeric(frequency))] 



ggplot(env[[obj]]) +
  geom_histogram(aes(x = mean.VeDBAs, y = cumsum(..count..))) +
  # scale_x_continuous(#breaks = c(0.01, 0.02, 0.03, 0.04, 0.1, 0.3, 1, 1.5, 2, 3, 4),
  #               limits = c(0.1, 4)) +
  geom_vline(aes(xintercept = 0.03), linewidth = 2, color = "red") +
  #scale_x_log10() +
  ggtitle(allForagingFileNames_nice[i])+
  background_grid()




net_null_model <- as_tbl_graph(lk_network[id != id_pair
][, c("id", "id_pair", "ratio", "syncrony")
] |> 
  rename(from = id, to = id_pair),
nodes = dominance_id) |>
  activate(nodes) |>
  mutate(node_number = row_number()) |>
  left_join(dominance_id |> 
              rename(name = id), 
            by = "name")



env <- .GlobalEnv
combined_night <- env[[allForagingFileNames[1]]][Date %in% unique(combined_m[number_individuals == 12][["date"]])][, c("dateTime", "inactive")] |> # I also need the day here
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  id_name <- str_replace(obj, "foraging", "")
  combined_night <- env[[obj]][Date %in% unique(combined_m[number_individuals == 12][["date"]])][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined_night, by = "dateTime", all = TRUE)
}
rm(env)

# Modify and filter
combined_night_m <- copy(combined_night)[, ":="(number_individuals = apply(.SD[, -c("dateTime")], 1, 
                                                                           function(x) {sum(!is.na(x))}),
                                                number_sleeping = apply(.SD[, -c("dateTime")], 1, 
                                                                        sum, na.rm = T))
][, ":="(simpsons_diversity = 1 - number_sleeping * (number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)) + 
           (number_individuals - number_sleeping) * (number_individuals - number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)),
         freq_sleeping = number_sleeping / number_individuals)
][, ":="(date = date(dateTime),
         time = as_hms(dateTime))
][number_individuals = 12]


# Transitions: proper markov chains stuff ----
{
  generative_data_prep <- (combined_m[, c(ids, "id_night", "dateTime"), with = F
                                      # next is to find consecutive moments to calculate transitions
  ][, ":="(diff = c(NA, diff(dateTime)))
  ][, ":="(unit = rleid(diff))
  ][, ":="(is_transition = ifelse(diff == 1, T, F))
  ][is_transition == T # I am loosing max 6 seconds per night to have a valid ONE SECOND RESOLUTION transition (there are some small gaps in dataset)
  ][, -c("is_transition", "diff")
    # create group sleep configuration
  ][, ":="(group_configuration = apply(.SD[, ids, with = F] * 1, 
                                       1, 
                                       paste, collapse = ""))
  ][, ":="(date = date(dateTime))] |>
    left_join(data_astronomical[, c("date", "night_time", "nightEnd_time")]) |>
    as.data.table())[, ":="(period = case_when(as_hms(dateTime) <= nightEnd_time | as_hms(dateTime) >= night_time ~ "night_time",
                                               as_hms(dateTime) > nightEnd_time & as_hms(dateTime) < as_hms("12:00:00") ~ "wakey_time",
                                               as_hms(dateTime) < night_time &  as_hms(dateTime) > as_hms("12:00:00") ~ "bed_time"))
    ][, -c("date", "night_time", "nightEnd_time")]
  
  id <- "Trinity"
  mc_data <- rename(generative_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
  ][, ":="(date = date(dateTime))
  ][, ":="(date_start_night = min(unique(date))),
    by = id_night
  ][, -c("dateTime", "date")], 
  state = id)[, ":="(next_state = c(tail(state, -1), NA)), # could use lead(state)
              by = list(unit, id_night)
  ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                  state == 1 & next_state == 0 ~ "p_sa",
                                  state == 0 & next_state == 0 ~ "p_aa",
                                  state == 0 & next_state == 1 ~ "p_as",
                                  T ~ NA))
  ][!is.na(transition) # exclude 64 observations (should be the same for each id) coming from the non-continuous time series (max 6 breaks per night), not much
  ][, ":="(tot_state = .N),
    by = list(state, id_night)
  ][, ":="(tot_obs = .N),
    by = list(state, id_night, period, group_configuration) # here should be put all the covariates
  ][, .(tot_obs = unique(tot_obs),
        tot_transition = .N,
        #tot_state = unique(tot_state),
        freq_transition = .N / unique(tot_obs),
        date_start_night = unique(date_start_night)),
    by = list(state, id_night, period, group_configuration, transition) # here should be put all the covariates and transition
  ][, ":="(id = id)
  ][, c("id", "id_night", "state", "period", "group_configuration", "transition", 
        "tot_obs", "tot_transition", "freq_transition", "date_start_night")
  ][order(id_night, state, period, group_configuration, transition)]
  
  for(id in ids[-1]) {
    print(id)
    mc_data <- rbind(mc_data,
                     rename(generative_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
                     ][, ":="(date = date(dateTime))
                     ][, ":="(date_start_night = min(unique(date))),
                       by = id_night
                     ][, -c("dateTime", "date")], 
                     state = id)[, ":="(next_state = c(tail(state, -1), NA)), 
                                 by = list(unit, id_night)
                     ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                                     state == 1 & next_state == 0 ~ "p_sa",
                                                     state == 0 & next_state == 0 ~ "p_aa",
                                                     state == 0 & next_state == 1 ~ "p_as",
                                                     T ~ NA))
                     ][!is.na(transition)
                     ][, ":="(tot_state = .N),
                       by = list(state, id_night)
                     ][, ":="(tot_obs = .N),
                       by = list(state, id_night, period, group_configuration) # here should be put all the covariates
                     ][, .(tot_obs = unique(tot_obs),
                           tot_transition = .N,
                           #tot_state = unique(tot_state),
                           freq_transition = .N / unique(tot_obs),
                           date_start_night = unique(date_start_night)),
                       by = list(state, id_night, period, group_configuration, transition) # here should be put all the covariates and transition
                     ][, ":="(id = id)
                     ][, c("id", "id_night", "state", "period", "group_configuration", "transition", 
                           "tot_obs", "tot_transition", "freq_transition", "date_start_night")
                     ][order(id_night, state, period, group_configuration, transition)])
  }
}



to_save <- (combined_m[, c("id_night", "date_start_night", ids), with = F
][, .(seconds_rest = apply(.SD[, ids, with = F], 2, FUN = sum),
      date_start_night = unique(date_start_night)), 
  by = id_night] |>
  cbind(combined_m[, c("id_night", ids), with = F
  ][, .(seconds_night = apply(.SD, 2, FUN = length)), 
    by = id_night][, -c("id_night")]) |>
  cbind(id = ids) 
)[, ":="(         number_active = seconds_night - seconds_rest,
                  probability_rest = seconds_rest / seconds_night,
                  time_rest = seconds_rest / (60*60),
                  id = ifelse(id == "Sheb", "Shebeleza", id),
                  id_night = rleid(date_start_night)),
  by = id
][order(date_start_night, id)
][, c("id_night", "date_start_night", "id", "probability_rest", "seconds_rest", "seconds_night")]

to_save2 <- copy(mc_data_covariates)[, ":="(seconds_night = sum(tot_transition)),
                                     by = list(id, id_night)
][, ":="(probability_rest = sum(tot_transition) / unique(seconds_night),
         seconds_rest = sum(tot_transition)), 
  by = list(id, dominance, id_night, date_start_night, state)
][, .(probability_transition = weighted.mean(freq_transition, tot_transition),
      probability_rest = unique(probability_rest),
      seconds_rest = unique(seconds_rest),
      seconds_night = unique(seconds_night)),
  by = list(id, dominance, id_night, date_start_night, state, transition)
][, ":="(probability_rest_to_active = unique(probability_transition[which(transition == "p_sa")]),
         probability_active_to_rest = unique(probability_transition[which(transition == "p_as")])),
  by = list(id, id_night)
][state == T & transition == "p_sa"
][, -c("state", "transition", "probability_transition")
][order(id, date_start_night)
][, ":="(id_night = rleid(date_start_night)),
  by = id
][order(date_start_night, id)
][, c("id_night", "date_start_night", "id", "dominance", "probability_rest", "seconds_rest", "seconds_night", "probability_rest_to_active", "probability_active_to_rest")]




shift <- 180
id1 <- "Trinity"
id2 <- "Trinity"

test <- cross_correlation_data[, c(id1, id2, "id_nightday", "id_continuous"), 
                               with = F
][, ":="(predictor = get(id1) - mean(get(id1)), # id1 predicts the other (is earlier in time), -mean() makes so that Person correlation is on residuals after accounting for null model
         estimand = lead(get(id2), shift) - mean(get(id2))), # lead takes away the last "shift" values of id2 (id2 will be predicted, is in the future). If more then vector length there are going to be just NA
  by = id_nightday
][, {#corr = cor.test(x = predictor,
  #                      y = estimand,
  #                      alternative = "two.sided",
  #                      method = "pearson");
  list(#rho = corr$statistic,
    #p.value = round(corr$p.value),
    length = .N - shift,
    shift = shift,
    id_before = id1,
    id_after = id2)},
  by = list(id_nightday, id_continuous)]

][, ":="(number_awake = number_individuals - number_sleeping)
][, ":="(simpsons_diversity = 1 - number_sleeping * (number_sleeping - 1) / 
          (number_individuals * (number_individuals - 1)) + 
          (number_individuals - number_sleeping) * (number_individuals - number_sleeping - 1) / 
          (number_individuals * (number_individuals - 1)),
        freq_sleeping = number_sleeping / number_individuals,
        freq_awake = number_awake / number_individuals)
]


ggplot(copy(foragingTrinity)[, ":="(test_diff = c(NA, diff(dateTime)))][test_diff > 1][, ":="(time = as_hms(Timestamp))]) +
  geom_histogram(aes(time)) +
  scale_x_time(limits = c(as_hms("20:00:00"), 
                          as_hms("24:00:00")))

ggsave("figures/test/trinity_bug/trinity_histogram_shifted.png",
       bg = "white",
       height = 10,
       width = 10)

ggplot(foragingTrinity[test_diff > 1][, ":="(time = as_hms(Timestamp))]) +
  geom_histogram(aes(time))


ggplot(foragingTrinity[test_diff > 1][, ":="(time = as_hms(dateTime))]) +
  geom_histogram(aes(time)) +
  scale_x_time(limits = c(as_hms("20:00:00"), 
                          as_hms("24:00:00")))

ggsave("figures/test/trinity_bug/trinity_2_histogram_zoom_shifted.png",
       bg = "white",
       height = 10,
       width = 10)


# ggplot(env[[obj]][test_diff > 1][, ":="(time = as_hms(dateTime))]) +
#   geom_histogram(aes(time)) +
#   scale_x_time(limits = c(as_hms("20:00:00"), 
#                           as_hms("24:00:00")))
# 
# ggsave(paste0("figures/test/trinity_bug/", obj, ".png"),
#        bg = "white",
#        height = 10,
#        width = 10)


test <- copy(combined)[, ":="(diffe = c(NA, diff(dateTime)))][diffe > 1]

ggplot(test[diffe > 1][, ":="(time = as_hms(dateTime))]) +
  geom_histogram(aes(time)) +
  scale_x_time(limits = c(as_hms("20:00:00"), 
                          as_hms("24:00:00")))

ggsave("figures/test/trinity_bug/histogram_zoom.png",
       bg = "white",
       height = 10,
       width = 10)


table(cross_correlation_data[, c("Trinity", "Patch")]) |> phi_max()

test <- cross_correlation_data[, c(id1, id2, "id_nightday", "id_continuous"), 
                               with = F
][, ":="(length = .N - shift),
  by = list(id_continuous)
][length > 3 # p_value of cross correlation can be calculated only for more than 2 pair of observations
][, ":="(predictor = get(id1), # id1 predicts the other (is earlier in time), -mean() makes so that Person correlation is on residuals after accounting for null model
         estimand = lead(get(id2), shift)), # lead takes away the last "shift" values of id2 (id2 will be predicted, is in the future). If more then vector length there are going to be just NA
  by = id_continuous
][!is.na(estimand) # remove excluded times
][, ":="(phi_max = phi_max_rcpp(predictor, estimand)),
  by = id_continuous
][, {corr = cor.test(x = as.numeric(predictor),
                     y = as.numeric(estimand),
                     alternative = "two.sided",
                     method = "pearson");
list(phi = corr$estimate,
     phi_max = unique(phi_max),
     p.value = round(corr$p.value, 6),
     length = .N - shift,
     shift = shift,
     id_before = id1,
     id_after = id2)
},
by = list(id_nightday, id_continuous)
]




m_active_simple_2 <- glmmTMB(cbind(p_as, p_aa) ~ 
                               # random effects
                               (1|id) + (1|id_night) +
                               # temporal check
                               period +
                               # inference covariates
                               dominance * 
                               (previous_sleep.z +
                                  date_median_VeDBA_day +                  
                                  avg_dominance_sleep +
                                  number_sleeping_excluding_focus) +
                               # control covariates
                               id_median_VeDBA +
                               precipitation_mm + min_temp + phase,
                             family = binomial,
                             data = mc_data_covariates_active)
summary(m_active_simple_2)

m_rest_simple_2 <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # temporal check
                             period +
                             # inference covariates
                             dominance * 
                             (previous_sleep.z +
                                date_median_VeDBA_day +                  
                                avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             id_median_VeDBA +
                             precipitation_mm + min_temp + phase,
                           family = binomial,
                           data = mc_data_covariates_rest)
summary(m_rest_simple_2)

# Complex
m_active <- glmmTMB(cbind(p_as, p_aa) ~ 
                      # random effects
                      (1|id) + (1|id_night) +
                      # temporal check
                      period * 
                      # inference covariates
                      dominance * 
                      (previous_sleep.z + avg_dominance_sleep + number_sleeping_excluding_focus) +
                      # control covariates
                      id_median_VeDBA +
                      precipitation_mm + min_temp + phase,
                    family = binomial,
                    data = mc_data_covariates_active)
summary(m_active)

m_rest <- glmmTMB(cbind(p_sa, p_ss) ~ 
                    # random effects
                    (1|id) + (1|id_night) +
                    # temporal check
                    period * 
                    # inference covariates
                    dominance * 
                    (previous_sleep.z + avg_dominance_sleep + number_sleeping_excluding_focus) +
                    # control covariates
                    id_median_VeDBA +
                    precipitation_mm + min_temp + phase, 
                  family = binomial,
                  data = mc_data_covariates_rest)
summary(m_rest)




model_check_predictions <- as.data.table(cbind(model_check, 
                                               predict_transition_active = predict(object = m_active, 
                                                                                   newdata = model_check, 
                                                                                   type = "response"), 
                                               predict_transition_rest = predict(object = m_rest, 
                                                                                 newdata = model_check, 
                                                                                 type = "response"))
)[, ":="(id = factor(id, levels = idsRanked),
         residual = ifelse(state == T, 
                           predict_transition_rest - freq_transition,
                           predict_transition_active - freq_transition),
         predicted_sleep =  predict_transition_active / (predict_transition_active + predict_transition_rest))
] |>
  merge(dominance_id)



(p2.2 <- predictions_period[dominance == 0 & number_sleeping_excluding_focus >= 8.9 & number_sleeping_excluding_focus <= 9.1 & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61
][, ":="(period = factor(period, levels = c("bed_time", "night_time", "wakey_time")))
][order(dominance)] |>
    ggplot() +
    geom_ribbon(aes(x = previous_sleep.z,
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se),
                    fill = dominance, group = dominance),
                color = NA, alpha = 0.2) +
    geom_line(aes(previous_sleep.z, p_rest, 
                  color = dominance, group = dominance),
              linewidth = 1) +
    facet_wrap(~period,
               labeller = labeller(period = c("bed_time" = "Twilight",
                                              "night_time" = "Night",
                                              "wakey_time" = "Dawn")
               )) +
    ylab(expression("Rest probability  "(p[R]))) +
    xlab("Rest in previous night (standardized)") +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "Dominance",
                         option = "plasma") +
    #ggtitle("B)") +
    background_grid())

ggsave(plot = p2.2,
       filename = here("figures", "mc_results", "poster", "compensation.png"),
       height = 3.5,
       width = 5.5,
       bg = "white")

(p2.2 <- predictions_period[number_sleeping_excluding_focus >= 8.9 & number_sleeping_excluding_focus <= 9.1 & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61
][, ":="(period = factor(period, levels = c("bed_time", "night_time", "wakey_time")))
][order(dominance)][period == "night_time"] |>
    ggplot() +
    geom_ribbon(aes(x = previous_sleep.z,
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se),
                    fill = dominance, group = dominance),
                color = NA, alpha = 0.2) +
    geom_line(aes(previous_sleep.z, p_rest, 
                  color = dominance, group = dominance),
              linewidth = 1) +
    facet_wrap(~period,
               labeller = labeller(period = c("bed_time" = "Twilight",
                                              "night_time" = "Night",
                                              "wakey_time" = "Dawn")
               )) +
    ylab(expression("Rest probability  "(p[R]))) +
    xlab("Rest in previous night (standardized)") +
    # ggtitle("B)") +
    # background_grid() +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "Dominance",
                         option = "plasma"))

ggsave(plot = p2.2,
       filename = here("figures", "mc_results", "poster", "compensation.png"),
       height = 3.5,
       width = 4.5,
       bg = "white") 

(fig_2 <- ggarrange(p2.1, p2.2,
                    ncol = 2, nrow = 1,
                    widths = c(1.3, 2)))

ggsave(plot = fig_2,
       filename = here("figures", "mc_results", "manuscript_1", "fig_2.png"),
       height = 4.5,
       width = 12,
       bg = "white")  


### Model checks ----
copy(model_check_predictions)[, ":="(cutted = cut(previous_sleep.z, 
                                                  breaks = 7, 
                                                  labels = F))
][, ":="(previous_sleep.z = mean(previous_sleep.z)),
  by = cutted
][, ":="(total = sum(tot_obs, na.rm = T)),
  by = list(state, period, dominance, previous_sleep.z)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T),
      tot_obs = sum(tot_obs)), 
  by = list(period, dominance, previous_sleep.z)
][,":="(residual = predicted_sleep - real_sleep)][, ":="(period = factor(period, levels = c("bed_time", "night_time", "wakey_time")))] |>
  ggplot() +
  geom_point(aes(previous_sleep.z, residual,
                 color = dominance, size = tot_obs)) +
  geom_line(aes(previous_sleep.z, residual, 
                color = dominance, group = dominance)) +
  # geom_segment(aes(y = rep(0, 181), yend = residual, 
  #                  x = previous_sleep.z, xend = previous_sleep.z,
  #                  color = dominance), linewidth = 2, linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linewidth = 2, 
             linetype = "dashed", color = "darkgrey") +
  facet_wrap(~period,
             labeller = labeller(period = c("bed_time" = "Twilight",
                                            "night_time" = "Night",
                                            "wakey_time" = "Dawn")
             )) +
  ylab(expression("Residuals for rest probability  "(p[R]))) +
  xlab("Rest in previous night (standardized)") +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  scale_size_continuous(name = "Sample size") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "fig2_residuals2.png"),
       height = 3.5,
       width = 12,
       bg = "white")


(p4.1 <- ggplot(time_correlation_synchro[id_focus == "Gabrielle" & id_focus != id_other & shift <= 50 & shift >= -50]) + # & shift <= 100 & shift >= -100
    geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
    geom_line(aes(shift, ratio, 
                  color = id_other, 
                  group = id_other), 
              linewidth = 1) +
    geom_point(aes(peak_shift, peak_ratio,
                   color = id_other),
               size = 5) +
    scale_color_viridis_d(option = "turbo",
                          name = "Individual") +
    # annotate("text", x = 10, y = 1.25, label = "Leads") +
    # annotate("text", x = -12, y = 1.25, label = "Follows") +
    # annotate("text", x = 10, y = 1.275, label = "Leads") +
    # annotate("text", x = -12, y = 1.275, label = "Follows") +
    # theme(legend.position = c(0.75, 0.61),
    #       legend.background = element_rect(fill = "white")) +
    ylab("Influence") +
    xlab("Time shift (s)") +
    #ggtitle("A)") +
    #background_grid() +
    guides(#color = guide_legend(ncol = 2),
      color = "none"))

ggsave(plot = p4.1,
       filename = here("figures", "mc_results", "poster", "influence_example.png"),
       height = 3.5,
       width = 6,
       bg = "white")





cc_net <- cross_correlation_stats[id_focus != id_other
][peak_time > 0
][, ":="(out_degree = .N),
  by = id_focus]

nodes_position <- data.table(id = c(cc_net[order(out_degree)]$id_focus |>
                                      unique() |>
                                      rev() |>
                                      as.character(), 
                                    "Azul") |> # add Azul becuase azul outdegree is zero
                               factor(levels = idsRanked),
                             x = c(0.5, 0.6, 0.35, 
                                   0.2, 0.45, 0.6, 
                                   0.25, 0.5, 0.65,
                                   0.3, 0.75, 0.1),
                             y = seq(5, 0, l = 12)) |>
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")])

cc_net_complete <- (cc_net |>
                      merge(nodes_position[, c("id", "x", "y")] |>
                              rename(id_focus = id,
                                     x_focus = x,
                                     y_focus = y),
                            by = "id_focus") |>
                      merge(nodes_position[, c("id", "x", "y")] |>
                              rename(id_other = id,
                                     x_other = x,
                                     y_other = y),
                            by = "id_other"))[order(peak_rho)]

ggplot(cc_net_complete[sex_focus == "female" & sex_other == "female"]) +
  geom_segment(aes(x = x_focus, y = y_focus,
                   xend = x_other, yend = y_other,
                   color = peak_rho),
               linewidth = 1,
               arrow = arrow(angle = 10, 
                             ends = "last", 
                             type = "closed")) +
  geom_point(data = nodes_position[sex == "female"],
             aes(x, y, fill = dominance),
             size = 5, shape = 21, stroke = 1) +
  geom_label(data = nodes_position[sex == "female"],
             aes(x, y, label = id),
             nudge_y = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance") +
  scale_color_viridis_c(name = "Peak\ninfluence") +
  scale_y_continuous(breaks = NULL,
                     name = NULL) +
  scale_x_continuous(breaks = NULL,
                     name = NULL) +
  theme_void()

mechanism_united <- (time_correlation_s_all[id_focus != id_other & 
                                              id_focus %in% ids_distances_ranked_correct_names & 
                                              id_other %in% ids_distances_ranked_correct_names
][, ":="(id_focus = factor(id_focus, levels = ids_distances_ranked_correct_names),
         id_other = factor(id_other, levels = ids_distances_ranked_correct_names))
][, .(peak_syncro = unique(peak_syncro),
      peak_ratio = unique(peak_ratio),
      peak_normalized_prob = unique(peak_normalized_prob),
      peak_shift = unique(peak_shift)),
  by = c("id_other", "id_focus")] |>
  merge(copy(data_distances_s)[, ":="(id = case_when(id == "Sheb" ~ "Shebeleza",
                                                     T ~ id),
                                      neighb = case_when(id == "Sheb" ~ "Shebeleza",
                                                         T ~ neighb))
  ][id %in% ids_distances_ranked_correct_names & neighb %in% ids_distances_ranked_correct_names
  ][, ":="(id = factor(id, levels = ids_distances_ranked_correct_names),
           neighb = factor(neighb, levels = ids_distances_ranked_correct_names))] |> 
    rename(id_focus = id,
           id_other = neighb,
           weight_dist = weight),
  by = c("id_focus", "id_other")) |>
  merge(dominance_id[, c("id", "dominance")] |>
          rename(id_focus = id,
                 dominance_focus = dominance),
        by = "id_focus") |>
  merge(dominance_id[, c("id", "dominance")] |>
          rename(id_other = id,
                 dominance_other = dominance),
        by = "id_other"))[, ":="(dom_diff = abs(dominance_focus - dominance_other))][order(dom_diff)] |>
  merge(net_null_model |> 
          activate(edges) |>
          filter(syncrony == "syncro") |>
          as.data.frame() |>
          left_join(net_null_model |> 
                      activate(nodes) |>
                      as.data.table() |>
                      select(name, node_number) |>
                      rename(from = node_number,
                             id_focus = name)) |>
          left_join(net_null_model |> 
                      activate(nodes) |>
                      as.data.table() |>
                      select(name, node_number) |>
                      rename(to = node_number,
                             id_other = name)) |>
          select(-c(from, to, syncrony)) |>
          rename(syncro_ratio = ratio),
        by = c("id_focus", "id_other"))


|>
  merge(net_null_model |> 
          activate(edges) |>
          filter(syncrony == "syncro") |>
          as.data.frame() |>
          left_join(net_null_model |> 
                      activate(nodes) |>
                      as.data.table() |>
                      select(name, node_number) |>
                      rename(from = node_number,
                             id_focus = name)) |>
          left_join(net_null_model |> 
                      activate(nodes) |>
                      as.data.table() |>
                      select(name, node_number) |>
                      rename(to = node_number,
                             id_other = name)) |>
          select(-c(from, to, syncrony)) |>
          rename(syncro_ratio = ratio),
        by = c("id_focus", "id_other"))



m_rest_simple <- glmmTMB(cbind(p_sa, p_ss) ~ 
                           # random effects
                           (1|id) + (1|id_night) +
                           # temporal check
                           # period +
                           # inference covariates
                           dominance * 
                           (previous_sleep.z +
                              avg_dominance_sleep +
                              number_sleeping_excluding_focus) +
                           # control covariates
                           id_median_VeDBA +
                           precipitation_mm + min_temp + phase,
                         family = binomial,
                         data = mc_data_covariates_rest)

m_rest_simple_1 <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # inference covariates
                             dominance * 
                             (avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             previous_sleep.z +
                             date_total_VeDBA_day +
                             precipitation_mm + 
                             min_temp + 
                             phase,
                           family = binomial,
                           data = mc_data_covariates_active)

m_rest_simple_2 <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # inference covariates
                             dominance * 
                             (avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             previous_sleep.z +
                             date_median_VeDBA_day +
                             precipitation_mm + 
                             min_temp + 
                             phase,
                           family = binomial,
                           data = mc_data_covariates_active)

m_rest_simple_3 <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # inference covariates
                             dominance * 
                             (avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             previous_sleep.z +
                             date_mean_VeDBA_day +
                             precipitation_mm + 
                             min_temp + 
                             phase,
                           family = binomial,
                           data = mc_data_covariates_active)

m_rest_simple_4 <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # inference covariates
                             dominance * 
                             (avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             previous_sleep.z +
                             id_median_VeDBA +
                             precipitation_mm + 
                             min_temp + 
                             phase,
                           family = binomial,
                           data = mc_data_covariates_active)

m_active_simple <- glmmTMB(cbind(p_as, p_aa) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # temporal check
                             #period +
                             # inference covariates
                             dominance * 
                             (previous_sleep.z +
                                avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             id_median_VeDBA +
                             precipitation_mm + 
                             min_temp + 
                             phase,
                           family = binomial,
                           data = mc_data_covariates_active)

m_active_simple_1 <- glmmTMB(cbind(p_as, p_aa) ~ 
                               # random effects
                               (1|id) + (1|id_night) +
                               # inference covariates
                               dominance * 
                               (avg_dominance_sleep +
                                  number_sleeping_excluding_focus) +
                               # control covariates
                               previous_sleep.z +
                               date_total_VeDBA_day +
                               precipitation_mm + 
                               min_temp + 
                               phase,
                             family = binomial,
                             data = mc_data_covariates_active)

m_active_simple_2 <- glmmTMB(cbind(p_as, p_aa) ~ 
                               # random effects
                               (1|id) + (1|id_night) +
                               # inference covariates
                               dominance * 
                               (avg_dominance_sleep +
                                  number_sleeping_excluding_focus) +
                               # control covariates
                               previous_sleep.z +
                               date_median_VeDBA_day +
                               precipitation_mm + 
                               min_temp + 
                               phase,
                             family = binomial,
                             data = mc_data_covariates_active)

m_active_simple_3 <- glmmTMB(cbind(p_as, p_aa) ~ 
                               # random effects
                               (1|id) + (1|id_night) +
                               # inference covariates
                               dominance * 
                               (avg_dominance_sleep +
                                  number_sleeping_excluding_focus) +
                               # control covariates
                               previous_sleep.z +
                               date_mean_VeDBA_day +
                               precipitation_mm + 
                               min_temp + 
                               phase,
                             family = binomial,
                             data = mc_data_covariates_active)

m_active_simple_4 <- glmmTMB(cbind(p_as, p_aa) ~ 
                               # random effects
                               (1|id) + (1|id_night) +
                               # inference covariates
                               dominance * 
                               (avg_dominance_sleep +
                                  number_sleeping_excluding_focus) +
                               # control covariates
                               previous_sleep.z +
                               id_median_VeDBA +
                               precipitation_mm + 
                               min_temp + 
                               phase,
                             family = binomial,
                             data = mc_data_covariates_active)


cross_correlation_stats_matrix_random[upper.tri(cross_correlation_stats_matrix_random)] <- c(interactions_strengths *
                                                                                               sample(c(rep(-1, sum(cross_correlation_stats$peak_time != 0)/2/2),
                                                                                                        rep(1, sum(cross_correlation_stats$peak_time != 0)/2/2)),
                                                                                                      replace = F), 
                                                                                             rep(0, l = sum(cross_correlation_stats$peak_time == 0)/2)) |> sample()
for(i in 1:1000) {
  print(i)
  cross_correlation_stats_matrix_random <- !diag(NA,
                                                 nrow = 12,
                                                 ncol = 12)
  cross_correlation_stats_matrix_random[upper.tri(cross_correlation_stats_matrix_random)] <- sample(m_cross_corr[upper.tri(m_cross_corr)])
  cross_correlation_stats_matrix_random[lower.tri(cross_correlation_stats_matrix_random)] <- -t(cross_correlation_stats_matrix_random)[upper.tri(cross_correlation_stats_matrix_random)]
  
  randomized_data <- cross_correlation_stats[, c("id_focus", "id_other", "peak_time", "sex_focus", "dominance_focus")
  ][order(id_focus, id_other)
  ][, ":="(peak_time = na.omit(as.vector(cross_correlation_stats_matrix_random)))
  ][peak_time != 0
  ][, ":="(proportion_influence = sum(peak_time > 0)/.N),
    by = id_focus]
  
  random_slope <- glm(data = randomized_data[, .(proportion_influence = unique(proportion_influence),
                                                 size = .N),
                                             by = list(dominance_focus, id_focus, sex_focus)],
                      proportion_influence ~ dominance_focus + sex_focus, weights = size)
  
  randomized_coeff[i] <- coefficients(random_slope)[2]
}


test <- randomized_data_all[peak_time != 0
][, ":="(proportion_influence = sum(peak_time > 0)/.N),
  by = list(replicate, id_focus)][, .(proportion_influence = unique(proportion_influence),
                                      size = .N),
                                  by = list(replicate, dominance_focus, id_focus, sex_focus)]

ggplot(test) +
  geom_point(aes(dominance_focus, proportion_influence), alpha = 0.1) +
  background_grid()


ggsave(plot = p1,
       filename = here("figures", "mc_results", "tests", "p1.png"),
       height = 7,
       width = 12,
       bg = "white")

(p4 <- ggplot(predictions_rain[avg_dominance_sleep == 0.5 & number_sleeping_excluding_focus == 9 & dominance == 0.5]) +
    geom_ribbon(aes(x =  precipitation_mm, 
                    ymin = 1 - (p_as - p_as_se), 
                    ymax = 1 - (p_as + p_as_se)),
                color = NA, alpha = 0.2) +
    geom_line(aes(precipitation_mm, 1 - p_as)) +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "DOminance",
                         option = "plasma") +
    xlab("Precipitation (mm)") +
    background_grid() +
    guides(fill = "none") +
    ggtitle("D)") +
    ylab(expression("Continue activity  "(p[A%->%A]))))


(p4 <- ggplot(night_sleep[!(id_night %in% c(1, 25))
][, ":="(is_rain = ifelse(precipitation_mm == 0, F, T))]) +
    geom_boxplot(aes(is_rain, time_sleep)) +
    stat_summary(aes(is_rain, time_sleep, group = 1),
                 fun=mean, geom="line", color = "red", size = 1)  + 
    stat_summary(aes(is_rain, time_sleep, group = 1),
                 fun=mean, geom="point", color = "red", size = 3)  + 
    guides(linetype = "none") +
    ylab("Nighttime rest (hours)") +
    scale_x_discrete(name = NULL,
                     labels = c("No rain", "Rain")) +   
    ggtitle("D)") +
    background_grid())



## For random forest ----
# for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
#   print(file)
#   assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
#          fill_empty_time(as.data.table(readRDS(here::here("data", # fill each second 
#                                                           "Baboon behaviour from acceleration (1)", 
#                                                           file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
#   
#   
# }


## For epoch ----
# for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
#   print(file)
#   assign(paste0("foraging", sub("trim2021.rds.*", "", file)),
#          fill_empty_time_mean_VeDBA(as.data.table(readRDS(here::here("data", # fill each second
#                                                                       "Baboon behaviour from acceleration (1)",
#                                                                       file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
# 
# 
# }
# # plot
# env <- .GlobalEnv
# allForagingFileNames <- grep("foraging", ls(), value = TRUE)
# for(obj in allForagingFileNames) {
#   print(obj)
#   p1 <- ggplot(env[[obj]]) +
#     geom_histogram(aes(mean.VeDBA)) +
#     scale_x_log10(breaks = c(0.01, 0.02, 0.03, 0.04, 0.1, 0.3, 1, 1.5, 2, 3, 4)) +
#     geom_vline(aes(xintercept = 0.03), linewidth = 2, color = "red") +
#     ggtitle(obj)+
#     background_grid()
# 
#   ggsave(plot = p1,
#          filename = here("figures", "random forest", "distribution", paste0(obj, ".png")),
#          height = 3,
#          width = 10,
#          bg = "white")
# }


## For random forest ----
# for(obj in allForagingFileNames) {
#   print(obj)
#   env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
#                              rename(dateTime = Timestamp) |>
#                              merge(data_astronomical[, c("sunrise", "sunset", "date")],
#                                    all.x = TRUE,
#                                    by.x = "Date", 
#                                    by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
#                                    ][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))]
# }



## For epoch ----
# for(obj in allForagingFileNames) { # takes a shit ton of time (more than half a day), so I first do this than save than do the rest
#   print(obj)
#   env[[obj]] <- copy(env[[obj]])[, ":="(time = as_hms(Timestamp),
#                                   median_9min_VeBDA = rollapply(mean.VeDBA, 
#                                                                 FUN = median, na.rm = T,
#                                                                 width = 60*9 + 1, 
#                                                                 fill = NA))] |> # by reference
#     rename(dateTime = Timestamp) |>
#     merge(data_astronomical[, c("sunrise", "sunset", 
#                                 #"night", "nightEnd","sunrise_time", "sunset_time", "night_time", "nightEnd_time", 
#                                 "date")],
#           all.x = TRUE,
#           by.x = "Date", 
#           by.y = "date")
# }
#save.image(here("data", "data_epoch.RData"))
# 
# for(obj in allForagingFileNames) {
#   print(obj)
#   env[[obj]] <- copy(env[[obj]])[, ":="(candidate_block = if_else(median_9min_VeBDA < quantile(median_9min_VeBDA, probs = 0.1, na.rm = T) * 1.125, 
#                                                       T, 
#                                                       F))][, ":="(length_candidate_block = rep(rle(candidate_block)[[1]], 
#                                                                                                rle(candidate_block)[[1]]))
#                                                            ][, ":="(block = ifelse(length_candidate_block > 60 * 30 & candidate_block == T,
#                                                                                    T, 
#                                                                                    F))
#                                                              ][, ":="(length_candidate_united = rep(rle(block)[[1]], 
#                                                                                                     rle(block)[[1]]))
#                                                                  ][, ":="(period = ifelse(block == F & length_candidate_united < 60 * 45 | # candidate_united == F is always separated by two sleep blocks, so just the ones longer than 45 minutes will suffice
#                                                                                             block == T,
#                                                                                                     T,
#                                                                                                     F))
#                                                                    ][, ":="(inactive = ifelse(candidate_block == T & length_candidate_block > 60 * 3,
#                                                                                                  T,
#                                                                                                  F))
#                                                                      ][, ":="(sunrise_time = as_hms(sunrise),
#                                                                               sunset_time = as_hms(sunset))
#                                                                        ][, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))]
#                                                                         
# }
# 
# # Plot
# for(obj in allForagingFileNames) {
#   print(obj)
#   
# p1 <- ggplot(env[[obj]]) +
#   geom_tile(aes(time, Date, fill = is_sleeping)) +
#   geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
#   geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
#   ggtitle(obj) +
#   scale_fill_viridis_d(direction = -1) 
# 
# ggsave(plot = p1,
#        filename = here("figures", "epoch", paste0(obj, "_epoch.png")),
#        height = 30,
#        width = 25,
#        bg = "white")  
# 
# p1 <- ggplot(env[[obj]]) +
#   geom_tile(aes(time, Date, fill = period)) +
#   geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
#   geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
#   ggtitle(obj) +
#   scale_fill_viridis_d(direction = -1) 
# 
# ggsave(plot = p1,
#        filename = here("figures", "epoch", paste0(obj, "_period.png")),
#        height = 30,
#        width = 25,
#        bg = "white") 
# 
# p1 <- ggplot(env[[obj]]) +
#   geom_tile(aes(time, Date, fill = block)) +
#   geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
#   geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
#   ggtitle(obj) +
#   scale_fill_viridis_d(direction = -1) 
# 
# ggsave(plot = p1,
#        filename = here("figures", "epoch", paste0(obj, "_block.png")),
#        height = 30,
#        width = 25,
#        bg = "white")
# }


## For epoch ----
# env <- .GlobalEnv
# combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "inactive")] |>
#   setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
# for(obj in allForagingFileNames[-1]) {
#   print(obj)
#   id_name <- str_replace(obj, "foraging", "")
#   combined <- env[[obj]][is_night == T][, c("dateTime", "inactive")] |>
#     setnames(old = "inactive", new = id_name) |>
#     merge(combined, by = "dateTime", all = TRUE)
# }
# rm(env)


data_GPS_m <- data_GPS[date_time_South_Africa >= night_10_min] |>
  filter(!is.na(lon) & !is.na(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), dim = "XY") |>
  sf::st_set_crs(22234) |>
  sf::st_transform(crs = 4326) %>%
  as.data.frame() %>%
  extract(geometry,
          c('Longitude', 'Latitude'),
          '\\((.*), (.*)\\)',
          convert = TRUE)

ggplot(data_GPS_m) +
  geom_point(aes(Longitude, Latitude, color = date)) +
  #ggspatial::annotation_scale() +
  ggmap::get_map(location = c(lon = 24.390, 
                              lat = -34.19),
                 source = "osm",
                 force = "T")


# quourm_sleep <- data.frame()
# for(id in ids) {
#   print(id)
#   quourm_sleep <- rbind(quourm_sleep, 
#                         combined_m[number_individuals == 12
#                         ][, .(count = .N),
#                           by = list(number_sleeping, get(id))
#                         ][, ":="(id = id)] |>
#                           setnames("get", "is_sleeping"))
# }
# 
# quourm_sleep_m <- copy(quourm_sleep)[, ':='(id = case_when(id == "Sheb" ~"Shebeleza",
#                                                            T ~ id))
# ][, ':='(is_sleeping = case_when(is_sleeping == T ~ "Sleeping",
#                                  is_sleeping == F ~ "Awake"))
# ][is_sleeping == "Sleeping",
#   # ":="(number_sleeping = number_sleeping - 1), # exclude focus individual
#   ":="(number_sleeping = number_sleeping ), # dont exclude focus individual
#   # likelihood calculations
# ][, ":="(tot_id = sum(count)), 
#   by = list(is_sleeping, id)
# ][, ":="(freq_sleep_by_id = count / tot_id), 
#   by = list(is_sleeping, id, number_sleeping)
#   # posterior calculations
# ][, ":="(tot_sleeping = sum(count)),
#   by = list(is_sleeping, number_sleeping)
# ][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
#   by = is_sleeping
# ][, ":="(freq_id_by_sleep = count / tot_sleeping), 
#   by = list(is_sleeping, number_sleeping, id)
# ][, ":="(check = sum(freq_sleep_by_id)), 
#   by = list(is_sleeping, id)
# ][, ":="(check2 = sum(freq_id_by_sleep)), 
#   by = list(is_sleeping, number_sleeping)] |>
#   merge(dominance_id, on = "id") |>
#   mutate(id = factor(id, levels = idsRanked))


# Data
model_rho_dom_diff_data <- data_model_mechanism_3[, c("peak_rho", "dom_diff", "sex_pair", "id_focus", "id_other", "id_pair")
][, .SD[1], by = id_pair]
model_rho_dom_diff_data_stan <- list(N = nrow(model_rho_dom_diff_data),
                                     N_ind = length(unique(c(model_rho_dom_diff_data$id_focus,
                                                             model_rho_dom_diff_data$id_other))),
                                     N_sex_pairs = 3,
                                     rho = as.vector(scale(model_rho_dom_diff_data$peak_rho)),
                                     dom_diff = as.vector(scale(model_rho_dom_diff_data$dom_diff)),
                                     sex_pair = as.numeric(as.factor(model_rho_dom_diff_data$sex_pair)),
                                     first_ind = as.character(model_rho_dom_diff_data$id_focus) |>
                                       factor(levels = unique(c(model_rho_dom_diff_data$id_focus,
                                                                model_rho_dom_diff_data$id_other))) |>
                                       as.numeric(),
                                     second_ind = as.character(model_rho_dom_diff_data$id_other) |>
                                       factor(levels = unique(c(model_rho_dom_diff_data$id_focus,
                                                                model_rho_dom_diff_data$id_other))) |>
                                       as.numeric())

# Compile
prior_rho_dom_diff <- rstan::stan_model("scripts/stan/rho_dom.stan")

# Prior predictive simulations 
prior_draws <- rstan::sampling(prior_rho_dom_diff,
                               data = model_rho_dom_diff_data_stan,
                               chains = 4,
                               iter = 1000,
                               algorithm = "Fixed_param") |> 
  as.data.table()

ggplot(prior_draws[, c("alpha", "beta")]) +
  geom_abline(aes(intercept = alpha,
                  slope = beta), 
              alpha = 0.1) +
  xlim(c(0, 1)) +
  ylim(c(-5, 5))

# Compile
model_rho_dom_diff <- rstan::stan_model("scripts/stan/rho_dom_model.stan")

# Prior predictive simulations 
posterior <- rstan::sampling(model_rho_dom_diff,
                             data = model_rho_dom_diff_data_stan,
                             chains = 4,
                             iter = 1000) |> 
  as.data.table()

# Diagnostics
bayesplot::mcmc_trace(posterior_draws, pars = "beta")
bayesplot::rhat(posterior_draws)

as.data.table(posterior) |>
  ggplot() +
  geom_histogram(aes(beta))

ggsave("figures/test/tests.png",
       bg = "white",
       width = 5,
       height = 5)

as.data.table(posterior) |>
  ggplot() +
  geom_histogram(aes(alpha))

ggplot(model_rho_dom_diff_data) +
  geom_abline(aes(intercept = mean(posterior$alpha),
                  slope = mean(posterior$beta))) +
  geom_point(aes(scale(dom_diff), scale(peak_rho)))


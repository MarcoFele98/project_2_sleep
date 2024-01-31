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

# perform bootstrap ----
quourm_bootstrap <- data.table()
for(i in 1:500) { # have to rerun this bc I added an important line --------------------------------------
  print(i)
  quourm_bootstrap <- rbind(quourm_bootstrap, 
                            behaviours_inActive_m[number_individuals == 12
  ][order(Timestamp, id)
  ][, ":="(repeated_start = rleid(Timestamp)) # when multiple individuals have the same start or end of behaviour the real number of inactive individuals is going to be the last in order
  ][, ":="(number_inactive = rep(tail(number_inactive, n = 1), l = .N)), 
    by = repeated_start
  ][point == "end", ":="(number_inactive = number_inactive + 1) # before it counted the number inactive remaining after stop inactive
  ][point == "start", ":="(number_inactive = number_inactive - 1) # before it counted the number inactive remaining after start beeing inactive ----------------------------------------------------------------
  ][, .SD[sample(.N, .N*6/10, replace = F)], # jack-knife is better!! bootstrap would create new observations which is strange[, ":="(tot = .N),
    by = point
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
          by = "id") |>
    mutate(id = factor(id, levels = idsRanked),
           replicate = i))
}
#write.csv(quourm_bootstrap, "data/quourm_jacknife.csv")

quourm_bootstrap_s <- quourm_bootstrap[, .(posterior = mean(posterior),
                                           posterior_low = quantile(posterior, prob = 0.025),
                                           posterior_high = quantile(posterior, prob = 0.975),
                                           likelihood = mean(likelihood),
                                           likelihood_low = quantile(likelihood, prob = 0.025),
                                           likelihood_high = quantile(likelihood, prob = 0.975),
                                           p_data = mean(p_data),
                                           p_data_low = quantile(p_data, prob = 0.025),
                                           p_data_high = quantile(p_data, prob = 0.975),
                                           prior = unique(prior),
                                           sex = unique(sex),
                                           dominance = unique(dominance)),
                                       by = list(point, id, number_inactive)]

# posterior
ggplot(quourm_bootstrap_s[point == "start"]) +
  geom_line(aes(number_inactive, posterior, color = id), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = posterior_high, ymin = posterior_low, fill = id), alpha = 0.3) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s[point == "end"]) +
  geom_line(aes(number_inactive, posterior, color = id), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = posterior_high, ymin = posterior_low, fill = id), alpha = 0.3) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s[point == "start"]) +
  geom_ribbon(aes(number_inactive, ymax = posterior_high, ymin = posterior_low, fill = dominance, group = id), alpha = 0.3) + 
  geom_line(aes(number_inactive, posterior, color = dominance, group = id, lty = sex), linewidth = 1) + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggsave("figures/nice/posterior2.png",
       width = 5,
       height = 4,
       bg = "white")

ggplot(quourm_bootstrap_s[point == "end"]) +
  geom_line(aes(number_inactive, posterior, color = dominance, group = id, lty = sex), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = posterior_high, ymin = posterior_low, fill = dominance, group = id), alpha = 0.3) + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s) +
  geom_line(aes(number_inactive, posterior, lty = sex, color = point), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = posterior_high, ymin = posterior_low, fill = point), alpha = 0.3) +
  facet_wrap(~ id, ncol = 6) +
  background_grid()

ggsave("figures/nice/quourm_comparison2.png",
       width = 10,
       height = 4,
       bg = "white")

# likelihood
ggplot(quourm_bootstrap_s[point == "start"]) +
  geom_line(aes(number_inactive, likelihood, color = id), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = likelihood_high, ymin = likelihood_low, fill = id), alpha = 0.3) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s[point == "end"]) +
  geom_line(aes(number_inactive, likelihood, color = id), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = likelihood_high, ymin = likelihood_low, fill = id), alpha = 0.3) + 
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s[point == "start"]) +
  geom_ribbon(aes(number_inactive, ymax = likelihood_high, ymin = likelihood_low, fill = dominance, group = id), alpha = 0.3) + 
  geom_ribbon(aes(number_inactive, ymax = p_data_high, ymin = p_data_low), fill = "darkred", alpha = 0.3) + 
  geom_line(aes(number_inactive, likelihood, color = dominance, group = id, lty = sex)) + 
  geom_line(aes(number_inactive, p_data), color = "darkred", linewidth = 1, lty = "dashed") + 
 # geom_line(aes(number_inactive, prior), linewidth = 1, lty = "dashed", color = "dark blue") + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggsave("figures/nice/likelihood2.png",
       width = 5,
       height = 4,
       bg = "white")

ggplot(quourm_bootstrap_s[point == "end"]) +
  geom_line(aes(number_inactive, likelihood, color = dominance, group = id, lty = sex), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = likelihood_high, ymin = likelihood_low, fill = dominance, group = id), alpha = 0.3) + 
  scale_color_viridis_c(option = "viridis") +
  scale_fill_viridis_c(option = "viridis") +
  facet_wrap(~point) +
  background_grid()

ggplot(quourm_bootstrap_s) +
  geom_line(aes(number_inactive, likelihood, lty = sex, color = point), linewidth = 1) + 
  geom_ribbon(aes(number_inactive, ymax = likelihood_high, ymin = likelihood_low, fill = point), alpha = 0.3) +
  facet_wrap(~ id, ncol = 6) +
  background_grid()

ggsave("figures/nice/likelihood_comparison2.png",
       width = 10,
       height = 4,
       bg = "white")



# predictor is average dominance ----

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
  by = list(point, number_inactive_excluding_dominance, avg_dominance_inactive)
][, .(tot_id = .N,
      tot = unique(tot)),
  by = list(point, number_inactive_excluding_dominance, avg_dominance_inactive, id)
][, ":="(prop = tot_id/tot)
][, ":="(avg_dominance_inactive.z = scale(avg_dominance_inactive)[,1]),
  by = list(number_inactive_excluding_dominance)
  ] |> 
  merge(dominance_id,
        by = "id") |>
  mutate(id = factor(id, levels = idsRanked))

quourm_dominance <- behaviours_inActive_m[number_individuals == 12  & !is.na(adding_dominance)
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
  by = list(point, avg_dominance_inactive)
][, .(tot_id = .N,
      tot = unique(tot)),
  by = list(point, avg_dominance_inactive, id)
][, ":="(prop = tot_id/tot)
] |> 
  merge(dominance_id,
        by = "id") |>
  mutate(id = factor(id, levels = idsRanked))

# posterior
ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, prop, 
                 #color = dominance, lty = sex, 
                 alpha = tot)) + 
  geom_smooth(aes(avg_dominance_inactive, prop, 
                  #color = dominance, lty = sex,
                  weight = tot), 
              linewidth = 1,color = "red",
              method = "glm",
              method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "viridis") +
  facet_grid(rows = vars(id), cols = vars(number_inactive_excluding_dominance)) +
  ylim(c(0,1)) +
  background_grid()

ggsave("figures/nice/prop~inactive.png",
       width = 15,
       height = 15,
       bg = "white")

ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive.z, prop, 
                 #color = dominance, lty = sex, 
                 alpha = tot)) + 
  geom_smooth(aes(avg_dominance_inactive.z, prop, 
                  #color = dominance, lty = sex,
                  weight = tot), 
              linewidth = 1,color = "red",
              method = "glm",
              method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "viridis") +
  facet_grid(rows = vars(id), cols = vars(number_inactive_excluding_dominance)) +
  ylim(c(0,1)) +
  background_grid()

ggsave("figures/nice/prop~inactive.z.png",
       width = 15,
       height = 15,
       bg = "white")

# this one !!!!!!!!!!!!!!!!!!!!!!!
ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, prop, alpha = tot, color = number_inactive_excluding_dominance)) + 
  geom_smooth(aes(avg_dominance_inactive, prop, weight = tot, #lty = sex,
                  group = number_inactive_excluding_dominance, color = number_inactive_excluding_dominance), 
              linewidth = 1,
              method = "glm",
              method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "magma",
                        name = "Inactive") +
  facet_wrap(~id) +
  ylim(c(0,1)) +
  background_grid()

ggsave("figures/nice/prop~inactive_by_id.z.png",
       width = 10,
       height = 7,
       bg = "white")

ggplot(quourm_dominance[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, prop, alpha = tot, color = dominance)) + 
  geom_smooth(aes(avg_dominance_inactive, prop, weight = tot,
                  color = dominance, group = id, lty = sex,
                  ), 
              linewidth = 1,
              method = "glm",
              method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "magma",
                        name = "Inactive") +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, prop, alpha = tot, color = dominance)) + 
  geom_smooth(aes(avg_dominance_inactive, prop, weight = tot,
                  color = dominance, group = id, lty = sex,
  ), 
  linewidth = 1,
  method = "gam",
  method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "magma",
                        name = "Inactive") +
  ylim(c(0,1)) +
  background_grid()


# also this one !!!!!!!!!!!!!!!!!!!!!!!
ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, prop, alpha = tot, color = dominance)) + 
  geom_smooth(aes(avg_dominance_inactive, prop, weight = tot, #lty = sex,
                  group = id, color = dominance), 
              linewidth = 1,
              method = "glm",
              method.args = list(family = "binomial")) + 
  scale_color_viridis_c(option = "magma",
                        name = "Dominance") +
  facet_wrap(~number_inactive_excluding_dominance) +
  ylim(c(0,1)) +
  background_grid()

ggsave("figures/nice/prop~dominance_by_inactive.z.png",
       width = 10,
       height = 7,
       bg = "white")



ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, posterior, color = dominance, lty = sex)) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~number_inactive_excluding_dominance) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, posterior, color = dominance, lty = sex)) + 
  geom_smooth(aes(avg_dominance_inactive, posterior, group = id, color = dominance, lty = sex), 
              linewidth = 1,
              method="glm",
              method.args = list(family="binomial")) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~number_inactive_excluding_dominance) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  geom_smooth(aes(avg_dominance_inactive, posterior, group = id, color = dominance, lty = sex), 
              linewidth = 1, se = F,
              method="glm",
              method.args = list(family="binomial")) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~number_inactive_excluding_dominance) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  geom_smooth(aes(avg_dominance_inactive, posterior, group = id, color = dominance, lty = sex), 
              linewidth = 1, se = F,
              method="glm",
              method.args = list(family="binomial")) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~id) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  #geom_point(aes(avg_dominance_inactive, posterior, color = dominance, lty = sex)) + 
  geom_smooth(aes(avg_dominance_inactive, posterior, group = number_inactive_excluding_dominance, color = number_inactive_excluding_dominance, lty = sex), 
              linewidth = 1,
              method = "lm") + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~id) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  #geom_point(aes(avg_dominance_inactive, posterior, color = dominance, lty = sex)) + 
  geom_smooth(aes(avg_dominance_inactive, posterior, group = number_inactive_excluding_dominance, color = number_inactive_excluding_dominance, lty = sex), 
              linewidth = 1,
              method = "lm") + 
  scale_color_viridis_c(option = "viridis") +
  facet_grid(rows = vars(id), cols = vars(number_inactive_excluding_dominance)) +
  ylim(c(0,1)) +
  background_grid()

# likelihood
ggplot(quourm_dominance_control[point == "start"]) +
  geom_point(aes(avg_dominance_inactive, likelihood, color = dominance, lty = sex)) + 
  geom_smooth(aes(avg_dominance_inactive, likelihood, color = dominance, lty = sex), 
              linewidth = 1,
              method = "loess") + 
  scale_color_viridis_c(option = "viridis") +
  facet_grid(rows = vars(id), cols = vars(number_inactive)) +
  ylim(c(0,1)) +
  background_grid()

ggplot(quourm_dominance_control[point == "start"]) +
  #geom_point(aes(avg_dominance_inactive, posterior, color = dominance, lty = sex)) + 
  geom_smooth(aes(avg_dominance_inactive, likelihood, group = number_inactive, color = dominance, lty = sex), 
              linewidth = 1,
              method = "lm") + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~id) +
  ylim(c(0,1)) +
  background_grid()

# Methods ----

## Table of sunrise and sunset ----
png("figures/mc_results/final/astro_times.png", height=800, width=600)
data_astronomical[, c("sunrise", "sunset", "nightEnd", "night")
][date(sunrise) >= date("2018-08-01") & date(sunrise) < date("2018-08-27")] |>
  tableGrob() |>
  grid.arrange()
dev.off()


## Distribution of length of resting ----
# for data
ggplot(generative_data_prep_check_s |>
         merge(dominance_id)) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  scale_color_discrete(name = "State",
                       labels = c("Active", "Rest")) +
  facet_wrap(~baboon) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "frequency_duration.png"),
       height = 10,
       width = 15,
       bg = "white")

# for model
ggplot(check_length |>
         merge(dominance_id)) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  scale_color_discrete(name = "State",
                       labels = c("Active", "Rest")) +
  facet_wrap(~baboon) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "frequency_duration_model.png"),
       height = 10,
       width = 15,
       bg = "white")

## Distribution of length of resting ----
ggplot(time_correlation_synchro[id_focus != id_other & 
                                  shift <= 50 & shift >= -50] |>
         merge(dominance_id[, c("id", "baboon")] |>
                 rename(baboon_focus = baboon,
                        id_focus = id)) |>
         left_join(dominance_id[, c("id", "baboon")] |>
                     rename(baboon_other = baboon,
                            id_other = id))) + 
  geom_vline(aes(xintercept = 0), lty = "dashed") +
  geom_line(aes(shift, syncro_prob, 
                color = baboon_other, 
                group = baboon_other), 
            linewidth = 1) +
  geom_point(aes(peak_shift, peak_syncro,
                 color = baboon_other),
             size = 5) +
  scale_color_viridis_d(option = "turbo",
                        name = "Other\nindividual") +
  facet_wrap(~baboon_focus) +
  ylab("Influence") +
  xlab("Time shift (s)") +
  background_grid()

ggplot(time_correlation_synchro[id_focus != id_other & 
                                  shift <= 50 & shift >= -50] |>
         merge(dominance_id[, c("id", "baboon")] |>
                 rename(baboon_focus = baboon,
                        id_focus = id)) |>
         left_join(dominance_id[, c("id", "baboon")] |>
                     rename(baboon_other = baboon,
                            id_other = id))) + 
  geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
  geom_line(aes(shift, ratio, 
                color = baboon_other, 
                group = baboon_other), 
            linewidth = 1) +
  geom_point(aes(peak_shift, peak_ratio,
                 color = baboon_other),
             size = 5) +
  scale_color_viridis_d(option = "turbo",
                        name = "Other\nindividual") +
  facet_wrap(~baboon_focus) +
  ylab("Influence") +
  xlab("Time shift (s)") +
  background_grid()

ggplot(time_correlation_synchro[id_focus != id_other & 
                                  shift <= 100 & shift >= -100] |>
         merge(dominance_id[, c("id", "baboon")] |>
                 rename(baboon_focus = baboon,
                        id_focus = id)) |>
         left_join(dominance_id[, c("id", "baboon")] |>
                     rename(baboon_other = baboon,
                            id_other = id))) + 
  geom_vline(aes(xintercept = 0), lty = "dashed") +
  geom_line(aes(shift, normalized_prob, 
                color = baboon_other, 
                group = baboon_other), 
            linewidth = 1) +
  geom_point(aes(peak_shift, peak_normalized_prob,
                 color = baboon_other),
             size = 2) +
  scale_color_viridis_d(option = "turbo",
                        name = "Other\nindividual") +
  facet_wrap(~baboon_focus) +
  ylab("Synchrony probability / average synchrony probability") +
  xlab("Time shift (s)") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "lagged_correlation_all.png"),
       height = 10,
       width = 15,
       bg = "white")

## Steady state and markov chain assumption ----

(sup_p1 <- copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs, na.rm = T)),
                                                by = list(id, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T)), 
  by = list(id, dominance)] |>
  merge(dominance_id) |>
  ggplot() +
  geom_point(aes(dominance, predicted_sleep), shape = 21,
             size = 3, stroke = 1, fill = NA) +
  geom_point(aes(dominance, real_sleep, 
                 color = baboon), size = 2) +
  scale_color_viridis_d(name = "Individual\ndata",
                        option = "turbo") +
  xlab("Dominance") +
  ylab(expression("Rest probability  "(p[R]))) +
  ggtitle("A)") +
  guides(color = "none") +
  background_grid())

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "fig2_predictions.png"),
       height = 3.5,
       width = 5,
       bg = "white")

(sup_p2 <- copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs, na.rm = T)),
                                                by = list(id, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T)), 
  by = list(id, dominance)][, ":="(residual = predicted_sleep - real_sleep)] |>
    merge(dominance_id) |>
    ggplot() +
    geom_point(aes(dominance, residual, 
                   color = baboon), size = 5) +
    geom_segment(aes(y = rep(0, 12), yend = residual, 
                     x = dominance, xend = dominance,
                     color = baboon), linewidth = 2, linetype = "dotted") +
    geom_hline(aes(yintercept = 0), linewidth = 2, 
               linetype = "dashed", color = "darkgrey") +
    scale_color_viridis_d(name = "Individual",
                          option = "turbo") +
    xlab("Dominance") +
    ylab("Residual") +
    ggtitle("B)") +
    background_grid())

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "fig2_residuals.png"),
       height = 3.5,
       width = 5,
       bg = "white")

(supp_p1 <- ggarrange(sup_p1, sup_p2,
                      ncol = 2, nrow = 1,
                      widths = c(1, 1)))

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "methods", "model_validation.png"),
       height = 3.5,
       width = 10,
       bg = "white")

0.001 * 9 * 25

## Cross correlation ----

time_correlation_s_all[id_focus != id_other & shift < 50 & shift > -50] |>
  ggplot() +
  geom_vline(aes(xintercept = 0), lty = "dashed", linewidth = 2) +
  geom_line(aes(shift, rho, color = dominance_other, group = id_other)) +
  geom_point(aes(peak_time, peak_rho, 
                 color = dominance_other),
             size = 2) +
  scale_color_viridis_c(name = "Dominance\nother",
                        option = "plasma") +
  facet_wrap(~baboon_focus) +
  xlab("Shift (seconds)") +
  ylab(~paste("Cross correlation \u03c6 / ", "\u03c6"[max])) +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "methods", "cross_correlation.png"),
       height = 10,
       width = 10,
       bg = "white")

# no change for when they sleep in threes rather than urban (must do)

# Results ----

# ggplot(predictions_rain[avg_dominance_sleep == 0.5 & number_sleeping_excluding_focus == 9 & dominance == 0.5]) +
#   geom_ribbon(aes(x = precipitation_mm, 
#                   ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
#                   ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
#               color = NA, alpha = 0.2) +
#   geom_line(aes(precipitation_mm, p_rest)) +
#   scale_color_viridis_c(name = "Dominance",
#                         option = "plasma") +
#   scale_fill_viridis_c(name = "DOminance",
#                        option = "plasma") +
#   xlab("Precipitation (mm)") +
#   background_grid() +
#   guides(fill = "none") +
#   ylab(expression("Rest probability  "(p[R]))) 
# 
# ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "precipitation.png"),
#        height = 3.5,
#        width = 4,
#        bg = "white")


## First results (overview) ----

### Model checks ----
copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs)),
                                     by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs),
      tot_obs = sum(tot_obs)), 
  by = list(id, id_night, dominance)] |>
  merge(dominance_id) |>
  ggplot() +
  geom_point(aes(id_night, real_sleep, color = dominance, group = id)) +
  geom_line(aes(id_night, predicted_sleep, color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  ylab(expression("Rest probability  "(p[R]))) +
  scale_x_continuous(name = "Date",
                     breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
                     labels = str_replace(unique(night_sleep$date_start_night), "2018-", "")[seq(0, 27, by = 7) + 1]) +
  facet_wrap(~baboon) +
  xlab("Night id") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "fig1_predictions.png"),
       height = 10.5,
       width = 16,
       bg = "white")

copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs, na.rm = T)),
                                     by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T),
      tot_obs = sum(tot_obs)), 
  by = list(id, id_night, dominance)][, ":="(residual = predicted_sleep - real_sleep)]  |>
  merge(dominance_id) |>
  ggplot() +
  geom_hline(aes(yintercept = 0), linewidth = 2, 
             linetype = "dashed", color = "darkgrey") +
  geom_point(aes(id_night, residual, #alpha = tot_obs,
                 color = dominance), size = 3) +
  geom_segment(aes(y = rep(0, 300), yend = residual, 
                   x = id_night, xend = id_night,
                   color = dominance), linewidth = 1, linetype = "dotted") +
  scale_color_viridis_c(option = "plasma") +
  xlab("Night id") +
  ylab("Residual") +
  facet_wrap(~baboon) +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "fig1_residuals.png"),
       height = 10.5,
       width = 16,
       bg = "white")

## Second results (individual characteristics) ----

ggplot(day_inactive) +
  geom_point(aes(dominance, freq_sleep), size = 3) +
  # geom_smooth(aes(dominance, freq_sleep, 
  #                 #group = sex,
  #                 #lty = sex,
  #                 weight = tot), 
  #             method = "glm", color = "red",
  #             method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  ylab("Daytime rest") +
  xlab("Dominance") +
  guides(linetype = "none") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", "supp", "daytime_rest.png"),
       height = 3.5,
       width = 4,
       bg = "white")

## Third results (social stuff) ----

(p3.3 <- ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61]) +
   geom_ribbon(aes(x = number_sleeping_excluding_focus,
                   fill = dominance, group = dominance,
                   ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se),
                   ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
               color = NA, alpha = 0.3) +
   geom_line(aes(number_sleeping_excluding_focus, 
                 p_rest, color = dominance, group = dominance),
             linewidth = 1) +
   scale_color_viridis_c(name = "Dominance",
                         option = "plasma") +
   scale_fill_viridis_c(name = "Dominance",
                        option = "plasma") +
   ggtitle("C)") +
   background_grid() +
   guides(color = "none", fill = "none") +
   xlab("Number individuals resting") +
   ylab(expression("Rest probability "(p[R]))))

# pairwise synchrony

ggplot(lk_network[id != id_pair][syncrony %in% c("syncro")
] |> merge(dominance_id)) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = baboon)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = baboon),
              method = "lm", linewidth = 2, fullrange = T,
              se = F) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance\nfocus\nindividual") +
  ylab("Synchrony") +
  scale_x_continuous(name = "Dominance other baboon",
                     breaks = c(0, 0.5, 1),
                     labels = c("0", "0.5", "1")) +
  facet_wrap(~baboon, ncol = 12) +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "pairwise_synchrony.png"),
       height = 3.5,
       width = 15,
       bg = "white")

# number of individuals graph
ggplot(model_check_predictions_simple[, .(freq_transition = weighted.mean(freq_transition, tot_obs, na.rm = T),
                                          predict_transition_active = weighted.mean(predict_transition_active, tot_obs, na.rm = T),
                                          tot_obs = sum(tot_obs)),
                                      by = list(period, state, id, number_sleeping_excluding_focus)
][period == "night_time"][state == F] |>
  merge(dominance_id)) +
  geom_line(aes(number_sleeping_excluding_focus, 1 - predict_transition_active, 
                color = baboon)) +
  geom_point(aes(number_sleeping_excluding_focus, 1 - freq_transition, 
                 color = baboon, size = tot_obs)) +
  # geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
  #               color = avg_dominance_sleep), lty = "dashed",
  #           linewidth = 1) +
  facet_wrap(~baboon) +
  scale_color_viridis_d(option = "turbo",
                        name = "Individual") +
  scale_size_continuous(name = "Sample\nsize (s)") +
  ylab(expression("Probability of continue activity "(p[A%->%A]))) +
  xlab("Number of resting individuals") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "residuals_activity_number_resting.png"),
       height = 10,
       width = 15,
       bg = "white")

ggplot(model_check_predictions_simple[, .(freq_transition = weighted.mean(freq_transition, tot_obs, na.rm = T),
                                          predict_transition_rest = weighted.mean(predict_transition_rest, tot_obs, na.rm = T),
                                          tot_obs = sum(tot_obs)),
                                      by = list(period, state, id, number_sleeping_excluding_focus)
][period == "night_time"][state == T] |>
  merge(dominance_id)) +
  geom_line(aes(number_sleeping_excluding_focus, 1 - predict_transition_rest, 
                color = baboon)) +
  geom_point(aes(number_sleeping_excluding_focus, 1 - freq_transition, 
                 color = baboon, size = tot_obs)) +
  # geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
  #               color = avg_dominance_sleep), lty = "dashed",
  #           linewidth = 1) +
  facet_wrap(~baboon) +
  scale_color_viridis_d(option = "turbo",
                        name = "Individual") +
  scale_size_continuous(name = "Sample\nsize (s)") +
  ylab(expression("Rest quality "(p[R%->%R]))) +
  xlab("Number of resting individuals") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "residuals_rest_number_resting.png"),
       height = 10,
       width = 15,
       bg = "white")

model_check_predictions_simple[, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
                                   n_obs = sum(tot_obs)),
                               by = list(period, state, id, number_sleeping_excluding_focus)
][period == "night_time"
][, ":="(tot = sum(n_obs)),
  by = list(period, id, number_sleeping_excluding_focus)
][, ":="(real_sleep = n_obs/tot)
][state == T] |>
  merge(dominance_id) |>
  ggplot() +
  geom_line(aes(number_sleeping_excluding_focus, predicted_sleep, 
                color = baboon)) +
  geom_point(aes(number_sleeping_excluding_focus, real_sleep, 
                 color = baboon, size = tot)) +
  facet_wrap(~baboon) +
  scale_color_viridis_d(option = "turbo",
                        name = "Individual") +
  scale_size_continuous(name = "Sample\nsize (s)") +
  ylab(expression("Rest probabability"(p[R]))) +
  xlab("Number of resting individuals") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "residuals_total_rest_number_resting.png"),
       height = 10,
       width = 15,
       bg = "white")

model_check_predictions_simple[period == "night_time" & state == T
][, ":="(total_sleep = sum(tot_obs),
         weight = tot_obs / sum(tot_obs)),
  by = id
][, .(predicted_sleep = sum(predicted_sleep * weight, na.rm = T), # expectation
      real_sleep = sum(tot_obs) / unique(total_sleep),
      sample_size = sum(tot_obs)),
  by = list(id, number_sleeping_excluding_focus)
] |>
  merge(dominance_id) |>
  ggplot() +
  geom_line(aes(number_sleeping_excluding_focus, predicted_sleep, 
                color = baboon)) +
  geom_point(aes(number_sleeping_excluding_focus, real_sleep, 
                 color = baboon, size = sample_size)) +
  facet_wrap(~baboon) +
  scale_color_viridis_d(option = "turbo",
                        name = "Individual") +
  scale_size_continuous(name = "Sample\nsize (s)") +
  ylab("Rest probabability (conditioned on focus individual resting)") +
  xlab("Number of resting individuals") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "residuals_total_rest_number_resting_conditional.png"),
       height = 10,
       width = 15,
       bg = "white")

# effect of dominance on transition probabilities

ggplot(data = predictions_dominance[number_sleeping_excluding_focus == 11]) +
  geom_ribbon(aes(x = avg_dominance_sleep, 
                  ymin = (1 - p_as - p_as_se), 
                  ymax = (1 - p_as + p_as_se),
                  fill = dominance, group = dominance),
              color = NA, alpha = 0.2) +
  geom_line(aes(avg_dominance_sleep, 1 - p_as, group = dominance, color = dominance)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance\nfocus\nindividual") +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance\nfocus\nindividual") +
  guides(fill = "none") +
  ylab(expression("Probability of continue activity "(p[A%->%A]))) +
  xlab("Average dominance sleeping individuals") +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "transition_aa_dominance.png"),
       height = 4,
       width = 4.5,
       bg = "white")

ggplot(data = predictions_dominance[number_sleeping_excluding_focus == 11]) +
  geom_ribbon(aes(x = avg_dominance_sleep, 
                  ymin = (1 - p_sa - p_sa_se), 
                  ymax = (1 - p_sa + p_sa_se),
                  fill = dominance, group = dominance),
              color = NA, alpha = 0.2) +
  geom_line(aes(avg_dominance_sleep, 1 - p_sa, group = dominance, color = dominance)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance\nfocus\nindividual") +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance\nfocus\nindividual") +
  guides(fill = "none") +
  ylab(expression("Rest quality "(p[R%->%R]))) +
  background_grid()

ggsave(filename = here("figures", "mc_results", "manuscript_1", 
                       "supp", "transitions_ss_dominance.png"),
       height = 4,
       width = 4.5,
       bg = "white")

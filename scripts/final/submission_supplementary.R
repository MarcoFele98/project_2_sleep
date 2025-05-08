# Results ----
## Baboons synchonize sleep ----
(p_r_s.1 <- ggplot(night_sleep[!(id_night %in% c(1, 25))]) +
    geom_point(aes(date_start_night, time_sleep, color = dominance), size = 3) +
    geom_smooth(aes(date_start_night, time_sleep, 
                    weight = tot), 
                method = "lm", color = "blue", se = T) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance") +
    #guides(color = "none") +
    ylab("Nighttime sleep (hours)") +
    xlab("Date"))

ggsave(plot = p_r_s.1,
       filename = "figures/submission/supp/results/Baboons synchonize sleep/p_r_s.1.png",
       height = 4,
       width = 5,
       bg = "white")

summary(model_night_duration)
coeff_model_night_duration <- tidy(model_night_duration) |>
  mutate(is_significant = ifelse(p.value < 0.01, T, F),
         p.value = round(p.value, 3))
png(paste0("figures/submission/supp/results/model_night_duration_", threshold, ".png"), 
    width = 1100)
grid.table(coeff_model_night_duration)
dev.off()

summary(m_active_simple)
coeff_summary_active <- tidy(m_active_simple) |>
  mutate(is_significant = ifelse(p.value < 0.01, T, F),
         p.value = round(p.value, 3))
png(paste0("figures/submission/supp/results/", threshold, "_active.png"), 
    width = 1100)
grid.table(coeff_summary_active)
dev.off()

summary(m_rest_simple)
coeff_summary_rest <- tidy(m_rest_simple) |>
  mutate(is_significant = ifelse(p.value < 0.01, T, F),
         p.value = round(p.value, 3))
png(paste0("figures/submission/supp/results/", threshold, "_rest.png"), 
    width = 1100)
grid.table(coeff_summary_rest)
dev.off()

## Dominant baboons have less and lower quality sleep ----
predictions_dominance[period == "night_time"
][, ":="(p_ass = 1 - p_sa,
         p_aa = 1 - p_as)
][, c("dominance", "p_rest", "p_aa", "p_ass", "number_sleeping_excluding_focus", "avg_dominance_sleep")] |>
  pivot_longer(cols = c("p_rest", "p_aa", "p_ass"),
               names_to = "probability_type",
               values_to = "probability") |>
  ggplot() +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = 1 - p_as - p_as_se,
                  ymax = 1 - p_as + p_as_se),
              fill = "#F8766D", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = 1 - p_sa - p_sa_se,
                  ymax = 1 - p_sa + p_sa_se),
              fill = "#00BFC4", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se),
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
              fill = "black", color = NA, alpha = 0.3) +
  geom_line(aes(dominance, 
                probability, 
                color = probability_type)) +
  geom_line(data = predictions_dominance[period == "night_time"],
            aes(dominance, p_rest),
            linewidth = 3) +
  scale_color_manual(name = "",
                     labels = c(expression("Wakefulness "(widehat(p)[A%->%A])),
                                expression("Sleep quality "(widehat(p)[S%->%S])),
                                expression("Sleep quantity "(widehat(p)[S]))),
                     values = c("#F8766D", "#00BFC4", "black")) +
  ylab("Probability") +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  xlab("Dominance")

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/full_model.png",
         height = 40,
         width = 40,
         bg = "white")

ggplot(predictions_dominance[avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6]) +
  geom_ribbon(aes(x = dominance,
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se) * avg_night_duration,
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se) * avg_night_duration),
              fill = "black", color = NA, alpha = 0.3) +
  geom_line(aes(dominance, p_rest * avg_night_duration),
            linewidth = 3) +
  ylab("Hours") +
  xlab("Dominance")

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/hours.png",
       height = 5,
       width = 5,
       bg = "white")

ggplot(predictions_dominance) +
  geom_ribbon(aes(x = dominance,
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se) * avg_night_duration,
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se) * avg_night_duration),
              fill = "black", color = NA, alpha = 0.3) +
  geom_line(aes(dominance, p_rest * avg_night_duration),
            linewidth = 3) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  ylab("Hours") +
  xlab("Dominance")

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/full_model_hours.png",
       height = 40,
       width = 40,
       bg = "white")

as.data.table(predictions_dominance[period == "night_time"
][, ":="(p_ass = 1 - p_sa,
         p_aa = 1 - p_as)
][, c("dominance", "p_rest", "p_aa", "p_ass", "number_sleeping_excluding_focus", "avg_dominance_sleep")] |>
  pivot_longer(cols = c("p_rest", "p_aa", "p_ass"),
               names_to = "probability_type",
               values_to = "probability")
)[, ":="(hours = probability * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                by = id]$tot_sleep))] |>
  ggplot() +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = (1 - p_as - p_as_se) ** mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                  by = id]$tot_sleep),
                  ymax = (1 - p_as + p_as_se) * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                 by = id]$tot_sleep)),
              fill = "#F8766D", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = (1 - p_sa - p_sa_se) * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                 by = id]$tot_sleep),
                  ymax = (1 - p_sa + p_sa_se) * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                 by = id]$tot_sleep)),
              fill = "#00BFC4", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[period == "night_time"],
              aes(x = dominance,
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se) * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                                                 by = id]$tot_sleep),
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se) * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                                                                 by = id]$tot_sleep)),
              fill = "black", color = NA, alpha = 0.3) +
  geom_line(aes(dominance, 
                hours, 
                color = probability_type)) +
  geom_line(data = predictions_dominance[period == "night_time"],
            aes(dominance, p_rest * mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                                                     by = id]$tot_sleep)),
            linewidth = 3) +
  scale_color_manual(name = "",
                     labels = c(expression("Wakefulness "(widehat(p)[A%->%A])),
                                expression("Sleep quality "(widehat(p)[S%->%S])),
                                expression("Sleep quantity "(widehat(p)[S]))),
                     values = c("#F8766D", "#00BFC4", "black")) +
  ylab("Hours") +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  xlab("Dominance")

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/full_model_hours.png",
       height = 40,
       width = 40,
       bg = "white")

(p2.4 <- ggraph(net_null_model |>   
                  activate(edges)  |>
                  filter(syncrony == "syncro",
                         ratio > quantile(ratio, probs = 0.4)),
                layout = "stress") +#"kk"
    geom_edge_link(
      aes(color = as.factor(ratio),
          width = ratio)) + 
    scale_edge_width_continuous(range = c(0.2, 3)) +
    scale_edge_color_manual(values = mako(n = 60, direction = -1)) + # 263
    geom_node_point(
      aes(#fill = as.factor(node_number),
        fill = dominance,
        size = dominance), 
      stroke = 1,
      #fill = "black", 
      color = "black", shape = 21) +
    scale_size_continuous(range = c(1, 5)) +
    scale_fill_viridis_c(option = "plasma") +
    geom_node_label(
      aes(label = baboon
          #fill = dominance
      ),
      nudge_y = 0.2
    ) +
    theme(legend.position = 'none'))

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/network.png",
       height = 5,
       width = 5,
       bg = "white")

ggplot(predictions_dominance[number_sleeping_excluding_focus == 6]) +
    geom_ribbon(aes(x = avg_dominance_sleep,
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se),
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se),
                    fill = dominance, group = dominance),
                color = NA, alpha = 0.2) +
    geom_line(aes(avg_dominance_sleep, p_rest, group = dominance,
                  color = dominance)) +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "Dominance",
                         option = "plasma") +
    xlab("Average dominance of sleeping individuals") +
    #background_grid() +
    guides(colour = guide_colourbar(title.position="top")) +
    theme(legend.direction = "horizontal",
          legend.key.width = unit(0.8, "cm"),
          #legend.background = element_rect(fill = "white"),
          legend.position = c(0.45, 0.2)) +
    ylab(expression("Sleep probability  "(widehat(p)[S])))

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/dominance_sleepers.png",
       height = 5,
       width = 5,
       bg = "white")

ggplot(data = predictions_dominance[number_sleeping_excluding_focus == 11]) +
  geom_ribbon(aes(x = avg_dominance_sleep, 
                  ymin = (1 - p_as - p_as_se), 
                  ymax = (1 - p_as + p_as_se),
                  fill = dominance, group = dominance),
              color = NA, alpha = 0.2) +
  geom_line(aes(avg_dominance_sleep, 1 - p_as, group = dominance, color = dominance)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance") +
  guides(fill = "none") +
  ylab(expression("Wakefulness "(p[A%->%A]))) +
  xlab("Average dominance of sleeping individuals") 

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/dominance_sleepers_1.png",
       height = 5,
       width = 5,
       bg = "white")

ggplot(data = predictions_dominance[number_sleeping_excluding_focus == 11]) +
  geom_ribbon(aes(x = avg_dominance_sleep, 
                  ymin = (1 - p_sa - p_sa_se), 
                  ymax = (1 - p_sa + p_sa_se),
                  fill = dominance, group = dominance),
              color = NA, alpha = 0.2) +
  geom_line(aes(avg_dominance_sleep, 1 - p_sa, group = dominance, color = dominance)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance") +
  guides(fill = "none") +
  ylab(expression("Sleep quality "(p[S%->%S]))) +
  xlab("Average dominance of sleeping individuals") 

ggsave(filename = "figures/submission/supp/results/Dominant baboons have less and lower quality sleep/dominance_sleepers_2.png",
       height = 5,
       width = 5,
       bg = "white")

## Why do dominant baboons have worse sleep ? ----

ggraph(mechanism_net |>   
         activate(edges)  |>
         filter(proximity > quantile(proximity, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    width = 2,
    aes(color = log(proximity)
        #width = peak_rho
    )) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      fill = dominance,
      size = dominance
    ), 
    #size = 8, 
    stroke = 2, shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_node_label(
    aes(label = baboon),
    nudge_y = 0.05
  )

ggsave(filename = "figures/submission/supp/results/why do dominant baboons have worse sleep/network_proximity.png",
       height = 10,
       width = 10,
       bg = "white")


# Methods ----
## Study site and troop ----

(ps.1 <- ggplot(night_sleep) +
  geom_vline(aes(xintercept = 11 - 2), lty = "dotted") +
  geom_vline(aes(xintercept = 19 - 2), lty = "dotted") +
  geom_vline(aes(xintercept = 24 - 2), lty = "dotted") +
  geom_boxplot(aes(as.factor(id_night), freq_sleep, group = id_night)) +
  # geom_line(aes(as.factor(id_night), freq_sleep, 
  #               color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  # theme(legend.position = c(0.5, 0.25),
  #       legend.background = element_rect(fill = "white")) +
  guides(color = "none") +
  ylab(expression("Obserevd probability of sleep  "(p[S]))) +
  scale_x_discrete(name = NULL,
                   breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
                   labels = str_replace(unique(night_sleep$date_start_night), "2018-08-", "Aug ")[seq(0, 27, by = 7) + 1]) +
  ggtitle("A)")
)

(ps.2 <- ggplot(night_sleep) +
    geom_vline(aes(xintercept = 11 - 2), lty = "dotted") +
    geom_vline(aes(xintercept = 19 - 2), lty = "dotted") +
    geom_vline(aes(xintercept = 24 - 2), lty = "dotted") +
    geom_line(aes(id_night, freq_sleep, color = dominance),
              linewidth = 1) +
    geom_point(aes(id_night, freq_sleep,
                  color = dominance),
               size = 2) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance") +
    ylab(expression("Obserevd probability of sleep  "(p[S]))) +
    xlab("Night identity") +
    # scale_x_discrete(name = NULL,
    #                  breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
    #                  labels = str_replace(unique(night_sleep$date_start_night), "2018-08-", "Aug ")[seq(0, 27, by = 7) + 1]) + 
    facet_wrap(~baboon) +
    ggtitle("B)")
)

(fig_supp_methods.1 <- ggarrange(ps.1, ps.2,
                                 ncol = 1, nrow = 2))

ggsave(plot = fig_supp_methods.1,
       filename = here("figures", "submission", "supp", "fig_supp_methods.1.png"),
       height = 15,
       width = 9,
       bg = "white")

## Night-time sleep ----

png("figures/submission/supp/astro_times.png", height=800, width=600)
data_astronomical[, c("sunrise", "sunset", "nightEnd", "night")
][date(sunrise) >= date("2018-08-01") & date(sunrise) < date("2018-08-27")] |>
  tableGrob() |>
  grid.arrange()
dev.off()

### Validation of Markov chian model ----

ggplot(generative_data_prep_check_s |>
         merge(dominance_id)) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  scale_color_discrete(name = "State",
                       labels = c("Active", "Sleep")) +
  facet_wrap(~baboon) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/log_log.png",
       height = 10,
       width = 15,
       bg = "white")

ggplot(check_length |>
         merge(dominance_id)) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  scale_color_discrete(name = "State",
                       labels = c("Sleep", "Rest")) +
  facet_wrap(~baboon) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()

ggsave(filename =  "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/model_exponential.png",
       height = 10,
       width = 15,
       bg = "white")

(ps.2.1 <- copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs, na.rm = T)),
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
    ylab(expression("Sleep probability  "(p[S]))) +
    ggtitle("A)") +
    guides(color = "none") +
    background_grid())

(ps.2.2 <- copy(model_check_predictions_simple)[, ":="(total = sum(tot_obs, na.rm = T)),
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

(ps.2 <- ggarrange(ps.2.1, ps.2.2,
                      ncol = 2, nrow = 1,
                      widths = c(1, 1)))

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_ind.png",
       height = 3.5,
       width = 10,
       bg = "white")

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
  ylab(expression("Sleep probability  "(p[S]))) +
  scale_x_continuous(name = "Date",
                     breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
                     labels = str_replace(unique(night_sleep$date_start_night), "2018-", "")[seq(0, 27, by = 7) + 1]) +
  facet_wrap(~baboon) +
  xlab("Night id") +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_night.png",
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

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_night_2.png",
       height = 10.5,
       width = 16,
       bg = "white")

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
  scale_size_continuous(name = "Sample\nsize") +
  ylab(expression("Wakefulness "(p[A%->%A]))) +
  xlab("Number of sleeping individuals") +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_n_sleep.png",
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
  scale_size_continuous(name = "Sample\nsize") +
  ylab(expression("Sleep quality "(p[S%->%S]))) +
  xlab("Number of sleeping individuals") +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_n_sleep_2.png",
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
  scale_size_continuous(name = "Sample\nsize") +
  ylab(expression("Sleep probability "(p[S]))) +
  xlab("Number of sleeping individuals") +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_n_sleep_sleep_3.png",
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
  ylab("Sleep probabability (conditioned on focus individual sleeping)") +
  xlab("Number of sleeping individuals") +
  background_grid()

ggsave(filename = "figures/submission/supp/sleep quantity, sleep quality, and wakefullness/residual_n_sleep_4.png",
       height = 10,
       width = 15,
       bg = "white")









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
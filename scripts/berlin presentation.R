p1 <- ggplot(copy(combined_night_m)[, ":="(time = (as.numeric(seconds(time)) + 12 * 60 * 60) %% (24 * 60 * 60)
)][, ":="(date = if_else(time > 12 * 60 * 60, 
                         date - days(1), 
                         date))
]) +
  geom_tile(aes(time, date, fill = freq_sleeping)) +
  geom_line(data = copy(data_astronomical)[, ":="(sunrise_time = (as.numeric(seconds(sunrise_time)) + 12 * 60 * 60) %% (24 * 60 * 60))
  ][date %in% unique(combined_m[number_individuals == 12][["date"]])
  ][, ":="(date = if_else(sunrise_time > 12 * 60 * 60, 
                          date - days(1), 
                          date))
  ],
  aes(sunrise_time, date), linewidth = 2, color = "darkred", lty = "dashed") + 
  geom_line(data = copy(data_astronomical)[, ":="(sunset_time = (as.numeric(seconds(sunset_time)) + 12 * 60 * 60) %% (24 * 60 * 60))
  ][date %in% unique(combined_m[number_individuals == 12][["date"]])
  ][, ":="(date = if_else(sunset_time > 12 * 60 * 60, 
                          date - days(1), 
                          date))
  ],
  aes(sunset_time, date), linewidth = 2, color = "darkred", lty = "dashed") + 
  geom_line(data = copy(data_astronomical)[, ":="(night_time = (as.numeric(seconds(night_time)) + 12 * 60 * 60) %% (24 * 60 * 60))
  ][date %in% unique(combined_m[number_individuals == 12][["date"]])
  ][, ":="(date = if_else(night_time > 12 * 60 * 60, 
                          date - days(1), 
                          date))
  ],
  aes(night_time, date), linewidth = 2, color = "darkblue") +
  geom_line(data = copy(data_astronomical)[, ":="(nightEnd_time = (as.numeric(seconds(nightEnd_time)) + 12 * 60 * 60 ) %% (24 * 60 * 60))
  ][date %in% unique(combined_m[number_individuals == 12][["date"]])
  ][, ":="(date = if_else(nightEnd_time > 12 * 60 * 60, 
                          date - days(1), 
                          date))
  ],
  aes(nightEnd_time, date), linewidth = 2, color = "darkblue") +
  scale_x_continuous(name = "Time",
                     breaks = seq(0, 24 * 60 * 60, by = 4 * 60 * 60),
                     labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00")) +
  theme(axis.title = element_blank(),
        text = element_text(size = 20)) +
  scale_fill_gradientn(colors = parula(),
                       name = "Proportion sleeping") +
  guides(fill = guide_colourbar(barwidth = 7,
                                barheight = 20)) +
  geom_text(aes(x = 0.13 * 24 * 60 * 60, y = ymd("2018-08-20"), label = "Sunset"), size = 20, color = "darkred", angle = -90) +
  geom_text(aes(x = 0.5 * 24 * 60 * 60, y = ymd("2018-08-24"), label = "Night"), size = 20, color = "darkblue") +
  geom_text(aes(x = 0.75 * 24 * 60 * 60, y = ymd("2018-08-20"), label = "Sunrise"), size = 20, color = "darkred", angle = -90) 

ggsave(plot = p1,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "n_sleeping.png"),
       height = 30,
       width = 30,
       bg = "white")


(p2 <- ggplot() +
    geom_point(data = night_sleep,
               aes(dominance, freq_sleep), 
               size = 5,
               color = "black") +
    geom_smooth(data = night_sleep,
                aes(dominance, freq_sleep, 
                    weight = tot), 
                method = "glm", linewidth = 3, 
                color = "black",
                method.args = list(family = "binomial")) +
    scale_shape_discrete(name = "") +
    scale_y_continuous(breaks = c(0.64, 0.68, 0.72, 0.76),
                       labels = round(c(0.64, 0.68, 0.72, 0.76) * unique(night_sleep$tot) / (60*60) / 25, 
                                      digits = 1)) +
    theme(legend.position = c(0.1, 0.2),
          legend.background = element_rect(fill = "white")) +
    guides(color = "none",
           lty = "none") +
    ylab("Average hours per night") +
    xlab("Dominance") +
    background_grid())

ggsave(plot = p2,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "sleep_time~dominance.png"),
       height = 3,
       width = 4,
       bg = "white")

(p2 <- ggplot() +
    geom_point(data = night_sleep,
               aes(dominance, freq_sleep), 
               size = 5,
               color = "black") +
    geom_smooth(data = night_sleep,
                aes(dominance, freq_sleep, 
                    weight = tot), 
                method = "glm", linewidth = 3, 
                color = "black",
                method.args = list(family = "binomial")) +
    scale_shape_discrete(name = "") +
    scale_y_continuous(breaks = c(0.64, 0.68, 0.72, 0.76),
                       labels = round(c(0.64, 0.68, 0.72, 0.76) * unique(night_sleep$tot) / (60*60) / 25, 
                                      digits = 1)) +
    theme(legend.position = c(0.1, 0.2),
          legend.background = element_rect(fill = "white")) +
    guides(color = "none",
           lty = "none") +
    ylab("Average resting hours per night") +
    xlab("Dominance") +
    background_grid())

ggsave(plot = p2,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "sleep_time~dominance.png"),
       height = 3.5,
       width = 4,
       bg = "white")

(p3 <- ggplot(day_inactive) +
    geom_point(aes(dominance, freq_sleep), 
               size = 5,
               color = "black") +
    geom_smooth(aes(dominance, freq_sleep, 
                    weight = tot), 
                method = "glm", linewidth = 3, 
                color = "black",
                method.args = list(family = "binomial")) +
    scale_shape_discrete(name = "") +
    scale_y_continuous(breaks = c(0.12, 0.16, 0.20, 0.24, 0.28),
                       labels = round(c(0.12, 0.16, 0.20, 0.24, 0.28) * unique(day_inactive$tot) / (60*60) / 25,
                                      digits = 1)) +
    theme(legend.position = c(0.1, 0.2),
          legend.background = element_rect(fill = "white")) +
    guides(color = "none",
           lty = "none") +
    ylab("Average resting hours per day") +
    xlab("Dominance") +
    background_grid())

ggsave(plot = p3,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "sleep_time~dominance_day.png"),
       height = 3.5,
       width = 4,
       bg = "white")

(p4 <- ggplot(null_n_simultaneously_sleeping) +
    geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
             color = "black") + #, size =1
    # geom_col(aes(n, prob_sleep), fill = NA,
    #          color = "red", size = 2) +
    scale_x_continuous(name = "Number resting",
                       breaks = seq(0, 12, by = 3)) +
    scale_y_continuous(name = "Probability",
                       limits = c(0, 0.25),
                       breaks = seq(0, 0.25, by = 0.05)) +
    background_grid())

ggsave(plot = p4,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "probability~number_sleeping_no_null.png"),
       height = 3.5,
       width = 4,
       bg = "white")

(p5 <- ggplot(null_n_simultaneously_sleeping) +
  geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
           color = "black") + #, size =1
  geom_col(aes(n, prob_sleep), fill = NA,
           color = "red", size = 1) +
  scale_x_continuous(name = "Number resting",
                     breaks = seq(0, 12, by = 3)) +
    scale_y_continuous(limits = c(0, 0.25)) +
  ylab("Probability") +
  background_grid())

ggsave(plot = p5,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "probability~number_sleeping.png"),
       height = 3.5,
       width = 4,
       bg = "white")


(p6 <- ggplot(copy(quourm_sleep_null_model)[, ":="(baboon = factor(baboon, levels = dominance_id$baboon))
][is_sleeping == "Sleeping"] |>
  pivot_longer(cols = c("freq_id_by_sleep", null_posterior),
               names_to = "model",
               values_to = "Probability")) +
  geom_line(aes(number_sleeping, Probability, color = dominance, lty = model, group = model),
            linewidth = 2) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  scale_linetype_discrete(name = "",
                          labels = c("Data", "Null model")) +
  scale_x_continuous(name = "Number resting",
                     breaks = seq(0, 12, by = 4)) +
  facet_wrap(~baboon, ncol = 12) +
  background_grid())

ggsave(plot = p6,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "P(ID_given_N).png"),
       height = 3.5,
       width = 15,
       bg = "white")

(p7 <- ggplot(quourm_sleep_null_model[is_sleeping == "Sleeping"]) +
    geom_hline(aes(yintercept = 1), size = 2, color = "darkgrey") +
  geom_line(aes(number_sleeping, ratio, color = dominance, group = id),
            linewidth = 1) +
  ylab("Resting preference") +
  xlab("Number resting") +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  background_grid())

ggsave(plot = p7,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "resting_preference.png"),
       height = 4,
       width = 7,
       bg = "white")

(p8 <- ggplot(lk_network[id != id_pair][syncrony %in% c("sleep")]) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2,  fullrange = T, 
              se = F) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance\nfocus\nindividual") +
  facet_wrap(~syncrony, labeller = as_labeller(c(
    `sleep` = "Resting"
  ))) +
  ylab("Pairwise synchrony (data/null model)") +
    xlab("Dominance paired individual") +
  background_grid())

ggsave(plot = p8,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "Pairwise_synchrony_resting.png"),
       height = 4,
       width = 4,
       bg = "white")

(p9 <- ggplot(lk_network[id != id_pair][syncrony %in% c("awake")]) +
    geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
    geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
                method = "lm", linewidth = 2, fullrange = T,
                se = F) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance\nfocus\nindividual") +
    facet_wrap(~syncrony, labeller = as_labeller(c(
      `awake` = "Active"
    ))) +
    ylab("Pairwise synchrony (data/null model)") +
    xlab("Dominance paired individual") +
    background_grid())

ggsave(plot = p9,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "Pairwise_synchrony_active.png"),
       height = 4,
       width = 4,
       bg = "white")

(p10 <- ggplot(lk_network[id != id_pair][syncrony %in% c("syncro")]) +
    geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
    geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
                method = "lm", linewidth = 2, fullrange = T,
                se = F) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance\nfocus\nindividual") +
    ylab("Pairwise synchrony (data/null model)") +
    xlab("Dominance paired individual") +
    background_grid())

ggsave(plot = p10,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "Pairwise_synchrony_syncro.png"),
       height = 4,
       width = 5,
       bg = "white")

(p10.1 <- ggplot(lk_network[id != id_pair][syncrony %in% c("syncro")
                                           ][, ":="(baboon.x = factor(baboon.x, 
                                                                      levels = c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","M1","M2")))]) +
    geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
    geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
                method = "lm", linewidth = 2, fullrange = T,
                se = F) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance\nfocus\nindividual") +
    ylab("Synchrony") +
    scale_x_continuous(name = "Dominance other baboon",
                     breaks = c(0, 0.5, 1),
                     labels = c("0", "0.5", "1")) +
    facet_wrap(~baboon.x, ncol = 12) +
    background_grid())

ggsave(plot = p10.1,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "Pairwise_synchrony_syncro_by_id.png"),
       height = 3.5,
       width = 15,
       bg = "white")


(p11 <- ggraph(net_null_model |>   
                activate(edges)  |>
                filter(syncrony == "syncro",
                       ratio > quantile(ratio, probs = 0.7)),
              layout = "stress") +#"kk"
    geom_edge_link(
      aes(color = as.factor(ratio),
          width = ratio)) + 
    scale_edge_width_continuous(range = c(0.2, 3)) +
    scale_edge_color_manual(values = viridis(n = 45)) + # 263
    geom_node_point(
      aes(#fill = as.factor(node_number),
        fill = dominance,
        size = dominance), 
      stroke = 1,
      #fill = "black", 
      color = "black", shape = 21) +
    scale_size_continuous(range = c(1, 15)) +
    scale_fill_viridis_c(option = "plasma") +
    geom_node_label(
      aes(label = baboon),
      nudge_y = 0.05
    ) +
    theme(legend.position = 'none'))

ggsave(plot = p11,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "network.png"),
       height = 6,
       width = 6,
       bg = "white")

(p12 <- ggraph(net_null_model |>   
                 activate(edges)  |>
                 filter(syncrony == "sleep",
                        ratio > quantile(ratio, probs = 0.3)),
               layout = "stress") +#"kk"
    geom_edge_link(
      aes(color = as.factor(ratio),
          width = ratio)) + 
    scale_edge_width_continuous(range = c(0.2, 3)) +
    scale_edge_color_manual(values = viridis(n = 52)) + # 263
    geom_node_point(
      aes(#fill = as.factor(node_number),
        fill = dominance,
        size = dominance), 
      stroke = 1,
      #fill = "black", 
      color = "black", shape = 21) +
    scale_size_continuous(range = c(1, 15)) +
    scale_fill_viridis_c(option = "plasma") +
    geom_node_label(
      aes(label = baboon),
      nudge_y = 0.05
    ) +
    theme(legend.position = 'none'))

ggsave(plot = p12,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "network_resting.png"),
       height = 6,
       width = 6,
       bg = "white")

(p13 <- ggraph(net_null_model |>   
                 activate(edges)  |>
                 filter(syncrony == "awake",
                        ratio > quantile(ratio, probs = 0.65)),
               layout = "stress") +#"kk"
    geom_edge_link(
      aes(color = as.factor(ratio),
          width = ratio)) + 
    scale_edge_width_continuous(range = c(0.2, 3)) +
    scale_edge_color_manual(values = viridis(n = 66)) + # 263
    geom_node_point(
      aes(#fill = as.factor(node_number),
        fill = dominance,
        size = dominance), 
      stroke = 1,
      #fill = "black", 
      color = "black", shape = 21) +
    scale_size_continuous(range = c(1, 15)) +
    scale_fill_viridis_c(option = "plasma") +
    geom_node_label(
      aes(label = baboon),
      nudge_y = 0.05
    ) +
    theme(legend.position = 'none'))

ggsave(plot = p13,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "presentation", "network_awake.png"),
       height = 6,
       width = 6,
       bg = "white")

# Results 1 Introdution ----

p1 <- copy(combined_night_m)[, ":="(time = (as.numeric(seconds(time)) + 12 * 60 * 60) %% (24 * 60 * 60)
)][, ":="(date = if_else(time > 12 * 60 * 60,
                         date - days(1),
                         date))] |>
  ggplot() +
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
  # theme(axis.title = element_blank(),
  #       text = element_text(size = 20)) +
  scale_fill_gradientn(colors = parula(),
                       name = "Proportion\nresting") +
  # guides(fill = guide_colourbar(barwidth = 7,
  #                               barheight = 20)) +
  geom_text(aes(x = (0.13 + 0.1) * 24 * 60 * 60, y = ymd("2018-08-20"), label = "Sunset"), size = 8, color = "darkred", angle = -90) +
  geom_text(aes(x = 0.5 * 24 * 60 * 60, y = ymd("2018-08-24"), label = "Night"), size = 8, color = "darkblue") +
  geom_text(aes(x = (0.75 + 0.1) * 24  * 60 * 60, y = ymd("2018-08-20"), label = "Sunrise"), size = 8, color = "darkred", angle = -90) +
  theme(axis.title.y = element_blank()) +
  ggtitle("A)") 

(p2 <- ggplot(night_sleep[!(id_night %in% c(1, 25))]) +
    geom_point(aes(date_start_night, time_sleep, color = dominance), size = 3) +
    geom_smooth(aes(date_start_night, time_sleep, 
                    weight = tot), 
                method = "lm", color = "blue", se = T) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance") +
    #guides(color = "none") +
    ylab("Nighttime rest (hours)") +
    ggtitle("B)") +
    guides(colour = guide_colourbar(title.position="top")) +
    theme(axis.title.x=element_blank(),
          legend.direction = "horizontal",
          legend.key.width = unit(0.8, "cm"),
          legend.position = c(0.1, 0.15),
          legend.background = element_rect(fill = "white")))

fig_1.1 <- ggarrange(p1, p2,
                     ncol = 2, nrow = 1,
                     widths = c(2, 1))

(p3 <- ggplot(night_sleep) +
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
    ylab(expression("Obserevd probability of rest  "(p[R]))) +
    scale_x_discrete(name = NULL,
                     breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
                     labels = str_replace(unique(night_sleep$date_start_night), "2018-08-", "Aug ")[seq(0, 27, by = 7) + 1]) +
    # geom_step(data = weather_data[2:25, ],
    #           aes(date, precipitation_mm), 
    #           color = "blue") +
    #background_grid() +
    ggtitle("D)") 
)

(p4 <- ggplot() +
    theme_void())

(fig_1.2 <- ggarrange(p4, p3,
                      ncol = 2, nrow = 1, 
                      widths = c(1, 1)))

fig_1 <- ggarrange(fig_1.1, fig_1.2,
                   ncol = 1, nrow = 2)

ggsave(plot = fig_1,
       filename = here("figures", "mc_results", "final", "fig_1.png"),
       height = 9,
       width = 12,
       bg = "white")

# Results 2 Group and pairwise synchonry ----

(p2.1 <- ggplot(null_n_simultaneously_sleeping) +
   geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
            color = "black") + #, size =1
   geom_col(aes(n, prob_sleep), fill = NA,
            color = "red", linewidth = 1) +
   scale_x_continuous(name = "Number individuals resting",
                      breaks = seq(0, 12, by = 3)) +
   scale_y_continuous(name = "Probability"#,
                      #limits = c(0, 0.25)
   ) +
   ggtitle("A)") +
   #background_grid() +
   #annotate("text", x = 3, y = 0.1, label = "DATA", color = "black")
   # annotate("text", x = 3, y = 0.2, 
   #          fill = "white",
   #          label = "NULL MODEL\nIndividual behaviour\nis independent", 
   #          color = "red") 
   geom_label(aes(x = 3, y = 0.2, label = "NULL MODEL\nIndividual behaviour\nis independent"), 
              fill = "white", label.size = NA,
              color = "red")
)

(p2.2 <- ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5
][, ":="(p_ass = 1 - p_sa,
         p_aa = 1 - p_as)
][, c("number_sleeping_excluding_focus", "p_rest", "p_aa", "p_ass")] |>
  pivot_longer(cols = c("p_rest", "p_aa", "p_ass"),
               names_to = "probability_type",
               values_to = "probability")) +
    geom_ribbon(data = predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5],
                aes(x = number_sleeping_excluding_focus, 
                    ymin = 1 - p_as - p_as_se, 
                    ymax = 1 - p_as + p_as_se), 
                fill = "#F8766D", color = NA, alpha = 0.3) +
    geom_ribbon(data = predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5],
                aes(x = number_sleeping_excluding_focus, 
                    ymin = 1 - p_sa - p_sa_se, 
                    ymax = 1 - p_sa + p_sa_se), 
                fill = "#00BFC4", color = NA, alpha = 0.3) +
    geom_ribbon(data = predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5],
                aes(x = number_sleeping_excluding_focus,
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
                fill = "black", color = NA, alpha = 0.3) +
    geom_line(aes(number_sleeping_excluding_focus, 
                  probability, color = probability_type)) +
    geom_line(data = predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5],
              aes(number_sleeping_excluding_focus, 
                  p_rest),
              linewidth = 2) +
    scale_color_manual(name = "",
                       labels = c(expression("Continue activity "(widehat(p)[A%->%A])),
                                  expression("Rest quality "(widehat(p)[R%->%R])),
                                  expression("Nighttime rest "(widehat(p)[R]))),
                       values = c("#F8766D", "#00BFC4", "black")) +
    ggtitle("B)") +
    xlab("Number of resting individuals (excluding focal)") +
    ylab("Probability")   +
    guides(color = guide_legend(override.aes = list(linewidth = 2))) +
    theme(legend.position = c(0.45, 0.25),
          legend.background = element_rect(fill = "white"),
          legend.title=element_blank()))

(fig2.1 <- ggarrange(p2.1, p2.2, 
                     ncol = 2, nrow = 1))

(p2.3 <- ggplot(lk_network[id != id_pair][syncrony %in% c("syncro")
] |> merge(dominance_id)) +
    geom_point(aes(dominance_pair, ratio, color = dominance, group = baboon)) +
    geom_smooth(aes(dominance_pair, ratio, color = dominance, group = baboon),
                method = "lm", linewidth = 2, fullrange = T,
                se = F) +
    scale_color_viridis_c(option = "plasma",
                          name = "Dominance\nfocal\nindividual") +
    ylab("Pairwise synchrony") +
    scale_x_continuous(name = "Dominance other individual",
                       breaks = c(0, 0.5, 1),
                       labels = c("0", "0.5", "1")) +
    facet_wrap(~baboon, ncol = 12) +
    ggtitle("C)") 
  #facet_grid(~factor(baboon, levels = dominance_id$baboon)) +
  #background_grid()
)

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
    ggtitle("D)") +
    theme(legend.position = 'none'))

(p2.5 <- ggplot(predictions_dominance[number_sleeping_excluding_focus == 6]) +
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
    xlab("Average dominance of resting individuals") +
    #background_grid() +
    ggtitle("E)") +
    guides(colour = guide_colourbar(title.position="top")) +
    theme(legend.direction = "horizontal",
          legend.key.width = unit(0.8, "cm"),
          #legend.background = element_rect(fill = "white"),
          legend.position = c(0.45, 0.2)) +
    ylab(expression("Rest probability  "(widehat(p)[R]))))

(fig2.2 <- ggarrange(p2.4, p2.5, 
                     ncol = 2, nrow = 1))

(fig2 <- ggarrange(fig2.1, p2.3, fig2.2,
                    ncol = 1, nrow = 3))

ggsave(plot = fig2,
       filename = here("figures", "mc_results", "final", "fig2.png"),
       height = 9,
       width = 9,
       bg = "white")

# Results 3 Effect of dominance on individual rest behvaiour ----

(p3.1 <- predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6
][, ":="(p_ass = 1 - p_sa,
         p_aa = 1 - p_as)
][, c("dominance", "p_rest", "p_aa", "p_ass")] |>
  pivot_longer(cols = c("p_rest", "p_aa", "p_ass"),
               names_to = "probability_type",
               values_to = "probability") |>
  ggplot() +
  geom_ribbon(data = predictions_dominance[avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6],
              aes(x = dominance,
                  ymin = 1 - p_as - p_as_se,
                  ymax = 1 - p_as + p_as_se),
              fill = "#F8766D", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6],
              aes(x = dominance,
                  ymin = 1 - p_sa - p_sa_se,
                  ymax = 1 - p_sa + p_sa_se),
              fill = "#00BFC4", color = NA, alpha = 0.3) +
  geom_ribbon(data = predictions_dominance[avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6],
              aes(x = dominance,
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se),
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
              fill = "black", color = NA, alpha = 0.3) +
  geom_line(aes(dominance, 
                probability, 
                color = probability_type)) +
  geom_line(data = predictions_dominance[avg_dominance_sleep > 0.49 & avg_dominance_sleep <= 0.51 & number_sleeping_excluding_focus == 6],
            aes(dominance, p_rest),
            linewidth = 3) +
  scale_color_manual(name = "",
                     labels = c(expression("Continue activity "(widehat(p)[A%->%A])),
                                expression("Rest quality "(widehat(p)[R%->%R])),
                                expression("Nighttime rest "(widehat(p)[R]))),
                     values = c("#F8766D", "#00BFC4", "black")) +
  ylab("Probability") +
  ggtitle("A)") +
  theme(legend.position = c(0.0, 0.2)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  #background_grid() +
  xlab("Dominance"))

(p3.2 <- predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance %in% c(0, 0.5, 1)
][, ":="(pss = 1 - p_sa,
         paa = 1 - p_as)
][, c("number_sleeping_excluding_focus", "dominance", "p_rest", "paa", "pss")] |>
    pivot_longer(cols = c("p_rest", "paa", "pss"),
                 names_to = "probability_type",
                 values_to = "probability") |> 
    filter(probability_type != "p_rest") |>
    mutate(probability_type = factor(probability_type), 
           probability_type = plyr::revalue(probability_type,
                                      c("paa" = expression("Continue activity "(widehat(p)[A%->%A])),
                                      "pss" = expression("Rest quality "(widehat(p)[R%->%R]))))) |>
    ggplot() +
    geom_ribbon(data = predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance %in% c(0, 0.5, 1)
    ][, ":="(paa_min = 1 - p_as + p_as_se,
           paa_max = 1 - p_as - p_as_se,
           pss_min = 1 - p_sa + p_sa_se,
           pss_max = 1 - p_sa - p_sa_se)
  ][, c("number_sleeping_excluding_focus", "dominance", "pss_max", "pss_min", "paa_min", "paa_max")] |>
    pivot_longer(cols = c("pss_max", "pss_min", "paa_min", "paa_max"),
                 names_to = c("probability_type", ".value"),
                 names_sep = "_") |>
    mutate(probability_type = factor(probability_type), 
           probability_type = plyr::revalue(probability_type,
                                            c("paa" = expression("Continue activity "(widehat(p)[A%->%A])),
                                              "pss" = expression("Rest quality "(widehat(p)[R%->%R]))))),
  aes(x = number_sleeping_excluding_focus,
      ymin = min,
      ymax = max,
      fill = dominance, group = dominance),
  color = NA, alpha = 0.3) +
  geom_line(aes(number_sleeping_excluding_focus, 
                probability, color = dominance, group = dominance),
            linewidth = 2) +
  facet_grid(cols = vars(probability_type), 
             labeller = label_parsed) +
  scale_color_viridis(name = "Dominance",
                      option = "plasma") +
  scale_fill_viridis(name = "Dominance",
                     option = "plasma") +
  ggtitle("B)") +
  xlab("Number of resting individuals (excluding focal)") +
  ylab("Probability") +
  guides(fill = "none") 
# theme(legend.position = c(0.45, 0.25),
#       legend.background = element_rect(fill = "white"),
#       legend.title=element_blank())
)

(fig3 <- ggarrange(p3.1, p3.2,
                   ncol = 2, nrow = 1, widths = c(1, 1.5)))

ggsave(plot = fig3,
       filename = here("figures", "mc_results", "final", "fig3.png"),
       height = 4,
       width = 10,
       bg = "white")

# Results 4 Social influence -----

(fig_4.1 <- time_correlation_s_all[id_focus != id_other & shift < 50 & shift > -50] |>
   ggplot() +
   geom_hline(data = time_correlation_s_all[id_focus != id_other & rho == peak_rho
   ][, .(peak_rho = mean(peak_rho)),
     by = list(id_focus, dominance_focus, baboon_focus)],
   aes(yintercept = peak_rho, color = dominance_focus), linewidth = 4, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), lty = "dashed") +
   geom_line(aes(shift, rho, color = dominance_other, group = id_other)) +
   geom_point(aes(peak_time, peak_rho, 
                  color = dominance_other),
              size = 2) +
   scale_color_viridis_c(name = "Dominance\nother",
                         option = "plasma") +
   facet_wrap(~baboon_focus, ncol = 12) +
   ylab(~paste("Cross-correlation \u03c6 / ", "\u03c6"[max])) +
   scale_x_continuous(name = "Shift (seconds)",
                      breaks = c(-25, 0, 25)) +
   guides(color = "none") +
   ggtitle("A)") 
 #background_grid()
)

(p4.2 <- ggplot(cc_net) +
    geom_segment(aes(x = x_focus, y = y_focus,
                     xend = x_other, yend = y_other,
                     color = peak_rho),
                 #linewidth = 1,
                 arrow = arrow(angle = 8, 
                               ends = "last", 
                               type = "closed")) +
    geom_point(data = nodes_position |>
                 merge(unique(cc_net[, c("id_focus", "out_degree")] |>
                                rename(id = id_focus))),
               aes(x, y, fill = dominance, 
                   size = out_degree),
               #size = 5, 
               shape = 21, stroke = 1) +
    geom_label(data = nodes_position,
               aes(x, y, label = baboon#fill = dominance
               ),
               nudge_y = 0.4) +
    geom_vline(aes(xintercept = 0.8),
               lty = "dashed", linewidth = 1) +
    geom_text(aes(x = 0.7, y = 0.1, label = "Females")) +
    geom_text(aes(x = 0.9, y = 0.1, label = "Males")) +
    scale_fill_viridis_c(option = "plasma",
                         name = "Dominance") +
    scale_color_viridis_c(name = "Peak\ncross-correlation") +
    scale_y_continuous(breaks = NULL,
                       name = NULL) +
    scale_x_continuous(breaks = NULL,
                       name = NULL) +
    guides(
      size = "none") +
    ggtitle("B)") +
    theme(axis.line = element_blank()))

(p4.3.1 <- ggplot(df, aes(x,y)) + 
    geom_line(linewidth = 1) + 
    geom_ribbon(aes(ymin = 0, ymax =  y, fill = quant)) + 
    scale_fill_manual(values = c("black", "white")) +
    guides(fill = "none") +
    geom_vline(aes(xintercept = coefficients(non_param_slope)[2]), 
               color = "red", linewidth = 2, lty = "dashed") +
    geom_text(aes(label = "*",
                  x = 0.6, y = 0.9),
              color = "red", size = 15) +
    theme(plot.background = element_rect(fill = "white")) +
    xlab("Slope") +
    ylab("Density"))

(p4.3 <- ggplot(cross_correlation_stats[peak_time != 0][, .(proportion_influence = unique(proportion_influence),
                                                            size = .N),
                                                        by = list(dominance_focus, id_focus, sex_focus)]) +
    geom_point(aes(dominance_focus, proportion_influence, 
                   shape = sex_focus),
               size = 5, stroke = 2) +
    geom_line(data = non_param_slope_predict,
              aes(dominance_focus, proportion_influence), 
              color = "red", linewidth = 2) +
    geom_text(aes(x = 0.5,
                  y = 0.1,
                  label = "Slope = 0.44 *"), color = "red", size = 7) +
    scale_shape_manual(name = "Sex",
                       values = c(1, 2)) +
    # geom_smooth(aes(dominance_focus, proportion_influence, 
    #                 group = sex_focus, weight = size), 
    #             se = F, color = "red", linewidth = 1,
    #             method = "glm",
    #             method.args = list(family = "binomial")) +
    ylab("Proportion influenced\n(positive peak cross-correlation)") +
    xlab("Dominance") +
    ggtitle("C)") +
    annotation_custom(ggplotGrob(p4.2.1), 
                      xmin = -0.05, xmax = 0.5, 
                      ymin = 0.55, ymax = 1.1))

(fig_4.2 <- ggarrange(p4.2, p4.3,
                      ncol = 2, nrow = 1))

(p4.4 <- ggplot(unique(mechanism_united[, c("dom_diff", "gran_median", "weight_dist")])) +
    geom_point(aes(dom_diff, gran_median, size = weight_dist)) +
    geom_line(data = mechanism_predictions_3,
              aes(dom_diff, gran_median),
              color = "red", linewidth = 1) +
    geom_ribbon(data = mechanism_predictions_3,
                aes(x = dom_diff, 
                    ymax = gran_median + se,
                    ymin = gran_median - se), 
                fill = "red", alpha = 0.3) +
    scale_color_viridis_c(option = "viridis",
                          name = "Dominance\ndifference",
                          direction = -1) +
    guides(size = "none") +
    xlab("Absolute difference in dominance") +
    ylab("Median inter-individual daytime distance") +
    ggtitle("D)") 
  #background_grid()
)

(p4.5 <- ggplot(unique(mechanism_united)) +
    geom_point(aes(log(proximity), log(peak_rho)#, color = peak_rho
    ),
    size = 5) +
    geom_line(data = mechanism_predictions,
              aes(log_proximity, log_predicted_rho),
              color = "red", linewidth = 1) +
    geom_ribbon(data = mechanism_predictions,
                aes(x = log_proximity,
                    ymax = (log_predicted_rho + log_predicted_rho_se),
                    ymin = (log_predicted_rho - log_predicted_rho_se)),
                fill = "red", alpha = 0.3) +
    ylab("Log peak cross-correlation") +
    xlab("Log daytime spatial proximity (1/m)") +
    ggtitle("E)") +
    #background_grid() +
    guides(color = "none") +
    scale_color_viridis_c(option = "viridis",
                          name = "Dominance\ndifference",
                          direction = -1))

(p4.6 <- ggplot(centrality_combined_predictions |>
                  pivot_longer(cols = !c("name", "dominance", "sex", "baboon"),
                               names_to = c("type", ".value"),
                               names_sep = "_")) +
    scale_fill_manual(values = c("#00204D", "#FFEA46", "white", "white")) +
    geom_ribbon(aes(x = dominance,
                    ymax = predictions + se,
                    ymin = predictions - se,
                    fill = interaction(type, sex), 
                    lty = sex,
                    group = interaction(type, sex)), 
                alpha = 0.3) +
    geom_point(aes(dominance, real, 
                   color = type, 
                   shape = sex),
               size = 5, stroke = 2) +
    geom_line(aes(dominance, predictions, 
                  color = type, lty = sex,
                  group = interaction(type, sex)),
              linewidth = 1) +
    scale_shape_manual(name = "Sex",
                       values = c(1, 2)) +
    scale_color_viridis_d(name = "",
                          labels = c("Influence (panel B)",
                                     "Proximity"),
                          option = "cividis") +
    scale_linetype_manual(values = c("solid", "blank")) +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    theme(legend.position = c(0.4, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white")) +
    ylab("Network centrality") +
    xlab("Dominance") +
    ggtitle("F)") 
  #background_grid()
)

(fig_4.3 <- ggarrange(p4.4, p4.5, p4.6,
                      ncol = 3, nrow = 1))

(fig_4 <- ggarrange(fig_4.1, fig_4.2, fig_4.3,
                    ncol = 1, nrow = 3))

ggsave(fig_4,
       filename = here("figures", "mc_results", "final", "fig_4.png"),
       height = 12,
       width = 13,
       bg = "white")

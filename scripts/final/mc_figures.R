# Results 1 ----

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
  ggtitle("A)") 

ggsave(plot = p1,
       filename = here("figures", "mc_results", "tests", "p1.png"),
       height = 7,
       width = 12,
       bg = "white")

(p2 <- ggplot(night_sleep) +
  geom_boxplot(aes(as.factor(id_night), freq_sleep, group = id_night)) +
  geom_line(aes(as.factor(id_night), freq_sleep, 
                color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  # theme(legend.position = c(0.5, 0.25),
  #       legend.background = element_rect(fill = "white")) +
  ylab(expression("Probability of rest  "(P[R]))) +
  scale_x_discrete(name = "Date",
                  breaks = unique(night_sleep$id_night)[seq(0, 27, by = 7) + 1],
                  labels = str_replace(unique(night_sleep$date_start_night), "2018-", "")[seq(0, 27, by = 7) + 1]) +
  # geom_step(data = weather_data[2:25, ],
  #           aes(date, precipitation_mm), 
  #           color = "blue") +
    ggtitle("B)") +
  background_grid())

ggsave(plot = p2,
       filename = here("figures", "mc_results", "manuscript_1", "p2.png"),
       height = 4.5,
       width = 7,
       bg = "white")

# Results 2 ----

(p2.1 <- predictions_dominance[avg_dominance_sleep < 0.1 & avg_dominance_sleep >= 0.0 & number_sleeping_excluding_focus == 11] |>
   ggplot() +
   geom_ribbon(aes(x = dominance, 
                   ymin = 1 - p_as - p_as_se, 
                   ymax = 1 - p_as + p_as_se), 
               fill = "#F8766D", color = NA, alpha = 0.3) +
   geom_ribbon(aes(x = dominance, 
                   ymin = 1 - p_sa - p_sa_se, 
                   ymax = 1 - p_sa + p_sa_se), 
               fill = "#00BFC4", color = NA, alpha = 0.3) +
   geom_ribbon(aes(x = dominance,
                   ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                   ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
               fill = "black", color = NA, alpha = 0.3) +
   geom_line(aes(dominance, 1 - p_as),
             linewidth = 1, color = "#F8766D") +
   geom_line(aes(dominance, 1 - p_sa),
             linewidth = 1, color = "#00BFC4") +
   geom_line(aes(dominance, p_rest),
             linewidth = 3) +
   annotate("text", x = 0.5, y = 0.8, label = expression("Rest quality "(p[R%->%R])), color = "#00BFC4") +
   annotate("text", x = 0.5, y = 0.7, label = expression("Continue activity "(p[A%->%A])), color = "#F8766D") +
   annotate("text", x = 0.5, y = 0.6, label = expression("Total nighttime rest "(p[R]))) +
   ylab("Probability") +
   #ggtitle("A)") +
   background_grid() +
   xlab("Dominance"))

ggsave(plot = p2.1,
       filename = here("figures", "mc_results", "manuscript_1", "p_dominance.png"),
       height = 3.5,
       width = 6,
       bg = "white") 

# Results 3 ----

(p3.1 <- ggplot(null_n_simultaneously_sleeping) +
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
   background_grid() +
   annotate("text", x = 3, y = 0.2, label = "NULL MODEL\nIndividual behaviour\nis independent", color = "red") +
   annotate("text", x = 3, y = 0.1, label = "DATA", color = "black") )

# ggsave(plot = p3.1,
#        filename = here("figures", "mc_results", "poster", "null_model.png"),
#        height = 3.5,
#        width = 4.5,
#        bg = "white")

(p3.2 <- ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5]) +
    geom_ribbon(aes(x = number_sleeping_excluding_focus, 
                    ymin = 1 - p_as - p_as_se, 
                    ymax = 1 - p_as + p_as_se), 
                fill = "#F8766D", color = NA, alpha = 0.3) +
    geom_ribbon(aes(x = number_sleeping_excluding_focus, 
                    ymin = 1 - p_sa - p_sa_se, 
                    ymax = 1 - p_sa + p_sa_se), 
                fill = "#00BFC4", color = NA, alpha = 0.3) +
    geom_ribbon(aes(x = number_sleeping_excluding_focus,
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
                fill = "black", color = NA, alpha = 0.3) +
    geom_line(aes(number_sleeping_excluding_focus, 
                  1 - p_sa), color = "#00BFC4") +
    geom_line(aes(number_sleeping_excluding_focus, 
                  1 - p_as), color = "#F8766D") +
    geom_line(aes(number_sleeping_excluding_focus, 
                  p_rest),
              linewidth = 1) +
    annotate("text", x = 3, y = 1, label = expression("Rest quality "(p[R%->%R])), color = "#00BFC4") +
    annotate("text", x = 3, y = 0.8, label = expression("Continue activity "(p[A%->%A])), color = "#F8766D") +
    annotate("text", x = 7.5, y = 0.5, label = expression("Total nighttime rest "(p[R]))) +
    ggtitle("B)") +
    background_grid() +
    xlab("Number individuals resting") +
    ylab("Probabaility"))

# ggsave(plot = p3.2,
#        filename = here("figures", "mc_results", "poster", "social_environment.png"),
#        height = 3.5,
#        width = 4.5,
#        bg = "white")

(fig_3.1 <- ggarrange(p3.1, p3.2,
                      ncol = 2, nrow = 1))

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

(p3.4 <- ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61]) +
    geom_ribbon(aes(x = number_sleeping_excluding_focus, 
                        ymin = 1 - p_as - p_as_se, 
                        ymax = 1 - p_as + p_as_se,
                    fill = dominance, group = dominance),
                color = NA, alpha = 0.3) +
    geom_line(aes(number_sleeping_excluding_focus, 
                  1 - p_as, color = dominance, group = dominance),
              linewidth = 1) +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "Dominance",
                         option = "plasma") +
    ggtitle("D)") +
    background_grid() +
    guides(color = "none", fill = "none") +
    xlab("Number individuals resting") +
    ylab(expression("Continue activity "(p[A%->%A]))))

(p3.5 <- ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61]) +
    geom_ribbon(aes(x = number_sleeping_excluding_focus, 
                    ymin = 1 - p_sa - p_sa_se, 
                    ymax = 1 - p_sa + p_sa_se,
                    fill = dominance, group = dominance),
                color = NA, alpha = 0.3) +
    geom_line(aes(number_sleeping_excluding_focus, 
                  1 - p_sa, color = dominance, group = dominance),
              linewidth = 1) +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "Dominance",
                         option = "plasma") +
    ggtitle("E)") +
    background_grid() +
    guides(color = "none", fill = "none") +
    xlab("Number individuals resting") +
    ylab(expression("Rest quality "(p[R%->%R]))))

(fig_3.2 <- ggarrange(p3.3, p3.4, p3.5,
                    ncol = 3, nrow = 1))

(p3.6 <- ggraph(net_null_model |>   
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
      aes(label = baboon),
      nudge_y = 0.2
    ) +
    ggtitle("F)") +
    theme(legend.position = 'none'))

# ggsave(plot = p3.3,
#        filename = here("figures", "mc_results", "poster", "network.png"),
#        height = 3.5,
#        width = 6.5)

(p3.7 <- ggplot(predictions_dominance[number_sleeping_excluding_focus == 9]) +
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
   background_grid() +
   ggtitle("G)") +
    ylab(expression("Rest probability  "(p[R]))) 
  )

(fig_3.3 <- ggarrange(p3.6, p3.7,
                      ncol = 2, nrow = 1))

# ggsave(plot = p3.4,
#        filename = here("figures", "mc_results", "poster", "pairwise_dominance.png"),
#        height = 3.5,
#        width = 6,
#        bg = "white")

(fig_3 <- ggarrange(fig_3.1, fig_3.2, fig_3.3,
                    ncol = 1, nrow = 3))

ggsave(plot = fig_3,
       filename = here("figures", "mc_results", "manuscript_1", "fig_3.png"),
       height = 9,
       width = 9,
       bg = "white") 

# Results 4 ----
(p4.1 <- ggplot(cc_net) +
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
              aes(x, y, label = baboon),
              nudge_y = 0.4) +
   geom_vline(aes(xintercept = 0.8),
              lty = "dashed", linewidth = 1) +
   geom_text(aes(x = 0.7, y = 0.1, label = "Females")) +
   geom_text(aes(x = 0.9, y = 0.1, label = "Males")) +
   scale_fill_viridis_c(option = "plasma",
                        name = "Dominance") +
   scale_color_viridis_c(name = "Peak\ninfluence") +
   scale_y_continuous(breaks = NULL,
                      name = NULL) +
   scale_x_continuous(breaks = NULL,
                      name = NULL) +
   guides(#color = "none",
          size = "none") +
   ggtitle("A)") +
   theme(axis.line = element_blank()))

(p4.2 <- time_correlation_s_all[id_focus != id_other 
                                  ][, .(peak_rho = unique(peak_rho),
                                        peak_time = unique(peak_time)),
  by = list(id_focus, id_other, dominance_focus)
][order(peak_rho)] |>
    ggplot() + # & shift <= 100 & shift >= -100
    geom_hline(aes(yintercept = 0), linewidth = 1, lty = "dashed") +
    geom_point(aes(
      #size = peak_prob,
      color = peak_rho,
      dominance_focus, peak_time, 
    ), size = 2) +
    geom_line(data = cross_correlation_model_predict,
              aes(dominance_focus, prediction), 
              color = "red", linewidth = 1) +
    geom_ribbon(data = cross_correlation_model_predict,
            aes(x = dominance_focus, 
                ymax = prediction + se,
                ymin = prediction - se), 
            fill = "red", alpha = 0.3) +
    scale_color_viridis_c(option = "viridis",
                          name = "Peak\ninfluence") +
    guides(size = "none", color = "none") +
    ggtitle("B)") +
    background_grid() +
    ylab("Time of peak influence (seconds)") +
    xlab("Dominance"))

(fig_4 <- ggarrange(p4.1, p4.2, 
                    widths = c(1.3, 1)))

ggsave(plot = fig_4,
       filename = here("figures", "mc_results", "manuscript_1", "fig_4.png"),
       height = 4,
       width = 11,
       bg = "white") 

# Results 5 ----
(p5.1 <- ggplot(unique(mechanism_united[, c("dom_diff", "gran_median", "weight_dist")])) +
   geom_point(aes(dom_diff, gran_median, size = weight_dist)) +
   geom_line(data = mechanism_predictions_2,
             aes(dom_diff, gran_median,),
             color = "red", linewidth = 1) +
   geom_ribbon(data = mechanism_predictions_2,
               aes(x = dom_diff, 
                   ymax = gran_median + se,
                   ymin = gran_median - se), 
               fill = "red", alpha = 0.3) +
   scale_color_viridis_c(option = "viridis",
                         name = "Dominance\ndifference",
                         direction = -1) +
   guides(size = "none") +
   ylab("Absolute difference in dominance") +
   xlab("Median inter-individual daytime distance") +
   ggtitle("A)") +
   background_grid())

(p5.2 <- ggplot(unique(mechanism_united)) +
  geom_point(aes(proximity, peak_rho),
             size = 2) +
   geom_line(data = mechanism_predictions,
             aes(proximity, predicted_rho),
             color = "red", linewidth = 1) +
   geom_ribbon(data = mechanism_predictions,
               aes(x = proximity, 
                   ymax = predicted_rho + predicted_rho_se,
                   ymin = predicted_rho - predicted_rho_se), 
               fill = "red", alpha = 0.3) +
   ylab("Peak influence") +
  xlab("Daytime spatial proximity (1/m)") +
   ggtitle("B)") +
   background_grid() +
   #scale_x_log10()+
  scale_color_viridis_c(option = "viridis",
                        name = "Dominance\ndifference",
                        direction = -1))

# ggsave(plot = p5.1,
#        filename = here("figures", "mc_results", "poster", "mechanism.png"),
#        height = 3.5,
#        width = 4.5,
#        bg = "white")

(fig_5 <- ggarrange(p5.1, p5.2))

ggsave(plot = fig_5,
       filename = here("figures", "mc_results", "manuscript_1", "fig_5.png"),
       height = 3.5,
       width = 8,
       bg = "white") 

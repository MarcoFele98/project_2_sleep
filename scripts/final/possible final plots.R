# final plots

# plot 1 ----
# 0     12    24   + 12
# 12    24    38   %% 24
# 12    0     12   -> subtract one day to bigger than 12
# 12(previous day)  0(current day)  12(current day)
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
  theme(axis.title = element_blank()) +
  scale_fill_gradientn(colors = parula()) +
  geom_text(aes(x = 0.13 * 24 * 60 * 60, y = ymd("2018-08-20"), label = "Sunset"), size = 5, color = "darkred", angle = -90) +
  geom_text(aes(x = 0.5 * 24 * 60 * 60, y = ymd("2018-08-24"), label = "Night"), size = 5, color = "darkblue") +
  geom_text(aes(x = 0.75 * 24 * 60 * 60, y = ymd("2018-08-20"), label = "Sunrise"), size = 5, color = "darkred", angle = -90) +
  guides(fill = "none")

# plot 2 ----
(p2 <- ggplot() +
  geom_point(data = night_sleep,
             aes(dominance, freq_sleep, shape = sex), 
             size = 5,
             color = "black") +
  geom_smooth(data = night_sleep,
              aes(dominance, freq_sleep, 
                  weight = tot, lty = sex), 
              method = "glm", linewidth = 3, 
              color = "black",
              method.args = list(family = "binomial")) +
  guides(linetype = "none") +
  geom_point(data = markov_data_m[transition %in% c("p1", "p4")],
             aes(dominance, probability, color = transition, shape = sex), size = 2) +
  geom_smooth(data = markov_data_m[transition %in% c("p1", "p4")],
              aes(dominance, probability, color = transition, weight = tot),
              method="glm", linewidth = 1, 
              method.args=list(family="binomial")) +
 # scale_color_manual(values = c("blue", "red")) +
   scale_color_manual(values = c("#00BFC4", "#F8766D")) +
   scale_shape_discrete(name = "") +
   theme(legend.position = c(0.1, 0.2),
         legend.background = element_rect(fill = "white")) +
  guides(color = "none") +
  geom_text(aes(x = 0.2, y = 0.7, label = "Overall sleep"), color = "black") +
  geom_text(aes(x = 0.2, y = 0.9, label = "Keep sleeping (p1)"), color = "#00BFC4") +
  geom_text(aes(x = 0.75, y = 0.8, label = "Remain awake (p2)"), color = "#F8766D") +
  ylab("Probability") +
  xlab("Dominance") +
  background_grid())

# plot 3 ----
(p3 <- ggplot(quourm_sleep_m[is_sleeping == "Sleeping"]) +
  geom_line(aes(number_sleeping, freq_sleep_by_id, color = dominance, group = id)) +
  #geom_line(aes(number_sleeping, freq_number_sleeping), linewidth = 3, color = "darkgreen", lty = "dashed") +
  ylab("Sleeping probability P(N|ID)") +
  xlab("Baboons sleeping (N)") +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
   guides(color = "none") +
  # theme(legend.position=c(0.2, 0.7),
  #       legend.background = element_rect(fill = "white")) +
  background_grid())

# plot 4 ----
(p4 <- ggplot(quourm_sleep_m) +
  geom_point(aes(number_sleeping, freq_id_by_sleep, color = dominance, shape = sex),
             size = 2, alpha = 0.5) +
  geom_smooth(aes(number_sleeping, freq_id_by_sleep, color = dominance, lty = sex,
                  group = id, weight = tot_sleeping),
              method="glm", linewidth = 2, se = F,
              method.args = list(family="binomial")) +
  ylab("Individual probability P(ID|N)") +
  xlab("Baboons sleeping (N)") +
  facet_wrap(~is_sleeping) +
  scale_color_viridis_c(option = "plasma") +
  guides(#color = "none",
         linetype = "none",
         shape = "none") +
   theme(legend.position = c(0.05, 0.8),
         legend.background = element_rect(fill = "white")) +
  background_grid())

# plot 5 ----
(p5 <- ggraph(net_null_model |>   
         activate(edges)  |>
         filter(ratio > quantile(ratio, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    aes(color = as.factor(ratio),
        width = ratio)) + 
   scale_edge_width_continuous(range = c(0.2, 3)) +
  scale_edge_color_manual(values = cubicl(n = 263)) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      fill = dominance,
      size = dominance), 
    stroke = 1,
    #fill = "black", 
    color = "black", shape = 21) +
  scale_size_continuous(range = c(1, 8)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_node_label(
    aes(label = baboon),
    nudge_y = 0.05
  ) +
  theme(legend.position = 'none'))

# plot 6 ----
(p6 <- ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = id, shape = sex),
             size = 2) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, 
                  linetype = sex,
                  group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c(option = "plasma") +
  ylab("Synchrony") +
   xlab("Dominance other individual")+
  #facet_wrap(~sex) +
  background_grid()+
   theme(legend.position = 'none'))


# combine ----
blanck <- ggplot() + theme_void()

pc <- ggarrange(ggarrange(p1, blanck, p2,
                          ncol = 3, nrow = 1, widths = c(0.55, 0.3, 0.4)),
                ggarrange(p3, p4, 
                          p5, p6,
                          ncol = 2, nrow = 2),
                heights = c(0.33, 0.66),
                ncol = 1, nrow = 2)

ggsave(filename = here("figures", "fig1.png"),
       plot = pc,
       height = 10*sqrt(2),
       width = 10,
       bg = "white")

pc2 <- ggarrange(ggarrange(p1, blanck,
                            ncol = 2, nrow = 1, 
                            widths = c(0.65, 0.35)),
                 ggarrange(p3, p2, 
                           p4, p6,
                          ncol = 2, nrow = 2),
                ncol = 1, nrow = 2,
                heights = c(0.33, 0.66))

ggsave(filename = here("figures", "fig2.png"),
       plot = pc2,
       height = 10*sqrt(2),
       width = 10,
       bg = "white")

pc3 <- ggarrange(ggarrange(p1, blanck,
                           ncol = 2, nrow = 1, 
                           widths = c(0.65, 0.35)),
                 ggarrange(p5, p2, 
                           ncol = 2, nrow = 1),
                 ggarrange(p3, p4, p6,
                           ncol = 3, nrow = 1,
                           widths = c(0.6, 1, 0.8)),
                 ncol = 1, nrow = 3)

ggsave(filename = here("figures", "fig3.png"),
       plot = pc3,
       height = 10*1.4142,
       width = 10,
       bg = "white")



# Create correlation data ----
time_correlation <- data.table()
for(id1 in ids) {
  for(id2 in ids) {
    print(id1)
    print(id2)
    for(shift in 1:250) {
      print(shift)
        time_correlation <- rbind(time_correlation, 
                                  (head(combined_night_m[, c(id1, "is_night"), with = F], -shift # take out from the end
                                        )[, ":="(syncro = get(id1) + tail(combined_night_m[[id2]], -shift)), # tail takes out form the beginning
                                  ][, -id1, with = F] |>
                                    table() |>
                                    as.data.table())[, ":="(shift = shift,
                                                            id_before = id1,
                                                            id_after = id2)])
    }
  }
}

# save(time_correlation,
#      file = here("data/time-correlation_t0.04.RData"),
#      header = T)

#load(here("data", "time-correlation_t0.05.RData"))

# time_correlation_corrected <- time_correlation |> # first time I inverted who before and who after .....
#   rename(id_after = id_before, id_before = id_after)

# Modify dataset ----
time_correlation_m <- copy(time_correlation)[, ":="(id_before = ifelse(id_before == "Sheb", "Shebeleza", id_before),
                                                    id_after = ifelse(id_after == "Sheb", "Shebeleza", id_after))
                                             ][, ":="(tot = sum(N)),
                                               by = list(id_before, id_after, is_night, shift)
                                               ][, ":="(prob = N/tot)] |> 
  merge(dominance_id[, c("id", "dominance", "sex")] |>
          rename(dominance_before = dominance,
                 sex_before = sex),
        by.x = "id_before",
        by.y = "id") |>
  merge(dominance_id[, c("id", "dominance", "sex")] |>
          rename(dominance_after = dominance,
                 sex_after = sex),
        by.x = "id_after",
        by.y = "id") |>
  mutate(id_after = factor(id_after, levels = idsRanked),
         id_before = factor(id_before, levels = idsRanked))

ggplot(time_correlation_m[syncro == 2 & is_night == T & id_before != id_after]) +
  geom_line(aes(shift, prob, color = id_after, group = id_after)) +
  facet_wrap(~id_before) +
  background_grid()

time_correlation_l <- rbind(copy(time_correlation_m |> rename(id_focus = id_after, # with this id focus is predicted until "shift" time steps before night ends 
                                                              id_other = id_before,
                                                              sex_focus = sex_after,
                                                              sex_other = sex_before,
                                                              dominance_focus = dominance_after,
                                                              dominance_other = dominance_before))[, ":="(shift = -shift)],
                            time_correlation_m |> rename(id_focus = id_before, # with this id focus predicts until "shift" time steps after night ends
                                                         id_other = id_after,
                                                         sex_focus = sex_before,
                                                         sex_other = sex_after,
                                                         dominance_focus = dominance_before,
                                                         dominance_other = dominance_after))[, ":="(id_focus = factor(id_focus, levels = idsRanked),
                                                                                                    id_other = factor(id_other, levels = idsRanked))
                                                                                             ][, ":="(avg_prob = mean(prob)),
                                                                                               by = list(id_focus, id_other, is_night, syncro)] |>
  merge(null_model_components[, c("id", "prob_sleep")] |>
          rename(id_focus = id, prob_sleep_focus = prob_sleep),
        by = "id_focus") |>
  merge(null_model_components[, c("id", "prob_sleep")] |>
          rename(id_other = id, prob_sleep_other = prob_sleep),
        by = "id_other") |>
  mutate(ratio = prob / avg_prob,
         null_sleep_prob = prob_sleep_other * prob_sleep_focus,
         null_awake_prob = (1 - prob_sleep_other) * (1 - prob_sleep_focus),
         null_asyncro_prob = ((1 - prob_sleep_other) * (prob_sleep_focus) + (prob_sleep_other) * (1 - prob_sleep_focus)),
         null_syncro_prob = prob_sleep_other * prob_sleep_focus + (1 - prob_sleep_other) * (1 - prob_sleep_focus),
         null_sleep_sequence = tot * null_sleep_prob,
         null_awake_sequence = tot * null_awake_prob,
         null_asyncro_sequence = tot * null_asyncro_prob) |>
  group_by(id_focus, id_other, is_night, syncro) |>
  mutate(peak_prob = ifelse(syncro != 1, max(prob), min(prob)), # go up for synchronization, go down for unsybchnroization
         shift_peak_prob = ifelse(id_focus != id_other, unique(shift[prob == peak_prob]), NA),
         peak_ratio = ifelse(syncro != 2, max(ratio), min(ratio))) |> 
  merge(dominance_id[, c("id", "baboon")] |> rename(id_focus = id, baboon_focus = baboon)) |>
  merge(dominance_id[, c("id", "baboon")] |> rename(id_other = id, baboon_other = baboon),
        by = "id_other") |>
  as.data.table()

ggplot(time_correlation_l[id_focus != id_other & syncro == 2 & is_night == F]) +
  geom_line(aes(shift, prob, color = id_other, group = id_other)) +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_l[syncro == 2 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
    geom_vline(aes(xintercept = 0), linewidth = 3, lty = "dashed") +
    geom_point(aes(shift_peak_prob, peak_prob, color = baboon_other), size = 5) +
    geom_line(aes(shift, prob, 
                  color = baboon_other, group = interaction(is_night, id_other)), linewidth = 1) +
    scale_color_viridis_d(option = "turbo",
                          name = "") +
    #guides(color = "none") +
    xlab("Time series shift") +
  #xlim(c(-50, 50)) +
    facet_wrap(~baboon_focus) +
    background_grid()

ggplot(time_correlation_l[syncro == 2 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_vline(aes(xintercept = 0), linewidth = 3, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_ratio, color = baboon_other), size = 5) +
  geom_line(aes(shift, ratio, 
                color = baboon_other, group = interaction(is_night, id_other)), linewidth = 1) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  #guides(color = "none") +
  xlab("Time series shift") +
  facet_wrap(~baboon_focus) +
  xlim(c(-50, 50)) +
  background_grid()

time_correlation_synchro <- time_correlation_l[is_night == T & syncro %in% c(0, 2)
                                               ][, .(syncro_prob = sum(prob)),
                                                   by =  c("id_other", "id_focus", "shift", "dominance_other", "dominance_focus", "baboon_focus", "baboon_other",  "null_syncro_prob")
                                                 ][, ":="(ratio = syncro_prob / null_syncro_prob)
                                                   ][, ":="(normalized_prob = syncro_prob / mean(syncro_prob)),
                                                     by = c("id_other", "id_focus")
                                                     ][, ":="(peak_syncro = max(syncro_prob),
                                                              peak_ratio = max(ratio),
                                                              peak_normalized_prob = max(normalized_prob)),
                                                     by = c("id_other", "id_focus")
                                                     ][, ":="(peak_shift = ifelse(id_focus != id_other, 
                                                                                  unique(shift[ratio == peak_ratio]), 
                                                                                  -1)),
                                                       by = c("id_other", "id_focus")]


ggplot(time_correlation_synchro[id_focus != id_other & shift <= 50 & shift >= -50]) + # & shift <= 100 & shift >= -100
  geom_vline(aes(xintercept = 0), linewidth = 3, lty = "dashed") +
  geom_point(aes(peak_shift, peak_ratio,
                 color = id_other),
             size = 5) +
  geom_line(aes(shift, ratio, 
                color = id_other, 
                group = id_other), 
            linewidth = 1) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  #guides(color = "none") +
  xlab("Time series shift") +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_synchro[id_focus != id_other & shift <= 50 & shift >= -50]) + # & shift <= 100 & shift >= -100
  geom_vline(aes(xintercept = 0), linewidth = 3, lty = "dashed") +
  geom_point(aes(peak_shift, peak_normalized_prob,
                 color = id_other),
             size = 5) +
  geom_line(aes(shift, normalized_prob, 
                color = id_other, 
                group = id_other), 
            linewidth = 1) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  #guides(color = "none") +
  xlab("Time series shift") +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_synchro[id_focus != id_other & shift <= 50 & shift >= -50]) + # & shift <= 100 & shift >= -100
  geom_vline(aes(xintercept = 0), linewidth = 3, lty = "dashed") +
  geom_point(aes(peak_shift, syncro_prob,
                 color = id_other),
             size = 5) +
  geom_line(aes(shift, peak_syncro, 
                color = id_other, 
                group = id_other), 
            linewidth = 1) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  #guides(color = "none") +
  xlab("Time series shift") +
  facet_wrap(~id_focus) +
  background_grid()

data_relevance <- unique(time_correlation_l[, c("id_focus", "id_other", "is_night", 
                                                "syncro", "dominance_other", "dominance_focus", 
                                                "sex_focus", "sex_other", "peak_prob", 
                                                "shift_peak_prob", "peak_ratio", "baboon_focus",
                                                "baboon_other")])[, ":="(relevance = mean(shift_peak_prob * peak_prob, na.rm = T),
                                                                     shift_relevance = mean(shift_peak_prob, na.rm = T),
                                                                     peak_relevance = mean(shift_peak_prob, na.rm = T),
                                                                     ratio_relevance = mean(shift_peak_prob * peak_ratio, na.rm = T)), 
                                                              by = list(id_focus, is_night, syncro)] 

ggplot(data_relevance[is_night == T & syncro == 0]) +
    geom_hline(aes(yintercept = 0), color = "grey", lty = "dashed", linewidth = 1) +
    geom_point(aes(dominance_focus, relevance, color = sex_focus),
               size = 5) +
    geom_smooth(aes(dominance_focus, relevance, color = sex_focus),
                method = "lm", linewidth = 3 , se = F) +
    scale_color_viridis_d(name = "",
                          labels = c("Female","Male")) +
    ylab("Influence") +
    xlab("Dominance") +
    guides(color = "none") +
    background_grid()

ggplot(data_relevance[is_night == T & syncro == 2]) +
  geom_hline(aes(yintercept = 0), color = "grey", lty = "dashed", linewidth = 1) +
  geom_point(aes(dominance_focus, relevance, color = sex_focus),
             size = 5) +
  geom_smooth(aes(dominance_focus, relevance, color = sex_focus),
              method = "lm", linewidth = 3 , se = F) +
  scale_color_viridis_d(name = "",
                        labels = c("Female","Male")) +
  ylab("Influence") +
  xlab("Dominance") +
  guides(color = "none") +
  background_grid()


ggplot(data_relevance[is_night == T & syncro == 0][, ":="(sex = ifelse(sex_type_focus == "m", "m", "f"))
][, ":="(sex = ifelse(is.na(sex), "f", sex))]) +
  geom_point(aes(dominance_focus, relevance, color = sex)) +
  geom_smooth(aes(dominance_focus, relevance, color = sex),
              method = "lm") +
  #geom_label(aes(dominance_focus, relevance, label = id_focus)) +
  background_grid()

ggplot(data_relevance[is_night == T & syncro == 0][, ":="(sex = ifelse(sex_type_focus == "m", "m", "f"))
][, ":="(sex = ifelse(is.na(sex), "f", sex))]) +
  geom_point(aes(dominance_focus, ratio_relevance, color = sex)) +
  geom_smooth(aes(dominance_focus, ratio_relevance, color = sex),
              method = "lm") +
  #geom_label(aes(dominance_focus, relevance, label = id_focus)) +
  background_grid()

ggplot(data_relevance) +
  geom_point(aes(dominance_focus, relevance, color = syncro)) +
  geom_line(aes(dominance_focus, relevance, color = syncro)) +
  facet_wrap(~is_night) +
  background_grid()

ggplot(data_relevance[is_night == T][, ":="(sex = ifelse(sex_type_focus == "m", "m", "f"))
][, ":="(sex = ifelse(is.na(sex), "f", sex))]) +
  geom_point(aes(dominance_focus, relevance, color = sex)) +
  geom_smooth(aes(dominance_focus, relevance, color = sex),
              method = "lm") +
  #geom_label(aes(dominance_focus, relevance, label = id_focus)) +
  facet_wrap(~syncro) +
  background_grid()

ggplot(data_relevance[is_night == T]) +
  geom_point(aes(dominance_focus, shift_relevance, color = syncro)) +
  facet_wrap(~syncro) +
  background_grid()

ggplot(data_relevance[is_night == T]) +
  geom_point(aes(dominance_focus, peak_relevance, color = sex_type_focus), size = 2) +
  facet_wrap(~syncro) +
  background_grid()

ggplot(data_relevance[is_night == T]) +
  geom_boxplot(aes(sex_type_focus, shift_relevance, color = syncro)) +
  geom_point(aes(sex_type_focus, shift_relevance)) +
  facet_wrap(~syncro) +
  background_grid()

ggplot(data_relevance[is_night == T]) +
  geom_boxplot(aes(sex_type_focus, relevance, color = syncro)) +
  geom_point(aes(sex_type_focus, relevance)) +
  facet_wrap(~syncro) +
  background_grid()

# Plot ----

# compare with null model (does not work very well)
ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 250 & shift >= -250 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prob, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_hline(aes(yintercept = null_awake_prob, color = id_other)) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_prob, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  #scale_y_log10() +
  background_grid()

# Both awake
ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, ratio, 
                color = dominance_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_ratio, color = dominance_other), size = 4) +
  facet_wrap(~id_focus) +
  scale_color_viridis_c(option = "viridis") +
  background_grid()

ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, ratio, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_ratio, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  background_grid()

ggsave(here("figures", "test", "lagged-correlation.png"),
       height = 20,
       width = 20,
       bg = "white")

ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prob, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_prob, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_l[syncro == 0 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prob, 
                color = dominance_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_prob, color = dominance_other), size = 4) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~id_focus) +
  background_grid()

# Asynchronous 
ggplot(time_correlation_l[syncro == 1 & id_focus != id_other & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prob, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_prob, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  background_grid()

# Synchronous 
ggplot(time_correlation_l[syncro == 1 & id_focus != id_other & shift <= 50 & shift >= -50 & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, 1 - prob, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, 1 - peak_prob, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  background_grid()

# both sleeping
ggplot(time_correlation_l[syncro == 2 & id_focus != id_other & is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prob, 
                color = id_other, group = interaction(is_night, id_other)), linewidth = 1) +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  geom_point(aes(shift_peak_prob, peak_prob, color = id_other), size = 4) +
  facet_wrap(~id_focus) +
  background_grid()


# create network ----
time_correlation_net_data <- unique(time_correlation_l[id_focus != id_other, c("id_focus", "id_other", "peak_ratio", "shift_peak_prob", "is_night", "syncro", "dominance_other", "dominance_focus")]) 

time_correlation_net <- as_tbl_graph(time_correlation_net_data[shift_peak_prob > 0, c("id_focus", "id_other", "peak_ratio", "shift_peak_prob", "is_night", "syncro")] |> 
  rename(from = id_focus, to = id_other), 
  directed = T) |>
  activate(nodes) |>
  mutate(node_number = row_number()) |>
  left_join(dominance_id |> rename(name = id), by = "name")

ggraph(time_correlation_net |> 
         activate(nodes) |>
         mutate(dominance_n = normalise(dominance, to = c(3, 16))) |>  
         activate(edges)  |>
         filter(syncro == 0,
                peak_ratio > 1.033),#1.025
       layout = "kk") +#"kk" "stress"
  geom_edge_link(aes(end_cap = circle(node2.dominance + 10, "pt"),
                     color = as.factor(shift_peak_prob),
                     width = shift_peak_prob
  ),
  arrow = arrow(length = unit(10, 'mm'),
                type = "closed",
                ends = "last",
                angle = 10)) + 
  scale_edge_width_continuous(range = c(0.2, 3)) +
  scale_edge_color_manual(values = viridis(n = 15)) + # 263
  geom_node_point(aes(#color = dominance,
                      size = I(dominance_n))) +
  scale_color_viridis_c(option = "plasma") +
  geom_node_label(
    aes(label = name),
    nudge_y = 0.05) +
  theme(legend.position = 'none')



ggsave(here("figures", "test", "lagged_correlation_net.png"),
       height = 5,
       width = 5,
       bg = "white")


# Figures ----
ggplot(time_correlation_synchro[id_focus != id_other & 
                                  shift <= 50 & shift >= -50] |>
         merge(dominance_id[, c("id", "baboon")] |>
                 rename(baboon_focus = baboon,
                        id_focus = id)) |>
         left_join(dominance_id[, c("id", "baboon")] |>
                     rename(baboon_other = baboon,
                            id_other = id))) + 
  geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
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
  geom_line(aes(shift, log(syncro_prob), 
                color = baboon_other, 
                group = baboon_other), 
            linewidth = 1) +
  geom_point(aes(peak_shift, log(peak_syncro),
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
                                  shift <= 50 & shift >= -50] |>
         merge(dominance_id[, c("id", "baboon")] |>
                 rename(baboon_focus = baboon,
                        id_focus = id)) |>
         left_join(dominance_id[, c("id", "baboon")] |>
                     rename(baboon_other = baboon,
                            id_other = id))) + 
  geom_vline(aes(xintercept = 0), linewidth = 1, lty = "dashed") +
  geom_line(aes(shift, normalized_prob, 
                color = baboon_other, 
                group = baboon_other), 
            linewidth = 1) +
  geom_point(aes(peak_shift, peak_normalized_prob,
                 color = baboon_other),
             size = 5) +
  scale_color_viridis_d(option = "turbo",
                        name = "Other\nindividual") +
  facet_wrap(~baboon_focus) +
  ylab("Influence") +
  xlab("Time shift (s)") +
  background_grid()






# problem null model
                             
test <- data.table()
for(shift in 1:100000) {
  print(shift)
  test <- rbind(test, (head(combined_night_m[, c("Kangela", "is_night"), with = F], -shift)[, ":="(syncro = Kangela + tail(combined_night_m[["Hanson"]], -shift)),
                            ][, -c("Kangela"), with = F] |>
                              table() |>
                              as.data.table())[, ":="(shift = shift,
                                                      id_after = "Hanson",
                                                      id_before = "Kangela")])
}
save(test,
     file = here("data/null_model_problem.RData"),
     header = T)

test2 <- test[is_night == T][, ":="(tot = sum(N)), by = list(is_night, shift)][, ":="(prop = N / tot)]

ggplot(test2) +
  geom_line(aes(shift, prop, color = syncro))

ggplot(test2[syncro == 0]) +
  geom_line(aes(shift, prop)) +
  geom_hline(aes(yintercept = (1 - 0.6855319) * (1 - 0.6827189)))

ggplot(test2[syncro == 2]) +
  geom_line(aes(shift, prop)) +
  geom_hline(aes(yintercept = 0.6855319 * 0.6827189))



ggplot(test2[is_night == T]) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prop, 
                color = syncro, group = interaction(is_night, syncro)), linewidth = 1) +
  geom_hline(aes(yintercept = (1 - null_model_components[id == "Kangela"]$prob_sleep) * (1 - null_model_components[id == "Hanson"]$prob_sleep)), color = 'red') +
  geom_hline(aes(yintercept = null_model_components[id == "Kangela"]$prob_sleep * null_model_components[id == "Hanson"]$prob_sleep), color = 'blue') +
  geom_hline(aes(yintercept = (1 - null_model_components[id == "Kangela"]$prob_sleep) * null_model_components[id == "Hanson"]$prob_sleep + 
                   null_model_components[id == "Kangela"]$prob_sleep * (1 - null_model_components[id == "Hanson"]$prob_sleep)), color = 'green') +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  #geom_point(aes(shift_peak_prob, peak_prob), size = 4) +
  #scale_y_log10() +
  background_grid()

test_combined_night <- combined_night_m[is_night == T]

test <- data.table()
for(night in unique(test_combined_night$id_nightday)) {
  print(night)
  for(shift in 1:18000) {
   #print(shift)
    test <- rbind(test, (head(test_combined_night[id_nightday == night, c("Kangela"), with = F], -shift)[, ":="(syncro = Kangela + tail(combined_night_m[id_nightday == night][["Hanson"]], -shift)),
    ][, -c("Kangela"), with = F] |>
      table() |>
      as.data.table())[, ":="(shift = shift,
                              night = night,
                              id_after = "Hanson",
                              id_before = "Kangela")])
  }
}

test2 <- copy(test)[, ":="(tot = sum(N)), by = list(night, shift)][, ":="(prop = N / tot)][, ":="(mean = mean(prop)), by = list(shift, syncro)]

ggplot(test2) + # & shift <= 100 & shift >= -100
  geom_line(aes(shift, prop, 
                color = syncro, group = interaction(syncro, night)), alpha = 0.3) +
  geom_line(aes(shift, mean, 
                color = syncro, group = interaction(syncro, night)), linewidth = 2) +
  geom_hline(aes(yintercept = (1 - null_model_components[id == "Kangela"]$prob_sleep) * (1 - null_model_components[id == "Hanson"]$prob_sleep)), color = 'red') +
  geom_hline(aes(yintercept = null_model_components[id == "Kangela"]$prob_sleep * null_model_components[id == "Hanson"]$prob_sleep), color = 'blue') +
  geom_hline(aes(yintercept = (1 - null_model_components[id == "Kangela"]$prob_sleep) * null_model_components[id == "Hanson"]$prob_sleep + 
                   null_model_components[id == "Kangela"]$prob_sleep * (1 - null_model_components[id == "Hanson"]$prob_sleep)), color = 'green') +
  geom_vline(aes(xintercept = 0), linewidth = 2, lty = "dashed") +
  #geom_point(aes(shift_peak_prob, peak_prob), size = 4) +
  #scale_y_log10() +
  background_grid()

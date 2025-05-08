# Load and visualize distance data ----
files_list <- list.files(here("data", "baboons_030122_neighbs"))[!grepl(pattern = "_v2", list.files(here("data", "baboons_030122_neighbs")))]

data_for_day <- as.data.table(read.csv(here("data", "baboons_030122_neighbs", files_list[1])))[, c("date", "time", "id", "dist", "neighb")] |>
  merge(dominance_id_columns) |>
  merge(dominance_id_columns |>
          rename(neighb = id,
                 baboon_neighb = baboon),
        by = "neighb")

data_distances <- data_for_day[, ":="(dist = round(dist))
                               ][, .(count = .N),
                                 by = list(date, id, neighb, baboon, baboon_neighb, dist)
                                 ][, ":="(mean_dist = sum(dist * count / sum(count)),
                                          median_dist = weightedMedian(dist, count)),
                                   by = list(id, neighb)]
ggplot(data_for_day) +
  geom_histogram(aes(dist)) +
  geom_vline(data = data_distances,
             aes(xintercept = mean_dist),
             col = "red") +
  geom_vline(data = data_distances,
             aes(xintercept = median_dist),
             col = "blue") +
  # geom_label(data = data_distances,
  #            aes(x = mean_dist, y = 11000, label = mean_dist), 
  #            color = "red") +
  # geom_label(data = data_distances,
  #            aes(x = median_dist, y = 6000, label = median_dist), 
  #            color = "blue") +
  facet_grid(cols = vars(baboon),
             rows = vars(baboon_neighb)) +
  ggtitle(str_replace(files_list[1], ".csv", "") |>
            str_replace("pairwise_all_", "")) +
  background_grid()

ggsave(here("figures", "submission", "supp", str_replace(files_list[1], ".csv", ".png")),
       height = 10,
       width = 15,
       bg = "white")

for(file_name in files_list[-1]) { # files_list[-1]
  print(file_name)
  
  data_for_day <- as.data.table(read.csv(here("data", "baboons_030122_neighbs", file_name)))[, c("date", "time", "id", "dist", "neighb")]|>
    merge(dominance_id_columns) |>
    merge(dominance_id_columns |>
            rename(neighb = id,
                   baboon_neighb = baboon),
          by = "neighb")
  
  data_for_day_s <- data_for_day[, ":="(dist = round(dist))
  ][, .(count = .N),
    by = list(date, id, baboon, neighb, baboon_neighb, dist)
  ][, ":="(mean_dist = sum(dist * count / sum(count)),
           median_dist = weightedMedian(dist, count)),
    by = list(id, neighb)]
  
  # For plotting
  # p1 <- ggplot(data_for_day) +
  #   geom_histogram(aes(dist)) +
  #   geom_vline(data = data_for_day_s,
  #              aes(xintercept = mean_dist),
  #              col = "red") +
  #   geom_vline(data = data_for_day_s,
  #              aes(xintercept = median_dist),
  #              col = "blue") +
  #   # geom_label(data = data_for_day_s,
  #   #            aes(x = mean_dist, y = 20000, label = mean_dist),
  #   #            color = "red") +
  #   # geom_label(data = data_for_day_s,
  #   #            aes(x = median_dist, y = 10000, label = median_dist),
  #   #            color = "blue") +
  #   facet_grid(cols = vars(baboon),
  #              rows = vars(baboon_neighb)) +
  #   ggtitle(str_replace(file_name, ".csv", "") |>
  #             str_replace("pairwise_all_", "")) +
  #   background_grid()
  # 
  # ggsave(filename = here("figures", "submission", "supp", str_replace(file_name, ".csv", ".png")),
  #        p1,
  #        height = 10,
  #        width = 15,
  #        bg = "white")
  
  data_distances <- rbind(data_distances, 
                          data_for_day_s)
}

# Add distance data to covariate data, keeping in mind that there is less individuals that have both distance and accelerometry data (for this reason redo all the markov chain covariates stuff with fewer individuals) ----
data_distances_m <- copy(data_distances)[, ":="(id = case_when(id == "Trin" ~ "Trinity",
                                                               id == "Kan" ~ "Kangela",
                                                               T ~ id),
                                                neighb = case_when(neighb == "Trin" ~ "Trinity",
                                                                   neighb == "Kan" ~ "Kangela",
                                                                   T ~ neighb))]

data_distances_s <- data_distances_m[, .(gran_median = weightedMedian(dist, count),
                                         weight = sum(count)),
                                     by = list(id, neighb, baboon, baboon_neighb)
                                     ][, ":="(rank_gran_median = rank(gran_median, 
                                                                      ties.method = "first")),
                                              by = id]

ggplot(data_distances_s) +
  geom_point(aes(neighb, gran_median)) +
  facet_wrap(~id) +
  background_grid()


# first exclude the individuals which are not present in the data of the nearest neighbour distances
{
  unique(data_distances_m[id %in% ids & neighb %in% ids]$id)
  
  ids_distances_ranked <- c("Azul",
    "Lola",
    "Trinity",
    "Nelly",
    "NikNak", 
    "Kym",
    "Luna",
    "Hanson", 
    "Sheb",
    "Kangela")

  mechanism_data_prep <- (combined_m[, c(ids_distances_ranked, "id_night", "dateTime"), with = F
                                      # next is to find consecutive moments to calculate transitions
  ][, ":="(diff = c(NA, diff(dateTime)))
  ][, ":="(unit = rleid(diff))
  ][, ":="(is_transition = ifelse(diff == 1, T, F))
  ][is_transition == T
  ][, -c("is_transition", "diff")
    # create group sleep configuration
  ][, ":="(group_configuration = apply(.SD[, ids_distances_ranked, with = F] * 1, 
                                       1, 
                                       paste, collapse = ""))
  ][, ":="(date = date(dateTime))] |>
    left_join(data_astronomical[, c("date", "night_time", "nightEnd_time")]) |>
    as.data.table())[, ":="(period = case_when(as_hms(dateTime) <= nightEnd_time | as_hms(dateTime) >= night_time ~ "night_time",
                                               as_hms(dateTime) > nightEnd_time & as_hms(dateTime) < as_hms("12:00:00") ~ "wakey_time",
                                               as_hms(dateTime) < night_time &  as_hms(dateTime) > as_hms("12:00:00") ~ "bed_time"))
    ][, -c("date", "night_time", "nightEnd_time")]
  
  id <- "Azul"
  mechanism_data <- rename(mechanism_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
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
  ][order(id_night, state, period, group_configuration, transition)]
  
  for(id in ids_distances_ranked[-1]) {
    print(id)
    mechanism_data <- rbind(mechanism_data,
                     rename(mechanism_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
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

# find a variable that is similar to average dominance sleeping (i.e. mean of nearest neigbour gran mean distance or rank gran mean distance to start)

ids_distances_ranked_correct_names <- c("Azul",
                          "Lola",
                          "Trinity",
                          "Nelly",
                          "NikNak", 
                          "Kym",
                          "Luna",
                          "Hanson", 
                          "Shebeleza",
                          "Kangela")

data_distances_f <- data_distances_s[id %in% ids_distances_ranked & neighb %in% ids_distances_ranked
                                           ][, ":="(id = ifelse(id == "Sheb", "Shebeleza", id),
                                                neighb = ifelse(neighb == "Sheb", "Shebeleza", neighb))
                                       ][, ":="(id = factor(id, levels = ids_distances_ranked_correct_names),
                                                neighb = factor(neighb, levels = ids_distances_ranked_correct_names))
                                       ][order(id, neighb)] |>
  rename(id_focus = id)

# Add covariates (both dominance and distances) ----
mechanism_data_covariates <- (mechanism_data[, ":="(id = ifelse(id == "Sheb", "Shebeleza", id))
][, ":="(number_sleeping = sapply(strsplit(group_configuration, ""), # find how many are sleeping
                                  function(x) { 
                                    sum(as.numeric(x))
                                  }))
] |>
  left_join(dominance_id) |>
  left_join(data_astronomical[, c("date", #"night_duration",
                                  "phase")] |> 
              rename(date_start_night = date)) |>
  left_join(weather_data |> 
              rename(date_start_night = date)) |>
  left_join(night_sleep[, c("id", "id_night", "sleep", "sleep.z")
  ][, ":="(id_night = id_night - 2)] |>
    rename(previous_sleep = sleep,
           previous_sleep.z = sleep.z)))[, ":="(sum_dominance_sleep = sapply(strsplit(group_configuration, ""), # find dominance of the sleeping individuals
                                                                             function(x) { 
                                                                               dominance_id[id %in% ids_distances_ranked_correct_names][order(id)]$dominance[!!as.numeric(x)] |> sum()
                                                                             }),
                                                sum_distance_sleep_excluding_focus = sapply(id, FUN = function(x) {
                                                  data_distances_f[id_focus == x]$gran_median[!!as.numeric(strsplit(group_configuration, "")[[1]])[-which(x %in% ids_distances_ranked_correct_names)]] |> sum()
                                                  }),
                                                sum_distance_ranked_sleep_excluding_focus = sapply(id, FUN = function(x) {
                                                  data_distances_f[id_focus == id]$rank_gran_median[!!as.numeric(strsplit(group_configuration, "")[[1]])[-which(id %in% ids_distances_ranked_correct_names)]] |> sum()
                                                  })),
                                         by = list(id, id_night, period, state, group_configuration, transition)
           ][, ":="(number_sleeping_excluding_focus = ifelse(state == T, 
                                                             number_sleeping - 1, 
                                                             number_sleeping),
                    sum_dominance_sleep_excluding_focus = ifelse(state == T, 
                                                                 sum_dominance_sleep - dominance, 
                                                                 sum_dominance_sleep),
                    maximum_dominance_sleep = sum(dominance_id$dominance) - dominance)
           ][, ":="(proportion_dominance_sleep_excluding_focus = sum_dominance_sleep_excluding_focus / maximum_dominance_sleep,
                    avg_dominance_sleep = ifelse(number_sleeping_excluding_focus != 0,
                                                 sum_dominance_sleep_excluding_focus / number_sleeping_excluding_focus,
                                                 0),
                    avg_dist_sleep =  ifelse(number_sleeping_excluding_focus != 0,
                                             sum_distance_sleep_excluding_focus / number_sleeping_excluding_focus,
                                             0),
                    avg_dist_rank_sleep =  ifelse(number_sleeping_excluding_focus != 0,
                                                  sum_distance_ranked_sleep_excluding_focus / number_sleeping_excluding_focus,
                                             0),
                    period = factor(period, levels = possible_periods))
           ][, ":="(id_row = rleid(id, group_configuration))
           ][order(id_night, state, period, group_configuration, transition)]

# for statistical model in wide format
mechanism_data_covariates_active <- as.data.table(mechanism_data_covariates[state == F] |>
                                             pivot_wider(id_cols = -c("freq_transition"),
                                                         names_from = "transition",
                                                         values_from = "tot_transition",
                                                         values_fill = 0))[, ":="(freq_as = p_as / tot_obs,
                                                                                  freq_aa = p_aa / tot_obs)]

mechanism_data_covariates_rest <- as.data.table(mechanism_data_covariates[state == T] |>
                                           pivot_wider(id_cols = -c("freq_transition"),
                                                       names_from = "transition",
                                                       values_from = "tot_transition",
                                                       values_fill = 0))[, ":="(freq_sa = p_sa / tot_obs,
                                                                                freq_ss = p_ss / tot_obs)]

# look at correlation between covariates (number sleeping and dominance sleeping)
ggplot(mechanism_data_covariates_rest) +
  geom_point(aes(number_sleeping_excluding_focus, avg_dist_rank_sleep)) + # this has to be similar to avg_dominance_sleep, which it is enough !!!!
  ggtitle("rest") +
  facet_wrap(~id) + 
  background_grid()

ggplot(mechanism_data_covariates_rest) +
  geom_point(aes(number_sleeping_excluding_focus, avg_dist_rank_sleep)) +
  ggtitle("active") +
  facet_wrap(~id)+ 
  background_grid()

# Create statistical models ----
m_dominance_active <- glmmTMB(cbind(p_as, p_aa) ~ 
                      # random effects
                      (1|id) + (1|id_night) +
                      # temporal check
                      period * 
                      # inference covariates
                      dominance * 
                      (previous_sleep.z + avg_dominance_sleep + number_sleeping_excluding_focus) +
                      # control covariates
                      precipitation_mm + min_temp + phase,
                    family = binomial,
                    data = mechanism_data_covariates_active)
summary(m_dominance_active)

m_dominance_rest <- glmmTMB(cbind(p_sa, p_ss) ~ 
                    # random effects
                    (1|id) + (1|id_night) +
                    # temporal check
                    period * 
                    # inference covariates
                    dominance * 
                    (previous_sleep.z + avg_dominance_sleep + number_sleeping_excluding_focus) +
                    # control covariates
                    precipitation_mm + min_temp + phase, 
                  family = binomial,
                  data = mechanism_data_covariates_rest)
summary(m_dominance_rest)

m_distance_active <- glmmTMB(cbind(p_as, p_aa) ~ 
                                # random effects
                                (1|id) + (1|id_night) +
                                # temporal check
                                period * 
                                # inference covariates
                                dominance * 
                                (previous_sleep.z + number_sleeping_excluding_focus) +
                                period * avg_dist_rank_sleep +
                                # control covariates
                                precipitation_mm + min_temp + phase,
                              family = binomial,
                              data = mechanism_data_covariates_active)
summary(m_distance_active)

m_distance_rest <- glmmTMB(cbind(p_sa, p_ss) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # temporal check
                             period * 
                             # inference covariates
                             dominance * 
                             (previous_sleep.z + number_sleeping_excluding_focus) +
                             period * avg_dist_rank_sleep +
                             # control covariates
                             precipitation_mm + min_temp + phase, 
                            family = binomial,
                            data = mechanism_data_covariates_rest)
summary(m_distance_rest)


#calculate AIC of each model
aictab(cand.set = list(m_dominance_active, m_distance_active), modnames = c("dominance", "distance"))
aictab(cand.set = list(m_dominance_rest, m_distance_rest), modnames = c("dominance", "distance"))


# Combine ----
mechanism_united <- (cross_correlation_stats |>
  merge(copy(data_distances_s)[, ":="(id = case_when(id == "Sheb" ~ "Shebeleza",
                                                     T ~ id),
                                      neighb = case_when(id == "Sheb" ~ "Shebeleza",
                                                         T ~ neighb))
  ][id %in% ids_distances_ranked_correct_names & 
      neighb %in% ids_distances_ranked_correct_names
  ][, ":="(id = factor(id, levels = ids_distances_ranked_correct_names),
           neighb = factor(neighb, levels = ids_distances_ranked_correct_names))] |> 
    rename(id_focus = id,
           id_other = neighb,
           weight_dist = weight),
  by = c("id_focus", "id_other"))
  )[, ":="(dom_diff = abs(dominance_focus - dominance_other))
    ][order(dom_diff)] |>
  merge(lk_network[id != id_pair & syncrony == "syncro"
  ][, ":="(id_focus = as.character(id),
           id_other = as.character(id_pair))
  ][, c("id_focus", "id_other", "ratio")])


# final data ----
mechanism_united <- (copy(cross_correlation_stats)[, ":="(id_focus = as.character(id_focus),
                                                          id_other = as.character(id_other))] |>
                       merge(copy(data_distances_s)[, ":="(id = case_when(id == "Sheb" ~ "Shebeleza",
                                                                          T ~ id),
                                                           neighb = case_when(neighb == "Sheb" ~ "Shebeleza",
                                                                              T ~ neighb))
                       ][id %in% ids_distances_ranked_correct_names & 
                           neighb %in% ids_distances_ranked_correct_names
                       ] |> 
                         rename(id_focus = id,
                                id_other = neighb,
                                weight_dist = weight),
                       by = c("id_focus", "id_other"))
)[, ":="(id_focus = factor(id_focus, levels = idsRanked),
         id_other = factor(id_other, levels = idsRanked))
  ][, ":="(dom_diff = abs(dominance_focus - dominance_other),
         proximity = 1/gran_median)
][order(dom_diff)]

ggplot(mechanism_united) +
  geom_point(aes(log(1/gran_median), log(ratio))) +
  geom_smooth(aes(log(1/gran_median), log(ratio)),
              method = "lm")

# eigen centrality and network ----
mechanism_net <- as_tbl_graph(mechanism_united[id_focus != id_other
][, ":="(from = as.character(id_focus),
         to = as.character(id_other))
][, c("from", "to", "gran_median", "proximity")],
nodes = copy(dominance_id)[, ":="(id = as.character(id))],
directed = F) |>
  activate(nodes) |>
  left_join(dominance_id[, ":="(name = as.character(id))][, -c("id")]) |>
  mutate(centrality_spatial = centrality_eigen(weights = gran_median, directed = F),
         centrality_proximity = centrality_eigen(weights = proximity, directed = F))

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


centrality_combined <- cross_correlation_network |> 
  activate(nodes) |> 
  as.data.table() |>
  merge(mechanism_net |> 
          activate(nodes) |> 
          as.data.table(), 
        all.x = T, all.y = T)

model_eigen_1 <- lm(data = centrality_combined,
                    cbind(centrality_influence, centrality_proximity) ~ dominance + sex)
summary(model_eigen_1)

model_eigen_1 <- glmmTMB(data = centrality_combined,
                    centrality_proximity ~ dominance + sex + (1|name))
summary(model_eigen_1)

model_eigen_2 <- glmmTMB(data = centrality_combined,
                    centrality_influence ~ dominance + sex + (1|name))
summary(model_eigen)

centrality_combined_predictions <- centrality_combined[, c("name", "dominance", "sex", "baboon", "centrality_proximity", "centrality_influence")
                                                       ][, ":="(influence_predictions = predict(model_eigen_1,
                                                                                                newdata = centrality_combined[, c("dominance", "sex")]),
                                                                influence_se = predict(model_eigen_1,
                                                                                       newdata = centrality_combined[, c("dominance", "sex")],
                                                                                       se = T)$se.fit,
                                                                proximity_predictions = predict(model_eigen_2,
                                                                                                newdata = centrality_combined[, c("dominance", "sex")]),
                                                                proximity_se = predict(model_eigen_2,
                                                                                       newdata = centrality_combined[, c("dominance", "sex")],
                                                                                       se = T)$se.fit)] |>
  rename(proximity_real = centrality_proximity,
         influence_real = centrality_influence)

ggplot(centrality_combined |>
         pivot_longer(cols = c("centrality_influence", "centrality_proximity"),
                      names_to = "type",
                      values_to = "centrality")) +
  geom_point(aes(dominance, centrality, color = type)) 
  

cor.test(x = centrality_combined$centrality_proximity, 
         y = centrality_combined$centrality_influence, 
         method = 'spearman')

eigen_predictions <- data.table(centrality_proximity = seq(0.3, 1, l = 100),
                                centrality_influence = predict(model_eigen_2, 
                                                               newdata = data.table(centrality_proximity = seq(0.3, 1, l = 100))),
                                se = predict(model_eigen_2, 
                                             newdata = data.table(centrality_proximity = seq(0.3, 1, l = 100)),
                                             se = T)$se.fit)



ggplot(centrality_combined) +
  geom_point(aes(centrality_proximity, centrality_influence)) +
  geom_label(aes(centrality_proximity, centrality_influence, label = name)) +
  geom_smooth(aes(centrality_proximity, centrality_influence),
              se = T,
              method = "lm")

ggplot(centrality_combined) +
  geom_point(aes(dominance, centrality_proximity)) +
  geom_label(aes(dominance, centrality_proximity, label = baboon))

ggplot(centrality_combined) +
  geom_point(aes(dominance, centrality_influence)) +
  geom_label(aes(dominance, centrality_influence, label = baboon))

ggplot(centrality_combined) +
  geom_point(aes(dominance, centrality_influence)) +
  geom_label(aes(dominance, centrality_influence, label = baboon))

all_id$baboon[which(all_id$accelerometer == T & all_id$gps == T)]
all_id$id[which(all_id$accelerometer == T & all_id$gps == T)]

slopes <- rep(NA, 1000)
for(i in 1:1000) {
  print(i)
  slopes[i] <- cor.test(x = centrality_combined$centrality_proximity, 
           y = sample(centrality_combined$centrality_influence), 
           method = 'spearman')$estimate
}

dt_2 <- data.table(x = 1:length(slopes),
                 y = slopes)
dens_2 <- density(dt_2$y)
df_2 <- data.frame(x=dens_2$x, y=dens_2$y)
quantiles_2 <- quantile(dt_2$y, prob = 0.95)
df_2$quant <- factor(findInterval(df_2$x, quantiles_2))

spearman <- cor.test(x = centrality_combined$centrality_proximity, 
         y = centrality_combined$centrality_influence, 
         method = 'spearman')$estimate

ggplot(df_2, aes(x,y)) + 
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax=  y, fill = quant)) + 
  geom_vline(aes(xintercept = spearman)) + 
  scale_fill_manual(values = c("grey", "white")) +
  guides(fill = "none")

# stats ----
# model_mechanism_2 <- lm(data = mechanism_united[, c("peak_rho", "proximity", 
#                                                     "dominance_focus", "dominance_other")
# ][, ":="(avg_dom_pair = apply(cbind(dominance_focus, dominance_other),
#                               1, 
#                               mean))] |> unique(),
# peak_rho ~ avg_dom_pair)
# summary(model_mechanism_2)

### proximity ~ difference- in dominance
data_model_mechanism_3 <- (mechanism_united|>
           merge(dominance_id[, c("id", "sex")] |>
                   rename(id_other = id,
                          sex_other = sex)))[, ":="(id_pair = paste(id_focus, id_other) |>
                                                      str_split("") |>
                                                      lapply(sort) |>
                                                      lapply(paste,  collapse = '') |> 
                                                      unlist(),
                                                    sex_pair = paste(sex_focus, sex_other) |>
                                                      str_split("") |>
                                                      lapply(sort) |>
                                                      lapply(paste,  collapse = '') |> 
                                                      unlist())
                          ]

model_mechanism_3 <- glmmTMB(data = data_model_mechanism_3,
                        gran_median ~ dom_diff + sex_focus + sex_pair + (1|id_focus), 
                        weights = weight_dist)
summary(model_mechanism_3)

mechanism_predictions_3 <- data.table(dom_diff = seq(0, 0.8, length = 100),
                                      gran_median = predict(model_mechanism_3,
                                                            newdata = data.table(dom_diff = seq(0, 0.8, length = 100),
                                                                                 sex_pair = " aaeeeeffllmm",
                                                                                 # id_focus = 1000,
                                                                                 # id_other = 1000,
                                                                                 weight_dist = 1),
                                                            re.form = NA ),
                                      se = predict(model_mechanism_3,
                                                   newdata = data.table(dom_diff = seq(0, 0.8, length = 100),
                                                                        sex_pair = " aaeeeeffllmm",
                                                                        # id_focus = 1000,
                                                                        # id_other = 1000,
                                                                        weight_dist = 1),
                                                   se = T,
                                                   re.form = NA)$se.fit)

### rho ~ proximity
model_mechanism_1 <- glmmTMB(data = data_model_mechanism_3[, c("peak_rho", "proximity", "sex_focus", "sex_pair", "id_focus")
                                                ][, ":="(log_proximity = log(proximity),
                                                         log_peak_rho = log(peak_rho))],
                        log_peak_rho ~ log_proximity + sex_focus + sex_pair + (1|id_focus))
summary(model_mechanism_1)

mechanism_predictions <- data.table(log_proximity = log(seq(0.01, 0.126, l = 100)),
                                    log_predicted_rho = predict(model_mechanism_1, 
                                                            newdata = data.table(log_proximity = log(seq(0.01, 0.126, l = 100)))),
                                    log_predicted_rho_se = predict(model_mechanism_1, 
                                                            newdata = data.table(log_proximity = log(seq(0.01, 0.126, l = 100))),
                                                            se = T)$se.fit)



# model_mechanism_4 <- lm(data = mechanism_united[, c("dom_diff", "gran_median", 
#                                                     "weight_dist")
# ] |> unique(),
# gran_median ~ dom_diff, weights = weight_dist)
# summary(model_mechanism_3)


# Cross correlation
setkey(combined_night_m, NULL)
cross_correlation_data <- combined_night_m[, c(ids, "id_nightday", "dateTime"), with = F
][order(dateTime)
][, ":="(lagged_date_time = lead(dateTime))
][, ":="(diff = difftime(dateTime, lagged_date_time, units = "sec"))
][!is.na(diff)
][, ":="(is_continuous = ifelse(diff == make_difftime(-1, units = "sec"), 
                                T, 
                                F))
][, ":="(id_continuous = rleid(is_continuous, id_nightday))
][, ":="(id_continuous = ifelse(is_continuous == F, 
                                id_continuous - 1, id_continuous))
][, ":="(id_continuous = rleid(id_continuous))
][, -c("is_continuous", "diff", "lagged_date_time")]

time_correlation <- data.table()
for(id1 in ids) {
  for(id2 in ids) {
    print(id1)
    print(id2)
    for(shift in 0:250) {
      print(shift)
      time_correlation <- rbind(time_correlation, 
                                cross_correlation_data[, c(id1, id2, "id_nightday", "id_continuous"), 
                                                       with = F
                                ][, ":="(length = .N - shift),
                                  by = list(id_continuous)
                                ][length > 3 # p_value of cross correlation can be calculated only for more than 2 pair of observations
                                ][, ":="(predictor = get(id1), # id1 predicts the other (is earlier in time),
                                         estimand = lead(get(id2), shift)), # lead takes away the last "shift" values of id2 (id2 will be predicted, is in the future). If more then vector length there are going to be just NA
                                  by = id_continuous
                                ][!is.na(estimand) # remove excluded times
                                ][, {corr = cor.test(x = as.numeric(predictor),
                                                     y = as.numeric(estimand),
                                                     alternative = "two.sided",
                                                     method = "pearson");
                                list(phi = corr$estimate,
                                     phi_max = phi_max_rcpp(predictor, estimand),
                                     p.value = round(corr$p.value, 6),
                                     length = .N - shift,
                                     shift = shift,
                                     id_before = id1,
                                     id_after = id2)
                                },
                                by = list(id_nightday, id_continuous)
                                ]
      )
    }
  }
}

time_correlation_m <- copy(time_correlation)[, ":="(id_before = ifelse(id_before == "Sheb", "Shebeleza", id_before),
                                                    id_after = ifelse(id_after == "Sheb", "Shebeleza", id_after),
                                                    rho = phi / phi_max)
] |> 
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

ggplot(time_correlation_m[id_after != id_before]) +
  geom_line(aes(shift, rho, color = id_after, group = interaction(id_after, id_nightday, id_continuous))) +
  facet_wrap(~id_before) 

time_correlation_night <- time_correlation_m[, .(rho = weighted.mean(rho, length, na.rm = T)),
                                         by = list(id_after, id_before, id_nightday, shift)] |>
  merge(unique(combined_night_m[, c("id_nightday", 
                                          "is_night")]), 
              by = "id_nightday")

ggplot(time_correlation_night[id_after != id_before & is_night == T]) +
  geom_line(aes(shift, rho, color = id_after, group = interaction(id_after, id_nightday))) +
  facet_wrap(~id_before) +
  background_grid()

ggplot(time_correlation_night[id_after != id_before & is_night == T]) +
  geom_line(aes(shift, rho, color = as.factor(id_nightday))) +
  facet_grid(col = vars(id_after),
             rows = vars(id_before)) +
  guides(color = "none") +
  background_grid()

ggsave("figures/test/cross_correlation.png",
       bg = "white",
       height = 20,
       width = 20)

ggplot(time_correlation_night[id_after == id_before & is_night == T]) +
  geom_line(aes(shift, rho_2, color = id_after, group = id_nightday)) +
  facet_wrap(~id_before) +
  background_grid()

time_correlation_night_all <- rbind(time_correlation_night[is_night == T
][, ":="(shift = -shift)] |> 
  rename(id_focus = id_after, # with this id focus is predicted until "shift" time steps before night ends 
         id_other = id_before),
time_correlation_night[is_night == T
]|> rename(id_focus = id_before, # with this id focus predicts until "shift" time steps after night ends
           id_other = id_after)
)[, ":="(peak_rho = max(rho),
         peak_time =  unique(shift[rho == max(rho)])),
  by = list(id_focus, id_other, id_nightday)] |> 
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")] |> rename(id_focus = id, baboon_focus = baboon, dominance_focus = dominance, sex_focus = sex)) |>
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")] |> rename(id_other = id, baboon_other = baboon, dominance_other = dominance, sex_other = sex),
        by = "id_other")

ggplot(time_correlation_night_all[id_focus != id_other & shift < 50 & shift > -50]) +
  geom_line(aes(shift, rho, color = id_other)) +
  geom_point(aes(peak_time, peak_rho)) +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_night_all[id_after != id_before & is_night == T]) +
  geom_line(aes(shift, rho, color = as.factor(id_nightday))) +
  geom_point(aes(peak_time, peak_rho)) +
  facet_grid(col = vars(id_after),
             rows = vars(id_before)) +
  guides(color = "none") +
  background_grid()

ggplot(time_correlation_night_all[, .(peak_time = unique(peak_time)),
                              by = list(id_focus, id_other, dominance_focus, sex_focus, id_nightday)]) +
  geom_point(aes(dominance_focus, peak_time, color = id_nightday)) +
  geom_smooth(aes(dominance_focus, peak_time, group = id_nightday),
              method = "lm") +
  facet_wrap(~sex_focus)

ggplot(time_correlation_night_all[, .(peak_time = unique(peak_time),
                                      peak_rho = unique(peak_rho)),
                                  by = list(id_focus, id_other, dominance_focus, sex_focus, id_nightday)]) +
  geom_point(aes(peak_time, peak_rho, color = id_nightday)) +
  facet_grid(col = vars(id_focus),
             rows = vars(id_other)) 

ggplot(time_correlation_night_all[, .(peak_time = unique(peak_time),
                                      peak_rho = unique(peak_rho)),
                                  by = list(id_focus, id_other, dominance_focus, sex_focus, id_nightday)
                                            ][id_focus != id_other]) +
  geom_point(aes(peak_time, peak_rho, color = dominance_focus)) +
  facet_wrap(~id_nightday) +
  scale_color_viridis_c() +
  background_grid()

ggplot(time_correlation_night_all[, .(peak_time = unique(peak_time),
                                      peak_rho = unique(peak_rho)),
                                  by = list(id_focus, id_other, dominance_focus, sex_focus, id_nightday)
][id_focus != id_other][peak_time < 50 & peak_time > -50]) +
  geom_point(aes(dominance_focus, peak_time, color = id_nightday)) +
  geom_smooth(aes(dominance_focus, peak_time, group = interaction(id_nightday, sex_focus)),
              method = "lm") +
  scale_color_viridis_c() +
  background_grid()


time_correlation_s <- merge(time_correlation_m,
                            unique(combined_night_m[, c("id_nightday", 
                                                        "is_night")]), 
                            by = "id_nightday")[, .(rho = weighted.mean(rho, length, na.rm = T)),
  by = list(id_after, id_before, is_night, shift)]

ggplot(time_correlation_s[id_after != id_before & is_night == T]) +
  geom_line(aes(shift, rho, color = id_after, group = id_after)) +
  facet_wrap(~id_before) +
  background_grid()

ggplot(time_correlation_s[id_after == id_before & is_night == T]) +
  geom_line(aes(shift, rho_2, color = id_after, group = id_after)) +
  background_grid()

# All nights toghter ----

time_correlation_s_all <- rbind(time_correlation_s[is_night == T
                                                   ][, ":="(shift = -shift)] |> 
                                  rename(id_focus = id_after, # with this id focus is predicted until "shift" time steps before night ends 
                                         id_other = id_before),
                                time_correlation_s[is_night == T
                                ]|> rename(id_focus = id_before, # with this id focus predicts until "shift" time steps after night ends
                                           id_other = id_after)
                                )[, ":="(peak_rho = max(rho),
                                         peak_time =  unique(shift[rho == max(rho)])),
                                  by = list(id_focus, id_other)] |> 
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")] |> rename(id_focus = id, baboon_focus = baboon, dominance_focus = dominance, sex_focus = sex)) |>
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")] |> rename(id_other = id, baboon_other = baboon, dominance_other = dominance, sex_other = sex),
        by = "id_other")

ggplot(time_correlation_s_all[id_focus != id_other & shift < 50 & shift > -50]) +
  geom_line(aes(shift, log(rho), color = id_other)) +
  geom_point(aes(peak_time, log(peak_rho))) +
  facet_wrap(~id_focus) +
  background_grid()

ggplot(time_correlation_s_all[, .(peak_time = unique(peak_time)),
                              by = list(id_focus, id_other, dominance_focus, sex_focus)]) +
  geom_point(aes(dominance_focus, peak_time)) +
  geom_smooth(aes(dominance_focus, peak_time, group = sex_focus),
              method = "lm")

# Stats ----

cross_correlation_stats <- (time_correlation_s_all[id_focus != id_other 
][, .(peak_rho = unique(peak_rho),
      sex_focus = unique(sex_focus),
      peak_time = unique(peak_time)),
  by = list(id_focus, id_other, dominance_focus)
][order(peak_rho)
  ][, ":="(proportion_influence = sum(peak_time > 0) / .N),
    by = id_focus
    ][, ":="(id_pair = paste(id_focus, id_other) |>
               str_split("") |>
               lapply(sort) |>
               lapply(paste,  collapse = '') |> 
               unlist())] |>
  merge(dominance_id[, c("id", "dominance", "sex")] |>
          rename(id_other = id,
                 dominance_other = dominance,
                 sex_other = sex)))[, ":="(sex_pair = paste(sex_focus, sex_other) |>
                                             str_split("") |>
                                             lapply(sort) |>
                                             lapply(paste,  collapse = '') |> 
                                             unlist())]

test <- ross_correlation_stats[, c("peak_rho", "id_focus", "id_pair", "sex_focus", "sex_pair", "dominance_focus")
][, ":="(id_pair = as.factor(as.numeric(as.factor(id_pair))),
         sex_pair = case_when(sex_pair == " aaeellmm" ~ "male_male",
                              sex_pair == " aaeeeeffllmm" ~ "female_female",
                              sex_pair == " aaeeefllmm" ~ "mixed"))]

cross_correlation_model <- glmmTMB(data = test, #cross_correlation_stats[, c("peak_rho", "id_focus", "id_pair", "sex_focus", "sex_pair", "dominance_focus")],
                              peak_rho ~ dominance_focus + sex_pair + sex_focus + (1|id_focus)  + (1|id_pair))
summary(cross_correlation_model)

write.csv(cross_correlation_stats[, c("peak_rho", "id_focus", "id_pair", "sex_focus", "sex_pair", "dominance_focus")
                                  ][, ":="(id_pair = as.factor(as.numeric(as.factor(id_pair))),
                                           sex_pair = case_when(sex_pair == " aaeellmm" ~ "male_male",
                                                                sex_pair == " aaeeeeffllmm" ~ "female_female",
                                                                sex_pair == " aaeeefllmm" ~ "mixed"))],
          file = "data/network_regression.csv",
          row.names = F)

cross_correlation_model_predict <- data.table(dominance_focus = seq(0, 1, l = 10),
                                              sex_focus = "female",
                                              peak_rho = mean(cross_correlation_stats$peak_rho)) |>
  mutate(prediction = predict(cross_correlation_model,
          newdata = data.table(dominance_focus = seq(0, 1, l = 10),
                               sex_focus = "female",
                               peak_rho = mean(cross_correlation_stats$peak_rho)),
          se = T)$fit,
         se = predict(cross_correlation_model,
                       newdata = data.table(dominance_focus = seq(0, 1, l = 10),
                                            sex_focus = "female",
                                            peak_rho = mean(cross_correlation_stats$peak_rho)),
                       se = T)$se.fit)

# cross_correlation_model <- lm(data = cross_correlation_stats,
#                               peak_time ~ dominance_focus + sex_focus + peak_rho)
# summary(cross_correlation_model)
# 
# cross_correlation_model_predict <- data.table(dominance_focus = seq(0, 1, l = 10),
#                                               sex_focus = "female",
#                                               peak_rho = mean(cross_correlation_stats$peak_rho)) |>
#   mutate(prediction = predict(cross_correlation_model,
#           newdata = data.table(dominance_focus = seq(0, 1, l = 10),
#                                sex_focus = "female",
#                                peak_rho = mean(cross_correlation_stats$peak_rho)),
#           se = T)$fit,
#          se = predict(cross_correlation_model,
#                        newdata = data.table(dominance_focus = seq(0, 1, l = 10),
#                                             sex_focus = "female",
#                                             peak_rho = mean(cross_correlation_stats$peak_rho)),
#                        se = T)$se.fit)

## Non-parametric analysis ----

ggplot(cross_correlation_stats[peak_time != 0][, .(proportion_influence = unique(proportion_influence),
                                   size = .N),
                               by = list(dominance_focus, id_focus, sex_focus)]) +
  geom_point(aes(dominance_focus, proportion_influence)) +
  geom_smooth(aes(dominance_focus, proportion_influence, 
                  group = sex_focus, weight = size), 
              se = F, 
              method = "glm",
              method.args = list(family = "binomial")) +
  background_grid()

non_param_slope <- glm(data = cross_correlation_stats[, .(proportion_influence = unique(proportion_influence),
                                                          size = .N),
                                                      by = list(dominance_focus, id_focus, sex_focus)],
                       proportion_influence ~ dominance_focus + sex_focus, weights = size)
summary(non_param_slope)
coefficients(non_param_slope)[2]

non_param_slope_predict <- data.table(dominance_focus = seq(0, 0.95, length = 100),
                                      sex_focus = "female")
non_param_slope_predict$proportion_influence <- predict(non_param_slope, newdata = non_param_slope_predict)

cross_correlation_stats_matrix <- (cross_correlation_stats[order(id_focus, id_other)
][, c("id_focus", "id_other", "peak_time")
] |>
  pivot_wider(names_from = id_focus,
              values_from = peak_time)
)[c(12, 1:11), ] # reorder

m_cross_corr <- cross_correlation_stats_matrix[, -1] |>
  as.matrix() 

randomized_data_all <- data.table(id_focus = character(),
                                  number_influences = numeric(),
                                  #id_other = character(),
                                  #peak_time = numeric(),
                                  replicate = numeric())

randomized_coeff <- rep(NA, 5000)
randomized_coeff_2 <- rep(NA, 5000)

for(i in 1:5000) {
  print(i)
  
  edges <- na.omit(m_cross_corr[m_cross_corr >= 0]) |> sample()
  randomized_data <- data.table(id_focus = idsRanked,
                                number_influences = rep(0, 12)
                                # id_other = character(),
                                # peak_time = numeric()
                                )
  
  count_indvidual <- rep(0, 12)
  individual_allowed <- idsRanked
  
  for(edge in edges) {
    id_focus_chosen <- sample(individual_allowed, 1)
    id_other_chosen <- sample(individual_allowed[individual_allowed != id_focus_chosen], 1)
    # do not count zero edges
    if(edge != 0) {
      randomized_data[id_focus == id_focus_chosen, ":="(number_influences = number_influences + 1)]  
    }
    # cannot have more than 11 edges in total (coming in + coming out), zero counts here though
    count_indvidual[which(idsRanked == id_focus_chosen)] <- count_indvidual[which(idsRanked == id_focus_chosen)] + 1
    count_indvidual[which(idsRanked == id_other_chosen)] <- count_indvidual[which(idsRanked == id_other_chosen)] + 1
    
    individual_allowed[count_indvidual != 11]
  }
  randomized_data_all <- rbind(randomized_data_all, 
                               randomized_data[, ":="(replicate = i)])
  
  random_slope <- glm(data = randomized_data[, ":="(proportion_influence = number_influences / 11,
                                                    size = 11)] |>
                        merge(dominance_id[, c("id", "dominance", "sex")] |>
                                rename(id_focus = id, dominance_focus = dominance, sex_focus = sex)),
  proportion_influence ~ dominance_focus + sex_focus, weights = size)
  
  randomized_coeff[i] <- coefficients(random_slope)[2]
  randomized_coeff_2[i] <- coefficients(random_slope)[3]
}

dt <- data.table(x = 1:length(randomized_coeff),
                 y = randomized_coeff)
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
quantiles <- quantile(dt$y, prob = 0.95)
df$quant <- factor(findInterval(df$x, quantiles))
ggplot(df, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = 0, ymax=  y, fill = quant)) + 
  scale_fill_manual(values = c("grey", "white")) +
  guides(fill = "none")

ggplot() +
  geom_histogram(aes(randomized_coeff)) +
  geom_vline(aes(xintercept = coefficients(non_param_slope)[2]), color = "red") +
  geom_vline(aes(xintercept = quantile(randomized_coeff, probs = 0.95)), color = "black") +
  background_grid()

1 - ecdf(randomized_coeff)(0.44)
ecdf(randomized_coeff_2)(-0.4165)

ggplot() +
  geom_histogram(aes(randomized_coeff_2)) 


# Network ----

cross_correlation_network <- as_tbl_graph(cross_correlation_stats[id_focus != id_other
][, ":="(from = as.character(id_focus),
         to = as.character(id_other))
][peak_time < 0][, c("from", "to", "peak_rho", "peak_time")],
nodes = copy(dominance_id)[, ":="(id = as.character(id))],
directed = T) |>
  activate(nodes) |>
  left_join(dominance_id[, ":="(name = as.character(id))][, -c("id")]) |>
  mutate(centrality_influence = centrality_eigen(weights = peak_rho, directed = F),
         centrality_influence_1 = centrality_eigen(weights = peak_rho, directed = T),
         centrality_influence_2 = centrality_eigen(weights = peak_time, directed = T))

ggraph(cross_correlation_network |>   
         activate(edges)  |>
         filter(peak_rho > quantile(peak_rho, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    arrow = arrow(type = "closed"),
    width = 2,
    aes(color = peak_rho
        #width = peak_rho
    )) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      fill = dominance,
      #size = dominance
    ), 
    #size = 8, 
    stroke = 2, shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_node_label(
    aes(label = baboon),
    nudge_y = 0.05
  ) +
  ggtitle("A)") +
  guides(color = 'none')

rev(c(0.49538027, 0.74122434, 0.25410185, 
      0.16527563, 0.43407303, 0.80783389, 
      0.06221967, 0.11885470, 0.83443748,
      0.08066892, 0.97729973, 0.12102426))

nodes_position <- data.table(id = factor(rev(idsRanked), 
                                         levels = idsRanked),
                             x = c(0.4+0.5, 0.4+0.6, 0.35, 
                                   0.2, 0.45, 0.6, 
                                   0.25, 0.5, 0.65,
                                   0.3, 0.75, 0.1),
                             y = seq(5, 0, l = 12)) |>
  merge(dominance_id[, c("id", "dominance", "baboon", "sex")])

cc_net <- (cross_correlation_stats[id_focus != id_other
                                   ][peak_time > 0] |>
             merge(nodes_position[, c("id", "x", "y")] |>
                     rename(id_focus = id,
                            x_focus = x,
                            y_focus = y),
                   by = "id_focus") |>
             merge(nodes_position[, c("id", "x", "y")] |>
                     rename(id_other = id,
                            x_other = x,
                            y_other = y),
                   by = "id_other"))[order(peak_rho)
                                     ][, ":="(out_degree = .N),
                                                      by = id_focus]

nodes_position_info <- (nodes_position |>
  merge(unique(cc_net[, c("id_focus", "out_degree")]) |>
                 rename(id = id_focus)))[order(-out_degree)]

ggplot(cc_net) +
  geom_segment(aes(x = x_focus, y = y_focus,
                   xend = x_other, yend = y_other,
                   color = peak_rho),
               linewidth = 1,
               arrow = arrow(angle = 10, 
                             ends = "last", 
                             type = "closed")) +
  geom_point(data = nodes_position_info,
             aes(x, y, fill = dominance, 
                 size = outdegree),
             size = 5, shape = 21, stroke = 1) +
  geom_label(data = nodes_position,
             aes(x, y, label = id),
             nudge_y = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Dominance") +
  scale_color_viridis_c(name = "Peak\ninfluence") +
  scale_y_continuous(breaks = NULL,
                     name = NULL) +
  scale_x_continuous(breaks = NULL,
                     name = NULL) +
  theme_void()


# check if temporal structure changes null expectation (very sleazy)

r <- 1000 # autocorrelation
n <- 100000 # sample size

test <- data.table(sample = rep(0, 10000),
                   rho_auto = rep(0, 10000),
                   rho = rep(0, 10000),
                   p_value_auto = rep(0, 10000),
                   p_value = rep(0, 10000),
                   low = rep(0, 10000),
                   high = rep(0, 10000),
                   low_auto = rep(0, 10000),
                   high_auto = rep(0, 10000))

for(i in 1:10000) {
  print(i)
series_1_auto <- rbinom(n / r, 1, prob = 0.9) %>>%
  rep(each = r)
series_2_auto <- rbinom(n / r, 1, prob = 0.9) %>>%
  rep(each = r)
series_1 <- rbinom(n = 10000, size = 1, prob = 0.9) # - 0.9
series_2 <- rbinom(n = 10000, size = 1, prob = 0.9) # - 0.1
cor <- cor.test(x = series_1,
         y = series_2,
         alternative = "two.sided",
         method = "pearson")
cor_auto <- cor.test(x = series_1_auto,
         y = series_2_auto,
         alternative = "two.sided",
         method = "pearson")
test[i, ":="(sample = i,
             rho_auto = cor_auto$estimate,
             rho = cor$estimate,
             p_value_auto = round(cor_auto$p.value, 6),
             p_value = round(cor$p.value, 6),
             low = cor$conf.int[1],
             high = cor$conf.int[2],
             low_auto = cor_auto$conf.int[1],
             high_auto = cor_auto$conf.int[2]) ]
}

ggplot(test) +
  geom_density(aes(rho), color = "black") +
  geom_density(aes(low), color = "lightgrey") +
  geom_density(aes(high), color = "darkgrey") +
  geom_density(aes(rho_auto), color = "red") +
  geom_density(aes(low_auto), color = "pink") +
  geom_density(aes(high_auto), color = "magenta") +
  background_grid()

ggplot(test) +
  geom_histogram(aes(p_value), color = "red") +
  geom_histogram(aes(p_value_auto), color = "black")  +
  background_grid()

ggplot(test) +
  geom_point(aes(rho, p_value)) +
  #geom_smooth(aes(rho, p_value)) +
  background_grid()

ggplot(test) +
  geom_point(aes(rho_auto, p_value_auto)) +
  #geom_smooth(aes(rho_auto, p_value_auto)) +
  background_grid()

ggplot(test) +
  geom_point(aes(rho_auto, p_value_auto), color = "red") +
  geom_point(aes(rho, p_value)) +
  scale_y_log10() +
  background_grid()


test_2 <- data.table(sample = rep(0, 10000),
                   rho_diff_2 = rep(0, 10000),
                   rho_diff = rep(0, 10000),
                   rho = rep(0, 10000),
                   p_value_diff_2 = rep(0, 10000),
                   p_value_diff = rep(0, 10000),
                   p_value = rep(0, 10000))

for(i in 1:1000) {
  print(i)
  
  series_1 <- rbinom(n = 10000, size = 1, prob = 0.9)
  series_2 <- rbinom(n = 10000, size = 1, prob = 0.9) 
  
  series_1_diff <- rbinom(n = 10000, size = 1, prob = 0.5)
  series_2_diff <- rbinom(n = 10000, size = 1, prob = 0.5)
  
  series_1_diff_2 <- rbinom(n = 10000, size = 1, prob = 0.9) - 0.9
  series_2_diff_2 <- rbinom(n = 10000, size = 1, prob = 0.1) - 0.1
  
  cor <- cor.test(x = series_1,
                  y = series_2,
                  alternative = "two.sided",
                  method = "pearson")
  cor_diff <- cor.test(x = series_1_diff,
                       y = series_2_diff,
                       alternative = "two.sided",
                       method = "pearson")
  cor_diff_2 <- cor.test(x = series_1_diff_2,
                       y = series_2_diff_2,
                       alternative = "two.sided",
                       method = "pearson")
  test_2[i, ":="(sample = i,
               rho_diff_2 = cor_diff_2$estimate,
               rho_diff = cor_diff$estimate,
               rho = cor$estimate,
               p_value_diff_2 = cor_diff_2$p.value,
               p_value_diff = cor_diff$p.value,
               p_value = cor$p.value)]
}

ggplot(test_2) +
  geom_point(aes(rho, log(p_value))) +
  geom_point(aes(rho_diff, log(p_value_diff)), color = "red") +
  geom_point(aes(rho_diff_2, log(p_value_diff_2)), color = "blue") +
  #scale_y_log10() +
  background_grid()

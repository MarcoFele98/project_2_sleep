### Visualize model predictions ----

new_data <- expand.grid(id = as.factor(idsRanked), 
                        id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                        previous_sleep.z = 0,
                        period = c("night_time", "bed_time", "wakey_time"),
                        date_median_VeDBA_day = mean(mc_data_covariates_rest$min_temp),
                        number_sleeping_excluding_focus = 0:11,
                        avg_dominance_sleep = seq(0, 1, l = 11),
                        precipitation_mm = 0,
                        min_temp = mean(mc_data_covariates_rest$min_temp),
                        phase = 0) |>
  mutate(id = factor(id, levels = idsRanked)) |>
  merge(dominance_id[, c("dominance", "id", "baboon")]) |>
  as.data.table()

predictions <- as.data.table(cbind(new_data, # confidence intervals need bootstrapping for random effects
                                   predict(object = m_active, 
                                           newdata = new_data, 
                                           type = "response",
                                           #re.form = NA,
                                           allow.new.levels = T,
                                           se.fit = T) |>
                                     as.data.table() |>
                                     rename(p_as = fit,
                                            p_as_se = se.fit),
                                   predict(object = m_rest, 
                                           newdata = new_data, 
                                           type = "response",
                                           #re.form = NA,
                                           allow.new.levels = T,
                                           se.fit = T) |>
                                     as.data.table() |>
                                     rename(p_sa = fit,
                                            p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                       p_active = p_sa / (p_as + p_sa))]

#### Rain ----
new_data_rain <- expand.grid(id = as.factor(666), 
                             id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                             previous_sleep.z = 0,
                             period = "night_time",
                             number_sleeping_excluding_focus = 0:11, #c(0, 6, 11),
                             avg_dominance_sleep = seq(0, 1, l = 11),
                             precipitation_mm = 0:30,
                             min_temp = mean(mc_data_covariates_rest$min_temp),
                             phase = 0,
                             dominance = dominance_ranked) |>
  as.data.table()

predictions_rain <- as.data.table(cbind(new_data_rain, # confidence intervals need bootstrapping for random effects
                                        predict(object = m_active, 
                                                newdata = new_data_rain, 
                                                type = "response",
                                                re.form = NA,
                                                se.fit = T) |>
                                          as.data.table() |>
                                          rename(p_as = fit,
                                                 p_as_se = se.fit),
                                        predict(object = m_rest, 
                                                newdata = new_data_rain, 
                                                type = "response",
                                                re.form = NA,
                                                se.fit = T) |>
                                          as.data.table() |>
                                          rename(p_sa = fit,
                                                 p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                            p_active = p_sa / (p_as + p_sa))]

ggplot(predictions_rain[avg_dominance_sleep == 0.5 & number_sleeping_excluding_focus == 9 & dominance == 0.5]) +
  geom_ribbon(aes(x = precipitation_mm, 
                  ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                  ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
              color = NA, alpha = 0.2) +
  geom_line(aes(precipitation_mm, p_rest)) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  scale_fill_viridis_c(name = "DOminance",
                       option = "plasma") +
  xlab("Dominance focus individual") +
  background_grid() +
  guides(fill = "none") +
  ylab(expression("Rest probability  "(p[R]))) 

ggsave(here("figures", "mc_results", "social", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")




#### Period ----

new_data_period <- expand.grid(id = as.factor(666), 
                               id_night = 666, 
                               previous_sleep.z = seq(-2, 2, l = 10),
                               period = possible_periods,
                               number_sleeping_excluding_focus = c(0, 5, 6, 9, 11), 
                               avg_dominance_sleep = c(0, 0.5, 0.6, 1), 
                               precipitation_mm = 0,
                               min_temp = mean(mc_data_covariates_rest$min_temp),
                               phase = 0,
                               dominance = dominance_ranked) |>
  as.data.table()

predictions_period <- as.data.table(cbind(new_data_period, # confidence intervals need bootstrapping for random effects
                                          predict(object = m_active, 
                                                  newdata = new_data_period, 
                                                  type = "response",
                                                  re.form = NA,
                                                  se.fit = T) |>
                                            as.data.table() |>
                                            rename(p_as = fit,
                                                   p_as_se = se.fit),
                                          predict(object = m_rest, 
                                                  newdata = new_data_period, 
                                                  type = "response",
                                                  re.form = NA,
                                                  se.fit = T) |>
                                            as.data.table() |>
                                            rename(p_sa = fit,
                                                   p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                              p_active = p_sa / (p_as + p_sa))]

# This one better probably
# new_data <- expand.grid(id = as.factor(idsRanked), 
#                         id_night = c(unique(mc_data_covariates_rest$id_night), 
#                                      as.factor(666)), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
#                         previous_sleep.z = 0,
#                         period = c("night_time", "bed_time", "wakey_time"),
#                         number_sleeping_excluding_focus = 0:11,
#                         avg_dominance_sleep = seq(0, 1, l = 11),
#                         precipitation_mm = seq(0, 30, l = 10),
#                         min_temp = mean(mc_data_covariates_rest$min_temp),
#                         phase = 0) |>
#   mutate(id = factor(id, levels = idsRanked)) |>
#   merge(dominance_id[, c("dominance", "id")]) |>
#   as.data.table()
# 
# predictions <- as.data.table(cbind(new_data, # confidence intervals need bootstrapping for random effects
#                                    predict(object = m_active, 
#                                            newdata = new_data, 
#                                            type = "response",
#                                            allow.new.levels = T,
#                                            se.fit = T) |>
#                                      as.data.table() |>
#                                      rename(p_as = fit,
#                                             p_as_se = se.fit),
#                                    predict(object = m_rest, 
#                                            newdata = new_data, 
#                                            type = "response",
#                                            allow.new.levels = T,
#                                            se.fit = T) |>
#                                      as.data.table() |>
#                                      rename(p_sa = fit,
#                                             p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
#                                                                        p_active = p_sa / (p_as + p_sa))]



##### Show nice results ----
ggplot(predictions_period[number_sleeping_excluding_focus == 6 & avg_dominance_sleep == 0.5]) +
  geom_line(aes(previous_sleep.z, p_rest, 
                color = dominance, group = dominance),
            linewidth = 1) +
  facet_wrap(~period) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "period", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 7,
       bg = "white")

ggplot(predictions_period[number_sleeping_excluding_focus == 6 & avg_dominance_sleep == 0.5]) +
  geom_line(aes(previous_sleep.z, p_as, 
                color = dominance, group = dominance),
            linewidth = 1) +
  facet_wrap(~period) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "period", "p_as.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 7,
       bg = "white")

ggplot(predictions_period[number_sleeping_excluding_focus == 6 & avg_dominance_sleep == 0.5]) +
  geom_line(aes(previous_sleep.z, p_sa, 
                color = dominance, group = dominance),
            linewidth = 1) +
  facet_wrap(~period) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "period", "p_sa.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 7,
       bg = "white")

##### Explore ----
# transition probabilities
ggplot(predictions_period) +
  geom_line(aes(number_sleeping_excluding_focus, p_as, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(period),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggplot(predictions_period) +
  geom_line(aes(previous_sleep.z, p_sa, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(number_sleeping_excluding_focus),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

# state probabilities
ggplot(predictions_period) +
  geom_line(aes(previous_sleep.z, p_rest, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(number_sleeping_excluding_focus),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()


#### Quantity of sleep previous night (homeostasis) ----

new_data_sleep <- expand.grid(id = 666, 
                              id_night = 666, # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                              previous_sleep.z = seq(-2, 2, by = 0.1),
                              period = c("night_time"),
                              number_sleeping_excluding_focus = c(0, 6, 11), #c(0, 6, 11),
                              avg_dominance_sleep = c(0, 0.5, 1), #
                              precipitation_mm = 0,
                              min_temp = mean(mc_data_covariates_rest$min_temp),
                              phase = 0,
                              dominance = dominance_ranked) |>
  as.data.table()

predictions_sleep <- as.data.table(cbind(new_data_sleep, # confidence intervals need bootstrapping for random effects
                                         p_as = predict(object = m_active, 
                                                        newdata = new_data_sleep, 
                                                        type = "response",
                                                        allow.new.levels = T),
                                         p_sa = predict(object = m_rest, 
                                                        newdata = new_data_sleep, 
                                                        type = "response",
                                                        allow.new.levels = T)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                                       p_active = p_sa / (p_as + p_sa))]

##### Show nice results ----
# transition probabilities
ggplot(predictions_sleep) +
  geom_line(aes(previous_sleep.z, p_as, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(number_sleeping_excluding_focus),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggplot(predictions_sleep) +
  geom_line(aes(previous_sleep.z, p_sa, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(number_sleeping_excluding_focus),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

# state probabilities
ggplot(predictions_sleep) +
  geom_line(aes(previous_sleep.z, p_rest, 
                color = dominance, group = dominance)) +
  facet_grid(col = vars(number_sleeping_excluding_focus),
             row = vars(avg_dominance_sleep)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

##### Explore ----


#### Dominance ----

# Create data 
new_data_dominance <- expand.grid(id = as.factor(666), 
                                  id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                                  previous_sleep.z = 0,
                                  period = c("night_time"),
                                  number_sleeping_excluding_focus = 0:11, #c(0, 6, 11),
                                  avg_dominance_sleep = seq(0, 1, l = 11),
                                  precipitation_mm = 0,
                                  min_temp = mean(mc_data_covariates_rest$min_temp),
                                  phase = 0,
                                  dominance = dominance_ranked) |>
  as.data.table()

predictions_dominance <- as.data.table(cbind(new_data_dominance, # confidence intervals need bootstrapping for random effects
                                             predict(object = m_active, 
                                                     newdata = new_data_dominance, 
                                                     type = "response",
                                                     re.form = NA,
                                                     se.fit = T) |>
                                               as.data.table() |>
                                               rename(p_as = fit,
                                                      p_as_se = se.fit),
                                             predict(object = m_rest, 
                                                     newdata = new_data_dominance, 
                                                     type = "response",
                                                     re.form = NA,
                                                     se.fit = T) |>
                                               as.data.table() |>
                                               rename(p_sa = fit,
                                                      p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                                 p_active = p_sa / (p_as + p_sa))]

##### Show nice results ----

# clean
ggplot(predictions_dominance[ avg_dominance_sleep %in% c(0.5) & number_sleeping_excluding_focus == 5]) +
  geom_line(aes(dominance, 1 - p_sa),
            linewidth = 3) +
  scale_color_viridis_c(name = "Number\nsleeping") +
  #facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "p_sa_clean.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_dominance[ avg_dominance_sleep %in% c(0.5) & number_sleeping_excluding_focus == 5]) +
  geom_line(aes(dominance, p_as),
            linewidth = 3) +
  scale_color_viridis_c(name = "Number\nsleeping") +
  #facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "p_as_clean.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_dominance[avg_dominance_sleep %in% c(0.5) & number_sleeping_excluding_focus == 5]) +
  geom_line(aes(dominance, p_rest),
            linewidth = 3) +
  scale_color_viridis_c(name = "Number\nsleeping") +
  #facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "p_rest_clean.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(data = predictions_dominance[ avg_dominance_sleep %in% c(0.5) & number_sleeping_excluding_focus == 5]) +
  geom_line(aes(dominance, p_as),
            linewidth = 3, color = "#00BFC4") +
  geom_line(aes(dominance, p_sa),
            linewidth = 3, color = "#F8766D") +
  annotate("text", x = 0.85, y = 0.08, label= "Start rest", color = "#00BFC4") +
  annotate("text", x = 0.1, y = 0.045, label= "End rest", color = "#F8766D") +
  ylab("Transition probability") +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "combined_clean.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

# more info
ggplot(predictions_dominance[ avg_dominance_sleep %in% c(0.5)]) +
  geom_line(aes(dominance, p_rest, 
                color = number_sleeping_excluding_focus, group = number_sleeping_excluding_focus),
            linewidth = 1) +
  scale_color_viridis_c(name = "Number\nsleeping") +
  #facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "p_rest_quantity.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_dominance[ avg_dominance_sleep %in% c(0.5)]) +
  geom_line(aes(dominance, p_sa, 
                color = number_sleeping_excluding_focus, group = number_sleeping_excluding_focus),
            linewidth = 1) +
  scale_color_viridis_c(name = "Number\nsleeping") +
  #facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggsave(here("figures", "mc_results", "dominance", "p_sa_quality.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")


##### With interactions ----
# probability transition
ggplot(predictions_dominance) +
  #geom_point(aes(dominance, partial_p_sa)) +
  geom_line(aes(dominance, p_sa),
            linewidth = 1) +
  facet_wrap(~id) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  background_grid()

ggplot(predictions_dominance) +
  #geom_point(aes(dominance, partial_p_sa)) +
  geom_line(aes(dominance, p_as),
            linewidth = 1) +
  facet_wrap(~id) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  background_grid()

ggplot(predictions_dominance[#number_sleeping_excluding_focus %in% c(0, 6, 11) &
  proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1)]) +
  geom_line(aes(dominance, 1 - p_as, group = number_sleeping_excluding_focus, 
                color = number_sleeping_excluding_focus),
            linewidth = 1) +
  facet_wrap(~avg_dominance_sleep) +
  scale_color_viridis_c(name = "Number sleeping") +
  background_grid()

ggplot(predictions_dominance[#number_sleeping_excluding_focus %in% c(0, 6, 11) &
  dominance %in% c(0.5)]) +
  geom_line(aes(number_sleeping_excluding_focus, p_as, group = avg_dominance_sleep, 
                color = avg_dominance_sleep),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
                color = avg_dominance_sleep), lty = "dashed",
            linewidth = 1) +
  scale_color_viridis_c(name = "avg_dominance_sleep") +
  background_grid()

ggplot(predictions_dominance[#proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1) &
  number_sleeping_excluding_focus %in% c(0, 6, 11)]) +
  geom_line(aes(dominance, p_as, group = avg_dominance_sleep, 
                color = avg_dominance_sleep),
            linewidth = 1) +
  facet_wrap(~number_sleeping_excluding_focus) +
  scale_color_viridis_c(name = "Dominance sleepers",
                        option = "plasma") +
  background_grid()

# probability state
ggplot(predictions_dominance) +
  #geom_point(aes(dominance, partial_p_sa)) +
  geom_line(aes(dominance, p_rest),
            linewidth = 1) +
  facet_wrap(~id) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  background_grid()

ggplot(predictions_dominance[dominance %in% c(0, 0.5, 1) & avg_dominance_sleep %in% c(0, 0.5, 1)]) +
  #geom_point(aes(dominance, partial_p_sa)) +
  geom_line(aes(number_sleeping_excluding_focus, p_rest),
            linewidth = 1) +
  facet_wrap(~id) +
  facet_grid(cols = vars(dominance),
             rows = vars(avg_dominance_sleep)) +
  background_grid()

ggplot(predictions_dominance) +
  #geom_point(aes(dominance, partial_p_sa)) +
  geom_line(aes(dominance, p_rest),
            linewidth = 1) +
  facet_wrap(~id) +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  background_grid()

ggplot(predictions_dominance[#number_sleeping_excluding_focus %in% c(0, 6, 11) &
  proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1)]) +
  geom_line(aes(dominance, p_rest, group = number_sleeping_excluding_focus, 
                color = number_sleeping_excluding_focus),
            linewidth = 1) +
  facet_wrap(~avg_dominance_sleep) +
  scale_color_viridis_c(name = "Number sleeping") +
  background_grid()

ggplot(predictions_dominance[#proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1) &
  number_sleeping_excluding_focus %in% c(0, 6, 11)]) +
  geom_line(aes(dominance, p_rest, group = avg_dominance_sleep, 
                color = avg_dominance_sleep),
            linewidth = 1) +
  facet_wrap(~number_sleeping_excluding_focus) +
  scale_color_viridis_c(name = "Dominance sleepers",
                        option = "plasma") +
  background_grid()

#### Social components ----
# new_data <- expand.grid(id = idsRanked, 
#                         id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
#                         previous_sleep.z = 0,
#                         period = c("night_time", "bed_time", "wakey_time"),
#                         dominance = dominance_ranked, 
#                         number_sleeping_excluding_focus = 0:11,
#                         avg_dominance_sleep = seq(0, 1, l = 11),
#                         precipitation_mm = 0,
#                         min_temp = mean(mc_data_covariates_rest$min_temp),
#                         phase = 0) |>
#   mutate(id = factor(id, levels = idsRanked)) |>
#   merge(dominance_id[, c("dominance", "id")]) |>
#   as.data.table()
# 
# predictions <- as.data.table(cbind(new_data, # confidence intervals need bootstrapping for random effects
#                                    predict(object = m_active, 
#                                            newdata = new_data, 
#                                            type = "response",
#                                            re.form = NA,
#                                            se.fit = T) |>
#                                      as.data.table() |>
#                                      rename(p_as = fit,
#                                             p_as_se = se.fit),
#                                    predict(object = m_rest, 
#                                            newdata = new_data, 
#                                            type = "response",
#                                            re.form = NA,
#                                            se.fit = T) |>
#                                      as.data.table() |>
#                                      rename(p_sa = fit,
#                                             p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
#                                                                    p_active = p_sa / (p_as + p_sa))]

##### Show nice results ----
ggplot(predictions[avg_dominance_sleep == 0.5][period == "night_time"]) +
  geom_line(aes(number_sleeping_excluding_focus, p_rest,
                color = dominance,
                group = interaction(dominance, id)),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "social", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_dominance[number_sleeping_excluding_focus %in% c(6)]) +
  geom_line(aes(dominance, p_rest, group = avg_dominance_sleep, 
                color = avg_dominance_sleep),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance\nsleepers",
                        option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "social", "p_rest_dominance_dominance_clean.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61 & dominance == 0.5]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_sa),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_as),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_active),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  background_grid()

##### Explore ----

# probability transitions
ggplot(predictions[period == "night_time"]) +
  geom_line(aes(avg_dominance_sleep, 
                p_sa,
                color = number_sleeping_excluding_focus,
                group = interaction(number_sleeping_excluding_focus, id_night)),
            linewidth = 1) +
  scale_color_viridis_c(name = "number resting",
                        option = "viridis") +
  facet_wrap(~id) +
  background_grid()

ggplot(predictions[period == "night_time"]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_sa,
                color = avg_dominance_sleep,
                group = interaction(avg_dominance_sleep, id_night)),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance sleepers",
                        option = "plasma") +
  facet_wrap(~id) +
  background_grid()

ggplot(predictions[avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_sa,
                color = period,
                group = period),
            linewidth = 1) +
  facet_wrap(~id) +
  background_grid()

ggplot(predictions_dominance[period == "night_time" & dominance == 0.5]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_sa),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_as),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_active),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  facet_wrap(~avg_dominance_sleep) +
  background_grid()

ggplot(predictions_dominance[period == "night_time" & avg_dominance_sleep > 0.59 & avg_dominance_sleep < 0.61]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_sa),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                1 - p_as),
            linewidth = 1) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_active),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  facet_wrap(~dominance) +
  background_grid()

# probability state
ggplot(predictions[period == "night_time"]) +
  geom_line(aes(avg_dominance_sleep, 
                p_rest,
                color = number_sleeping_excluding_focus,
                group = interaction(number_sleeping_excluding_focus, id_night)),
            linewidth = 1) +
  scale_color_viridis_c(name = "number resting",
                        option = "viridis") +
  facet_wrap(~id) +
  background_grid()

ggplot(predictions[period == "night_time"]) +
  geom_line(aes(number_sleeping_excluding_focus, 
                p_rest,
                color = avg_dominance_sleep,
                group = interaction(avg_dominance_sleep, id_night)),
            linewidth = 1) +
  scale_color_viridis_c(name = "Dominance sleepers",
                        option = "plasma") +
  facet_wrap(~id) +
  background_grid()

ggplot(predictions[#proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1) &
  number_sleeping_excluding_focus %in% c(0, 6, 11)][period == "night_time"]) +
  geom_line(aes(avg_dominance_sleep, p_rest, group = id,
                color = dominance),
            linewidth = 1) +
  facet_wrap(~number_sleeping_excluding_focus) +
  scale_color_viridis_c(name = "Dominance\nsleepers",
                        option = "plasma") +
  background_grid()

ggplot(predictions_dominance[#proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1) &
  number_sleeping_excluding_focus %in% c(0, 6, 11)][period == "night_time"]) +
  geom_line(aes(avg_dominance_sleep, p_rest, group = dominance,
                color = dominance),
            linewidth = 1) +
  facet_wrap(~number_sleeping_excluding_focus) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  background_grid()

ggplot(predictions_dominance[#proportion_dominance_sleep_excluding_focus %in% c(0, 0.5, 1) &
  number_sleeping_excluding_focus %in% c(0, 6, 11)][period == "night_time"]) +
  geom_line(aes(dominance, p_rest, group = avg_dominance_sleep, 
                color = avg_dominance_sleep),
            linewidth = 1) +
  facet_wrap(~number_sleeping_excluding_focus) +
  scale_color_viridis_c(name = "Dominance\nsleepers",
                        option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "social", "p_rest_dominance_dominance.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 7,
       bg = "white")

# Total sleep ----
night_sleep <- ((combined_m[, c("id_night", "date_start_night", ids), with = F
][, .(sleep = apply(.SD[, ids, with = F], 2, FUN = sum),
      date_start_night = unique(date_start_night)), 
  by = id_night] |>
  cbind(combined_m[, c("id_night", ids), with = F
  ][, .(tot = apply(.SD, 2, FUN = length)), 
    by = id_night][, -c("id_night")]) |>
  cbind(id = ids) 
)[, ":="(freq_sleep = sleep / tot,
         time_sleep = sleep / (60*60),
         active = tot - sleep,
         id = ifelse(id == "Sheb", "Shebeleza", id))
][, ":="(mean_freq_sleep = sum(sleep) / sum(tot)),
  by = id
][order(id, id_night)
][, ":="(next_freq_sleep = c(tail(freq_sleep, -1), NA)), 
  by = id] |>
  merge(dominance_id) |>
  merge(weather_data |>
          rename(date_start_night = date),
        by = "date_start_night") |>
  merge(data_astronomical[, c("date", "phase", "night_duration")],
        by.x = "date_start_night",
        by.y = "date"))[, ":="(id = factor(id, levels = idsRanked))
        ][, ":="(freq_sleep.z = (freq_sleep - mean(freq_sleep, na.rm = T)) / sd(freq_sleep, na.rm = T),
                 next_freq_sleep.z = (next_freq_sleep - mean(next_freq_sleep, na.rm = T)) / sd(next_freq_sleep, na.rm = T)),
          by = id][, ":="(sleep.z = (sleep - mean(sleep)) / sd(sleep)),
                   by = list(id)]

## Stats ----
avg_night_duration <- mean(unique(night_sleep[!(id_night %in% c(1, 25))]$tot)) / (60*60)

mean(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
            by = id]$tot_sleep)

sd(night_sleep[, .(tot_sleep = sum(sleep) / (60*60*24)), 
                 by = id]$tot_sleep)

mean(night_sleep[, .(tot_active = sum(active) / (60*60*24)), 
                 by = id]$tot_active)

sd(night_sleep[, .(tot_active = sum(active) / (60*60*24)), 
                 by = id]$tot_active)

mean(night_sleep[, .(tot_freq_sleep = mean(freq_sleep)), 
                 by = id]$tot_freq_sleep)

sd(night_sleep[, .(tot_freq_sleep = mean(freq_sleep)), 
                 by = id]$tot_freq_sleep)

max(night_sleep[, sleep]) / (60*60)

min(night_sleep[!(id_night %in% c(1, 25)), sleep]) / (60*60)

model_night_duration <- lm(data = night_sleep[!(id_night %in% c(1, 25))],
                           time_sleep ~ date_start_night)
summary(model_night_duration)

# ammount of sleep by dominance
ggplot(copy(night_sleep)[, ":="(is_rain = ifelse(precipitation_mm == 0, F, T))]) +
  geom_point(aes(precipitation_mm, time_sleep), size = 3) +
  geom_smooth(aes(precipitation_mm, time_sleep), 
              method = "glm", color = "red", se = T) +
  guides(linetype = "none") +
  background_grid()

ggplot(night_sleep[!(id_night %in% c(1, 25))][, ":="(is_rain = ifelse(precipitation_mm == 0, F, T))]) +
  geom_boxplot(aes(is_rain, time_sleep)) +
  guides(linetype = "none") +
  background_grid()

ggplot(night_sleep) +
  geom_point(aes(dominance, freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, freq_sleep, 
                  group = sex,
                  lty = sex,
                  weight = tot), 
              method = "glm", color = "red", se = T,
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggplot(night_sleep) +
  geom_point(aes(dominance, mean_freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, mean_freq_sleep, 
                  group = sex,
                  lty = sex,
                  weight = tot), 
              method = "glm", color = "red", se = T,
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggplot(night_sleep[!(id_night %in% c(1, 25))]) +
  geom_point(aes(id_night, time_sleep, color = id), size = 3) +
  geom_smooth(aes(id_night, time_sleep, 
                  weight = tot), 
              method = "lm", color = "red", se = T) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggsave(here("figures", "sleep", "freq-sleep~dominance.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 5,
       bg = "white")

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "freq_sleep~dominance_night.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 5,
       bg = "white")

ggplot(night_sleep) +
  geom_point(aes(dominance, freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, freq_sleep, 
                  group = sex,
                  lty = sex,
                  weight = tot), 
              method = "glm", color = "red", se = T,
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  facet_wrap(~id_night) +
  background_grid()

ggplot(night_sleep) +
  geom_point(aes(dominance, time_sleep, color = id), size = 3) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

# night variation
ggplot(night_sleep) +
  #geom_boxplot(aes(id_night, freq_sleep, group = id_night)) +
  geom_line(aes(id_night, freq_sleep, color = dominance, group = id), linewidth = 1) +
  geom_point(aes(id_night, phase), color = "black", size = 2) +
  #geom_point(aes(id_night, mean_temp/max(mean_temp)), color = "red", size = 2) +
  geom_point(aes(id_night, as.numeric(is_rain)), color = "blue", size = 5) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  background_grid()

ggplot(night_sleep[!(id_night %in% c(1, 49))]) +
  geom_boxplot(aes(as.factor(id_night), time_sleep)) +
  geom_line(aes(as.factor(id_night), time_sleep, color = dominance, group = id), linewidth = 1) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  background_grid()

ggplot(night_sleep) +
  geom_point(aes(dominance, freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, freq_sleep, 
                  group = sex,
                  lty = sex,
                  weight = tot), 
              method = "glm", color = "red", se = T,
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggplot(night_sleep) +
  geom_point(aes(freq_sleep, next_freq_sleep, color = dominance)) +
  geom_smooth(aes(freq_sleep, next_freq_sleep, color = dominance, group = id),
              method = "lm", se = F) +
  geom_smooth(aes(freq_sleep, next_freq_sleep), linewidth =2, color = "black",
                   method = "lm", se = F) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggplot(night_sleep) + # simpsons paradox 
  geom_point(aes(freq_sleep, next_freq_sleep, color = dominance)) +
  geom_smooth(aes(freq_sleep, next_freq_sleep, color = dominance, group = id),
              method = "lm", se = F) +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(~id) +
  background_grid()

ggplot(night_sleep[!(id_night %in% c(1, 25))]) + # simpsons paradox 
  geom_point(aes(freq_sleep.z, next_freq_sleep.z, color = dominance)) +
  geom_smooth(aes(freq_sleep.z, next_freq_sleep.z, 
                  color = dominance, group = id),
              method = "lm", se = F) +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(~id) +
  background_grid()

ggplot(night_sleep[!(id_night %in% c(1, 25))][, ":="(test = next_freq_sleep.z - freq_sleep.z)]) +
  geom_histogram(aes(test)) +
  #facet_wrap(~id) +
  background_grid()

# night autocorrelation
library(tseries)
night_autocorrelation <- data.table()
for(ind in idsRanked){
  night_autocorrelation <- rbind(night_autocorrelation,
                                 data.table(acf = acf(night_sleep[order(id_night)][!(id_night %in% c(1, 25))][id == ind, freq_sleep], 
                                                      lag = 25, 
                                                      pl = F)[[1]] |> as.vector(),
                                            lag = 0:22,
                                            id = ind))
}

ggplot(night_autocorrelation) +
  geom_line(aes(lag, acf, color = id, group = id)) +
  facet_wrap(~id) +
  background_grid()

test <- data.table(time = seq(0, 100, l = 1000),
                   rest = sin(seq(0, 100, l = 1000)),
                   rest_prev = c(NA, sin(seq(0, 100, l = 1000))[-1000]),
                   diff = sin(seq(0, 100, l = 1000)) - c(sin(seq(0, 100, l = 1000))[-1], NA))

ggplot(test) +
  geom_point(aes(time, rest)) +
  geom_point(aes(time, rest_prev), color = "red")

ggplot(test) +
  geom_point(aes(rest_prev, rest))

ggplot(test) +
  geom_histogram(aes(diff))
  

# Transitions: proper markov chains stuff ----
{
  generative_data_prep <- (combined_m[, c(ids, "id_night", "dateTime"), with = F
                                      # next is to find consecutive moments to calculate transitions
  ][, ":="(diff = c(diff(dateTime), 1)) # consider last one as part of the continous behaviour
  ][, ":="(unit = rleid(diff))
  ][, ":="(is_transition = ifelse(diff == 1, T, F)) # max 6 seconds F per night to have a valid ONE SECOND RESOLUTION transition (there are some small gaps in dataset)
  ][, ":="(unit = ifelse(is_transition == F, unit - 1, unit)) 
  ][, -c("is_transition", "diff")
    # create group sleep configuration
  ][, ":="(group_configuration = apply(.SD[, ids, with = F] * 1, 
                                       1, 
                                       paste, collapse = ""))
  ][, ":="(date = date(dateTime))] |>
    left_join(data_astronomical[, c("date", "night_time", "nightEnd_time")]) |>
    as.data.table())[, ":="(period = case_when(as_hms(dateTime) <= nightEnd_time | as_hms(dateTime) >= night_time ~ "night_time",
                                               as_hms(dateTime) > nightEnd_time & as_hms(dateTime) < as_hms("12:00:00") ~ "wakey_time",
                                               as_hms(dateTime) < night_time &  as_hms(dateTime) > as_hms("12:00:00") ~ "bed_time"))
          ][, -c("date", "night_time", "nightEnd_time")]

  id <- "Trinity"
  mc_data <- rename(generative_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
    ][, ":="(date = date(dateTime))
    ][, ":="(date_start_night = min(unique(date))),
      by = id_night
    ][, -c("dateTime", "date")], 
    state = id)[, ":="(next_state = lead(state)),
                       by = list(unit, id_night)
    ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                    state == 1 & next_state == 0 ~ "p_sa",
                                    state == 0 & next_state == 0 ~ "p_aa",
                                    state == 0 & next_state == 1 ~ "p_as",
                                    T ~ NA)) # there will be 64 observations (should be the same for each id) coming from the non-continuous time series (max 6 breaks per night), not much
    ][!is.na(transition) # exclude them (in total I lose 64 seconds minute, who cares)
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
  
  for(id in ids[-1]) {
    print(id)
    mc_data <- rbind(mc_data,
                     rename(generative_data_prep[, c(id, "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
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

## Create data set with covariates ----
mc_data_covariates <- (mc_data[, ":="(id = ifelse(id == "Sheb", "Shebeleza", id))
][, ":="(number_sleeping = sapply(strsplit(group_configuration, ""), # find how many are sleeping
                                  function(x) { 
                                    sum(as.numeric(x))
                                  }))
] |>
  left_join(dominance_id) |>
  left_join(data_astronomical[, c("date", #"night_duration",
                                  "fraction",
                                  "night_duration")] |> 
              rename(date_start_night = date)) |>
  left_join(weather_data |> 
              rename(date_start_night = date)) |>
  left_join(night_sleep[, c("id", "id_night", "date_start_night", "sleep", "sleep.z")
                        ][, ":="(date_start_night = if_else(id_night == 1, 
                                                           date_start_night - days(1), 
                                                           date_start_night))] |>
              rename(previous_sleep = sleep,
                     previous_sleep.z = sleep.z)))[, ":="(sum_dominance_sleep = sapply(strsplit(group_configuration, ""), # find dominance of the sleeping individuals
                                                                                function(x) { 
                                                                                  #browser()
                                                                                  dominance_id[match(column_order, # the group configuration is not sorted by dominance!!!!
                                                                                                     id)]$dominance[!!as.numeric(x)] |> sum()
                                                                                  }))
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
                                                           period = factor(period, levels = possible_periods))
                                                           #avg_dominance_sleep = sum_dominance_sleep_excluding_focus / number_sleeping_excluding_focus),
                                                  ][, ":="(id = factor(id, levels = idsRanked))
                                                    ][order(id_night, state, period, group_configuration, transition)] |>
  merge(thresholds_data_w)

### Visualize covariate data ----
ggplot(mc_data_covariates[transition %in% c("p_as", "p_sa")]) +
  geom_point(aes(dominance, freq_transition, color = id_night, 
                 size = tot_obs), alpha = 0.1) +
  facet_wrap(~state) +
  background_grid()

ggplot(unique(mc_data_covariates[, c("phase", "date_start_night")])) +
  geom_point(aes(date_start_night, phase)) +
  background_grid()

str(mc_data_covariates)
time_slept_together <- mc_data_covariates[state == T
                   ][number_sleeping_excluding_focus == 11
                     ][, .(total_time = unique(tot_obs) |> sum()),
                                                          by = list(id_night, id)
                       ][, .(total_time = unique(total_time) / (60 * 60)),
                         by = id_night]

mean(time_slept_together$total_time)
sd(time_slept_together$total_time)


## Statistical models ----

write.csv(mc_data_covariates,
          "data/mc_data.csv")

# for statistical model in wide format
mc_data_covariates_active <- as.data.table(mc_data_covariates[state == F] |>
                                             pivot_wider(id_cols = -c("freq_transition"),
                                                         names_from = "transition",
                                                         values_from = "tot_transition",
                                                         values_fill = 0))[, ":="(freq_as = p_as / tot_obs,
                                                                                  freq_aa = p_aa / tot_obs)]

mc_data_covariates_rest <- as.data.table(mc_data_covariates[state == T] |>
                                           pivot_wider(id_cols = -c("freq_transition"),
                                                       names_from = "transition",
                                                       values_from = "tot_transition",
                                                       values_fill = 0))[, ":="(freq_sa = p_sa / tot_obs,
                                                                                freq_ss = p_ss / tot_obs)]

str(mc_data_covariates_active)
# look at correlation between covariates (number sleeping and dominance sleeping)
ggplot(mc_data_covariates_rest) +
  geom_point(aes(number_sleeping_excluding_focus, avg_dominance_sleep)) +
  ggtitle("rest") +
  facet_wrap(~id) + 
  background_grid()

ggplot(mc_data_covariates_active) +
  geom_point(aes(number_sleeping_excluding_focus, avg_dominance_sleep)) +
  ggtitle("active") +
  facet_wrap(~id)+ 
  background_grid()

ggplot(mc_data_covariates_rest) +
  geom_histogram(aes(avg_dominance_sleep)) +
  facet_wrap(~id)

### Create model ----
# Simple
m_active_simple <- glmmTMB(cbind(p_as, p_aa) ~ 
                             # random effects
                             (1|id) + (1|id_night) +
                             # inference covariates
                             dominance * 
                             (avg_dominance_sleep +
                                number_sleeping_excluding_focus) +
                             # control covariates
                             night_duration +
                             previous_sleep.z +
                             is_rain + 
                             mean_temp + 
                             fraction,
                           family = binomial,
                             data = mc_data_covariates_active)

summary(m_active_simple)
coeff_summary_active <- tidy(m_active_simple) |>
  mutate(is_significant = ifelse(p.value < 0.01, T, F),
         p.value = round(p.value, 3))
png(paste0("figures/submission/supp/methods/nighttime sleep/", threshold, "_active.png"), width = 1100)
grid.table(coeff_summary_active)
dev.off()

m_rest_simple <- glmmTMB(cbind(p_sa, p_ss) ~ 
                           # random effects
                           (1|id) + (1|id_night) +
                           # inference covariates
                           dominance * 
                           (avg_dominance_sleep +
                              number_sleeping_excluding_focus) +
                           # control covariates
                           night_duration +
                           previous_sleep.z +
                           is_rain + 
                           mean_temp + 
                           fraction,
                             family = binomial,
                             data = mc_data_covariates_rest)

summary(m_rest_simple)
coeff_summary_rest <- tidy(m_rest_simple) |>
  mutate(is_significant = ifelse(p.value < 0.01, T, F),
         p.value = round(p.value, 3))
png(paste0("figures/submission/supp/methods/nighttime sleep/", threshold, "_rest.png"), width = 1100)
grid.table(coeff_summary_rest)
dev.off()

#### Model distributions ----

chisq.test(x = null_n_simultaneously_sleeping$n_obs,
           p = null_n_simultaneously_sleeping$prob_sleep * null_n_simultaneously_sleeping$tot,
           rescale.p = T)

#### Pairwise synchrony ----

model_synchro <- lm(data = lk_network,
                    ratio ~ dominance*dominance_pair)
summary(model_synchro)

#### Time of sleep ----

model_night_duration <- glmmTMB(data = night_sleep[!(id_night %in% c(1, 25))],
                           time_sleep ~ date_start_night + (1|id))
summary(model_night_duration)

#### Hours of sleep ----

model_hours <- lm(data = to_save[state == "rest"],
           seconds_in_state ~ dominance, 
           weights = night_duration_seconds)
summary(model_hours)

#### peak time - supplementary ----

cross_correlation_model <- glmmTMB(data = cross_correlation_stats, 
                                   peak_time ~ dominance_focus + sex_focus)
summary(cross_correlation_model)





### Visualize model predictions ----
new_data <- expand.grid(id = as.factor(idsRanked), 
                        id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                        previous_sleep.z = 0,
                        period = c("night_time", "bed_time", "wakey_time"),
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
                                  #id_median_VeDBA = mean(unique(mc_data_covariates_rest$id_median_VeDBA)),
                                  previous_sleep.z = 0,
                                  period = "night_time",
                                  number_sleeping_excluding_focus = 0:11, #c(0, 6, 11),
                                  avg_dominance_sleep = seq(0, 1, l = 11),
                                  #precipitation_mm = 0:30,
                                  is_rain = c(T, F),
                                  min_temp = mean(mc_data_covariates_rest$min_temp),
                                  phase = 0,
                                  dominance = dominance_ranked) |>
  as.data.table()

predictions_rain <- as.data.table(cbind(new_data_rain, # confidence intervals need bootstrapping for random effects
                                             predict(object = m_active_simple_1, 
                                                     newdata = new_data_rain, 
                                                     type = "response",
                                                     re.form = NA,
                                                     se.fit = T) |>
                                               as.data.table() |>
                                               rename(p_as = fit,
                                                      p_as_se = se.fit),
                                             predict(object = m_rest_simple_1, 
                                                     newdata = new_data_rain, 
                                                     type = "response",
                                                     re.form = NA,
                                                     se.fit = T) |>
                                               as.data.table() |>
                                               rename(p_sa = fit,
                                                      p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                                 p_active = p_sa / (p_as + p_sa))]

ggplot(predictions_rain[avg_dominance_sleep == 0.5 & number_sleeping_excluding_focus == 9 & dominance == 0.5]) +
    geom_ribbon(aes(x = is_rain, #precipitation_mm, 
                    ymin = (p_as - p_as_se) / (p_as - p_as_se + p_sa + p_sa_se), 
                    ymax = (p_as + p_as_se) / (p_as + p_as_se + p_sa - p_sa_se)),
                color = NA, alpha = 0.2) +
    geom_line(aes(is_rain,#precipitation_mm, 
                  p_rest)) +
    scale_color_viridis_c(name = "Dominance",
                          option = "plasma") +
    scale_fill_viridis_c(name = "DOminance",
                         option = "plasma") +
    xlab("Dominance focus individual") +
    background_grid() +
    guides(fill = "none") +
    ylab(expression("Rest probability  "(p[R]))) 

ggplot(predictions_rain) +
  geom_point(aes(x = is_rain, p_rest, color = dominance),
               alpha = 0.2) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  scale_fill_viridis_c(name = "DOminance",
                       option = "plasma") +
  xlab("Dominance focus individual") +
  facet_grid(cols = vars(number_sleeping_excluding_focus),
             rows = vars(avg_dominance_sleep)) +
  background_grid() +
  guides(fill = "none") +
  ylab(expression("Rest probability  "(p[R]))) 

ggsave(here("figures", "mc_results", "social", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 4,
       bg = "white")

ggplot(predictions_rain[avg_dominance_sleep == 0.5 & number_sleeping_excluding_focus == 9 & dominance == 0.5]) +
  geom_ribbon(aes(x = precipitation_mm, 
                  ymin = (p_as - p_as_se), 
                  ymax = (p_as + p_as_se)),
              color = NA, alpha = 0.2) +
  geom_line(aes(precipitation_mm, p_as)) +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  scale_fill_viridis_c(name = "DOminance",
                       option = "plasma") +
  xlab("Dominance focus individual") +
  background_grid() +
  guides(fill = "none") +
  ylab(expression("Rest probability  "(p[R]))) 


#### Period ----

new_data_period <- expand.grid(#id = as.factor(666), 
                               #id_night = 666, 
                               previous_sleep.z = seq(-2, 2, l = 10),
                               period = possible_periods,
                               number_sleeping_excluding_focus = c(0, 5, 6, 9, 11), 
                               avg_dominance_sleep = c(0, 0.5, 0.6, 1), 
                               id_median_VeDBA = mean(unique(mc_data_covariates_rest$id_median_VeDBA)),
                               precipitation_mm = 0,
                               min_temp = mean(mc_data_covariates_rest$min_temp),
                               phase = 0,
                               dominance = dominance_ranked) |>
  as.data.table()

predictions_period_simple <- as.data.table(cbind(new_data_period, # confidence intervals need bootstrapping for random effects
                                          predict(object = m_active_simple, 
                                                  newdata = new_data_period, 
                                                  type = "response",
                                                  re.form = NA,
                                                  se.fit = T) |>
                                            as.data.table() |>
                                            rename(p_as = fit,
                                                   p_as_se = se.fit),
                                          predict(object = m_rest_simple, 
                                                  newdata = new_data_period, 
                                                  type = "response",
                                                  re.form = NA,
                                                  se.fit = T) |>
                                            as.data.table() |>
                                            rename(p_sa = fit,
                                                   p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
                                                                              p_active = p_sa / (p_as + p_sa))]

predictions_period <- as.data.table(cbind(new_data_period,
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
ggplot(predictions_period_simple[number_sleeping_excluding_focus == 6 & avg_dominance_sleep == 0.5]) +
  geom_line(aes(previous_sleep.z, p_rest, 
                color = dominance, group = dominance),
            linewidth = 1) +
  facet_wrap(~period) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggplot(predictions_period_simple[number_sleeping_excluding_focus == 6 & avg_dominance_sleep == 0.5][, ":="(test = p_rest - previous_sleep.z)]) +
  geom_histogram(aes(test)) +
  background_grid()

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

new_data_sleep <- expand.grid(#id = 666, 
                              #id_night = 666, # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                              previous_sleep.z = seq(-2, 2, by = 0.1),
                              period = c("night_time"),
                              number_sleeping_excluding_focus = c(0, 6, 11), #c(0, 6, 11),
                              avg_dominance_sleep = c(0, 0.5, 1), #
                              id_median_VeDBA = mean(unique(mc_data_covariates_rest$id_median_VeDBA)),
                              precipitation_mm = 0,
                              min_temp = mean(mc_data_covariates_rest$min_temp),
                              phase = 0,
                              dominance = dominance_ranked) |>
  as.data.table()

predictions_sleep <- as.data.table(cbind(new_data_sleep, # confidence intervals need bootstrapping for random effects
                                         predict(object = m_active_simple, 
                                                 newdata = new_data_sleep, 
                                                 type = "response",
                                                 re.form = NA,
                                                 se.fit = T) |>
                                           as.data.table() |>
                                           rename(p_as = fit,
                                                  p_as_se = se.fit),
                                         predict(object = m_rest_simple, 
                                                 newdata = new_data_sleep, 
                                                 type = "response",
                                                 re.form = NA,
                                                 se.fit = T) |>
                                           as.data.table() |>
                                           rename(p_sa = fit,
                                                  p_sa_se = se.fit)))[, ":="(p_rest = p_as / (p_as + p_sa),
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
new_data_dominance <- expand.grid(#id = as.factor(666), 
                                  #id_night = as.factor(666), # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                                  previous_sleep.z = 0,
                                  #period = c("night_time"),
                                  number_sleeping_excluding_focus = 0:11, #c(0, 6, 11),
                                  #id_median_VeDBA = mean(unique(mc_data_covariates_rest$id_median_VeDBA)),
                                  avg_dominance_sleep = seq(0, 1, l = 11),
                                  is_rain = F,
                                  #min_temp = mean(mc_data_covariates_rest$min_temp),
                                  night_duration = avg_night_duration * (60*60) ,
                                  fraction = 0.5,
                                  mean_temp = mean(unique(mc_data_covariates_rest$mean_temp), 
                                                   weights = unique(mc_data_covariates_rest$tot_obs)),
                                  dominance = dominance_ranked) |> 
  as.data.table()

predictions_dominance <- as.data.table(cbind(new_data_dominance, # confidence intervals need bootstrapping for random effects
                                             predict(object = m_active_simple, 
                                                     newdata = new_data_dominance, 
                                                     type = "response",
                                                     re.form = NA,
                                                     se.fit = T) |>
                                               as.data.table() |>
                                               rename(p_as = fit,
                                                      p_as_se = se.fit),
                                             predict(object = m_rest_simple, 
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

### Model checks ----

residuals_predictions_rest <- as.data.table(cbind(mc_data_covariates_rest, 
                                                  predict_sa = predict(object = m_rest_simple, 
                                                                       newdata = mc_data_covariates_rest, 
                                                                       type = "response"))
                                            )[, ":="(residual = predict_sa - freq_sa)]

residuals_predictions_active <- as.data.table(cbind(mc_data_covariates_active, 
                                                    predict_as = predict(object = m_active_simple, 
                                                                         newdata = mc_data_covariates_active, 
                                                                         type = "response"))
                                              )[, ":="(residual = predict_as - freq_as)]

model_check <- rbind(mc_data_covariates_rest[, -c("p_sa", "p_ss")] |>
                       rename(freq_transition = freq_sa,
                              freq_remain = freq_ss),
                     mc_data_covariates_active[, -c("p_as", "p_aa")] |>
                       rename(freq_transition = freq_as,
                              freq_remain = freq_aa)) 

model_check_predictions_simple <- as.data.table(cbind(model_check, 
                                               predict_transition_active = predict(object = m_active_simple, 
                                                                                   newdata = model_check, 
                                                                                   type = "response"), 
                                               predict_transition_rest = predict(object = m_rest_simple, 
                                                                                 newdata = model_check, 
                                                                                 type = "response"))
)[, ":="(id = factor(id, levels = idsRanked),
         residual = ifelse(state == T, 
                           predict_transition_rest - freq_transition,
                           predict_transition_active - freq_transition),
         predicted_sleep =  predict_transition_active / (predict_transition_active + predict_transition_rest))
] |>
  merge(dominance_id)

#### Check data vs predicted ----

copy(model_check_predictions)[, ":="(total = sum(tot_obs)),
                              by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs),
      tot_obs = sum(tot_obs)), 
  by = list(id, id_night, dominance)] |>
  ggplot() +
  geom_point(aes(id_night, real_sleep, color = dominance, group = id)) +
  geom_line(aes(id_night, predicted_sleep, color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  ylab(expression("Rest probability  "(p[R]))) +
  facet_wrap(~id) +
  xlab("Night id") +
  background_grid()

# first graph 
copy(model_check_predictions)[, ":="(total = sum(tot_obs)),
                              by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs),
      tot_obs = sum(tot_obs)), 
  by = list(id, id_night, dominance)] |>
  ggplot() +
  geom_point(aes(id_night, real_sleep, color = dominance, group = id)) +
  geom_line(aes(id_night, predicted_sleep, color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma",
                        name = "Dominance") +
  ylab(expression("Rest probability  "(p[R]))) +
  facet_wrap(~id) +
  xlab("Night id") +
  background_grid()

copy(model_check_predictions)[, ":="(total = sum(tot_obs, na.rm = T)),
                              by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T),
      tot_obs = sum(tot_obs)), 
  by = list(id, id_night, dominance)][, ":="(residual = predicted_sleep - real_sleep)] |>
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
  facet_wrap(~id) +
  background_grid()

# Second figure
# First graph predictions
copy(model_check_predictions)[, ":="(total = sum(tot_obs, na.rm = T)),
                              by = list(id, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T)), 
  by = list(id, dominance)] |>
  ggplot() +
  geom_line(aes(dominance, predicted_sleep),
            linewidth = 1) +
  geom_point(aes(dominance, real_sleep, 
                 color = id), size = 5) +
  scale_color_viridis_d(option = "turbo") +
  xlab("Dominance") +
  ylab(expression("Rest probability  "(p[R]))) +
  background_grid()

# First graph residuals
copy(model_check_predictions)[, ":="(total = sum(tot_obs, na.rm = T)),
                              by = list(id, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T)), 
  by = list(id, dominance)][, ":="(residual = predicted_sleep - real_sleep)] |>
  ggplot() +
  geom_point(aes(dominance, residual, 
                 color = id), size = 5) +
  geom_segment(aes(y = rep(0, 12), yend = residual, 
                   x = dominance, xend = dominance,
                   color = id), linewidth = 2, linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linewidth = 2, 
             linetype = "dashed", color = "darkgrey") +
  scale_color_viridis_d(option = "turbo") +
  xlab("Dominance") +
  ylab("Residual") +
  background_grid()

# Second graph predictions
copy(model_check_predictions)[, ":="(cutted = cut(previous_sleep.z, 
                                                            breaks = 7, 
                                                            labels = F))
                              ][, ":="(previous_sleep.z = mean(previous_sleep.z)),
                                by = cutted
                                ][, ":="(total = sum(tot_obs, na.rm = T)),
                              by = list(state, period, dominance, previous_sleep.z)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T),
      tot_obs = sum(tot_obs)), 
  by = list(period, dominance, previous_sleep.z)
  ][, ":="(period = factor(period, levels = c("bed_time", "night_time", "wakey_time")))] |>
  ggplot() +
  geom_point(aes(previous_sleep.z, real_sleep, 
                 color = dominance, size = tot_obs)) +
  geom_line(aes(previous_sleep.z, predicted_sleep,
                color = dominance, group = dominance),
            linewidth = 1) +
  facet_wrap(~period,
             labeller = labeller(period = c("bed_time" = "Twilight",
                                            "night_time" = "Night",
                                            "wakey_time" = "Dawn")
             )) +
  ylab(expression("Rest probability  "(p[R]))) +
  xlab("Rest in previous night (standardized)") +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  background_grid()

# Second graph residuals
copy(model_check_predictions)[, ":="(cutted = cut(previous_sleep.z, 
                                                  breaks = 7, 
                                                  labels = F))
][, ":="(previous_sleep.z = mean(previous_sleep.z)),
  by = cutted
][, ":="(total = sum(tot_obs, na.rm = T)),
  by = list(state, period, dominance, previous_sleep.z)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs, na.rm = T),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs, na.rm = T),
      tot_obs = sum(tot_obs)), 
  by = list(period, dominance, previous_sleep.z)
][,":="(residual = predicted_sleep - real_sleep)][, ":="(period = factor(period, levels = c("bed_time", "night_time", "wakey_time")))] |>
  ggplot() +
  geom_point(aes(previous_sleep.z, residual,
                 color = dominance, size = tot_obs)) +
  geom_line(aes(previous_sleep.z, residual, 
                 color = dominance, group = dominance)) +
  # geom_segment(aes(y = rep(0, 181), yend = residual, 
  #                  x = previous_sleep.z, xend = previous_sleep.z,
  #                  color = dominance), linewidth = 2, linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linewidth = 2, 
             linetype = "dashed", color = "darkgrey") +
  facet_wrap(~period,
             labeller = labeller(period = c("bed_time" = "Twilight",
                                            "night_time" = "Night",
                                            "wakey_time" = "Dawn")
             )) +
  ylab(expression("Rest probability  "(p[R]))) +
  xlab("Rest in previous night (standardized)") +
  scale_color_viridis_c(name = "Dominance",
                        option = "plasma") +
  background_grid()

# number of individuals graph
ggplot(model_check_predictions[, .(freq_transition = weighted.mean(freq_transition, tot_obs, na.rm = T),
                                   predict_transition_active = weighted.mean(predict_transition_active, tot_obs, na.rm = T),
                       tot_obs = sum(tot_obs)),
                                  by = list(period, state, id, number_sleeping_excluding_focus)
][period == "night_time"][state == F]) +
  geom_line(aes(number_sleeping_excluding_focus, predict_transition_active, 
                color = id)) +
  geom_point(aes(number_sleeping_excluding_focus, freq_transition, 
                 color = id, size = tot_obs)) +
  # geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
  #               color = avg_dominance_sleep), lty = "dashed",
  #           linewidth = 1) +
  facet_wrap(~id) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

ggplot(model_check_predictions[, .(freq_transition = weighted.mean(freq_transition, tot_obs, na.rm = T),
                                   predict_transition_rest = weighted.mean(predict_transition_rest, tot_obs, na.rm = T),
                                   tot_obs = sum(tot_obs)),
                               by = list(period, state, id, number_sleeping_excluding_focus)
][period == "night_time"][state == T]) +
  geom_line(aes(number_sleeping_excluding_focus, predict_transition_rest, 
                color = id)) +
  geom_point(aes(number_sleeping_excluding_focus, freq_transition, 
                 color = id, size = tot_obs)) +
  # geom_line(aes(number_sleeping_excluding_focus, p_sa, group = avg_dominance_sleep, 
  #               color = avg_dominance_sleep), lty = "dashed",
  #           linewidth = 1) +
  facet_wrap(~id) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()


#### Visualize residuals ----

# check correlation true and fitted
ggplot(residuals_predictions_rest) +
  geom_point(aes(freq_sa, predict_sa, size = tot_obs), alpha = 0.2) +
  geom_smooth(aes(freq_sa, predict_sa, weight = tot_obs),
              method = "lm", color = "red", linewidth = 1) +
  background_grid()

ggplot(residuals_predictions_active) +
  geom_point(aes(freq_as, predict_as, size = tot_obs), alpha = 0.2) +
  geom_smooth(aes(freq_as, predict_as, weight = tot_obs),
              method = "lm", color = "red", linewidth = 1) +
  background_grid()

# check residual
ggplot(residuals_predictions_rest) +
  geom_point(aes(number_sleeping_excluding_focus, residual, 
                 size = tot_obs, color = id), 
             alpha = 0.2) +
  geom_smooth(aes(number_sleeping_excluding_focus, residual, weight = tot_obs, color = id),
              method = "gam", linewidth = 1) +
  facet_wrap(~id_night) +
  background_grid()

#### Look at distribution of lengths of behavior ----

generative_data_prep_check <- generative_data_prep[order(dateTime)
][, paste0("continuous_", names(generative_data_prep)[1:12]) := lapply(.SD, FUN = rleid),
  .SDcols = names(generative_data_prep)[1:12],
  by = "unit"
][, paste0("length_", names(generative_data_prep)[1:12]) := lapply(.SD, FUN = function(x) {
  rep(rle(x)[[1]],
      times = rle(x)[[1]])}),
.SDcols =  paste0("continuous_", names(generative_data_prep)[1:12]),
by = "unit"
][, -paste0("continuous_", names(generative_data_prep)[1:12]),
  with = F]

generative_data_prep_check_s <- (mapply(
  lapply(generative_data_prep_check[, names(generative_data_prep)[1:12],
                                    with = F],
         FUN = function(x) {
           rle(x)[[1]] |> 
             as.data.table() |> 
             rename(duration = V1)
         }),
  lapply(generative_data_prep_check[, names(generative_data_prep)[1:12],
                                    with = F],
         FUN = function(x) {
           rle(x)[[2]] |> 
             as.data.table() |> 
             rename(state = V1)
         }), 
  FUN = function(x, y) {
    cbind(x, y)[, .(frequency = .N), 
                by = list(state, duration)]
  }, 
  SIMPLIFY = F) |> 
  rbindlist(idcol = "id"))[, ":="(id = ifelse(id == "Sheb", "Shebeleza", id))]
  
ggplot(generative_data_prep_check_s[state == T]) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  facet_wrap(~id) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()


#### Check steady state assumption ----

ggsave(here("figures", "mc_results", "overview", "p_rest_byid.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 7,
       width = 20,
       bg = "white")
  
copy(model_check_predictions)[, ":="(total = sum(tot_obs)),
                              by = list(id, id_night, state)
][, .(predicted_sleep = weighted.mean(predicted_sleep, tot_obs),
      real_sleep = unique(total[which(state == T)]) / sum(tot_obs)), 
  by = list(id, id_night, dominance)] |>
  ggplot() +
  geom_boxplot(aes(id_night, real_sleep, group = id_night)) +
  geom_point(aes(id_night, real_sleep, color = dominance, group = id)) +
  geom_line(aes(id_night, predicted_sleep, color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma") +
  background_grid()

ggsave(here("figures", "mc_results", "overview", "p_rest.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 4,
       width = 8,
       bg = "white")


# Save data ----
to_save <- mc_data_covariates[, c("id_night", "date_start_night", "id", "dominance", "state", "transition", "tot_transition", "tot_obs", "period", "group_configuration")
][, ":="(night_duration_seconds = sum(tot_transition),
         state_2 = ifelse(state == T, "rest", "active"),
         transition = case_when(transition == "p_as" ~ "active_to_rest",
                                transition == "p_aa" ~ "active_to_active",
                                transition == "p_sa" ~ "rest_to_active",
                                transition == "p_ss" ~ "rest_to_rest")),
  by = list(id_night, date_start_night, id, dominance)
][, ":="(seconds_in_state = sum(tot_transition)),
  by = list(id_night, date_start_night, id, dominance, state)
][, -c("state")
][, ":="(state = state_2)
][, -c("state_2")
][, .(night_duration_seconds = unique(night_duration_seconds),
      seconds_in_state = unique(seconds_in_state),
      number_of_transitions = sum(tot_transition)),
  by = list(id_night, date_start_night, id, dominance, state, transition)
][order(date_start_night, id, state, transition)
][, ":="(id_night = rleid(id_night)),
  by = id
][, ":="(probability_transition = number_of_transitions / seconds_in_state,
         probability_state = seconds_in_state / night_duration_seconds)
][, c("id_night", "date_start_night", "id", "dominance", 
      "state", "seconds_in_state", "probability_state", "night_duration_seconds",
      "transition", "number_of_transitions", "probability_transition")]

write.csv(to_save,
          file = "data/night_info.csv",
          row.names = F)

to_save_wide <-  pivot_wider(to_save, 
                             names_from = "state",
                             id_expand = F,
                             values_from = c("seconds_in_state", "probability_state")) |>
  group_by(id, id_night) |>
  mutate(seconds_in_state_active = unique(na.omit(seconds_in_state_active)),
         seconds_in_state_rest = unique(na.omit(seconds_in_state_rest)),
         probability_state_active = unique(na.omit(probability_state_active)),
         probability_state_rest = unique(na.omit(probability_state_rest))) |>
  pivot_wider(names_from = "transition",
              values_from = c("number_of_transitions", "probability_transition")) 

write.csv(to_save_wide,
          file = "data/night_info_wide_format.csv",
          row.names = F)

ggplot(to_save[state == "rest"]) +
  geom_point(aes(dominance, probability_state, color = date_start_night)) +
  geom_smooth(aes(dominance, probability_state),
              method = "lm") +
  background_grid()

ggplot(to_save[state == "rest"]) +
  geom_point(aes(dominance, probability_rest_to_active, color = date_start_night)) +
  geom_smooth(aes(dominance, probability_rest_to_active),
              method = "lm") +
  scale_color_viridis_c(option = "magma") +
  background_grid()

ggplot(to_save) +
  geom_point(aes(dominance, probability_active_to_rest, color = date_start_night)) +
  geom_smooth(aes(dominance, probability_active_to_rest),
              method = "lm") +
  background_grid()

ggplot(to_save) +
  geom_density(aes(probability_rest, color = dominance, group = id)) +
  background_grid()


write.csv(to_save,
          file = "data/night_info.csv",
          row.names = F)

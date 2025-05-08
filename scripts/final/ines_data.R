# astronomical info

data_astronomical <- as.data.table(getSunlightTimes(
  date = seq.Date(as.Date("2018-07-24"), as.Date("2018-10-17"), by = 1),
  keep =  c("solarNoon", "sunrise", "sunset", #"sunriseEnd", "sunsetStart",
            #"dawn", "dusk", "nauticalDawn", "nauticalDusk", 
            "nightEnd", "night"),
  lat = -33.918861,
  lon = 18.423300,
  tz = "Africa/Johannesburg"
))[, ":="(sunrise_time = as_hms(sunrise),
          sunset_time = as_hms(sunset),
          # nauticalDawn_time = as_hms(nauticalDawn),
          # nauticalDusk_time = as_hms(nauticalDusk),
          night_time = as_hms(night),
          nightEnd_time = as_hms(nightEnd))
][, ":="(next_sunrise_time = c(tail(sunrise_time, -1), as_hms(NA)))
][, ":="(night_duration = as_hms(difftime(next_sunrise_time, sunset_time)))] |>
  merge(getMoonIllumination(date = seq.Date(as.Date("2018-07-24"), 
                                            as.Date("2018-10-17"), 
                                            by = 1),
                            keep = c("phase")),
        by = "date")

for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)),
         fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                      "Baboon behaviour from acceleration (1)",
                                                                      file))))[, ":="(Date = date(Timestamp),
                                                                                      Timestamp = with_tz(Timestamp,
                                                                                                          tz = "Africa/Johannesburg"))]) # change by reference
  
  if(file == "Trinitytrim2021.rds") {
    foragingTrinity[,  ":="(Timestamp = Timestamp + hours(1))]
    print("Doone")
  }
}
env <- .GlobalEnv
allForagingFileNames <- grep("foraging", ls(), value = TRUE)


ggplot(foragingTrinity) +
  geom_tile(aes(as_hms(Timestamp),
                Date,
                fill = mean.VeDBAs)) +
  geom_path(data = data_astronomical[date >= ymd("2018-07-29") &
                                        date <= ymd("2018-10-30")
                                     ][, c("date", "sunrise", "sunset", "night", "nightEnd")] |>
              pivot_longer(cols = c("sunrise", "sunset", "night", "nightEnd"),
                           names_to = "event",
                           values_to = "time"),
            aes(as_hms(time), date,
                color = event),
            linewidth = 1.5) +
  scale_fill_viridis_c() +
  ylab("date") +
  xlab("time")

env <- .GlobalEnv
new_allForagingFileNames <- sample(allForagingFileNames)
for(obj in new_allForagingFileNames) {
  print(obj)
  env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
                   rename(dateTime = Timestamp) |>
                   merge(data_astronomical[, c("sunrise", "sunset", 
                                               "night", "nightEnd",
                                               "date")],
                         all.x = TRUE,
                         by.x = "Date",
                         by.y = "date"))[, #":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
                                         ":="(is_night = if_else(dateTime < nightEnd | dateTime > night, T, F))
                         ][, ":="(inactive = if_else(mean.VeDBAs < 0.04, T, F))]
}

combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "inactive")] |>
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  
  id_name <- str_replace(obj, "foraging", "")
  combined <- env[[obj]][is_night == T][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined, by = "dateTime", all = TRUE)
}

comb_m <- copy(combined)[, ":="(date = date(dateTime), # by refrence
                              time = as_hms(dateTime))
][, ":="(is_early = ifelse(time > as_hms("12:00:00"), T, F)) # thing to identify night identity
][order(dateTime)
][, ":="(id_night = rleid(is_early))
][, ":="(id_night_new = ifelse(!(id_night %% 2), 
                           id_night + 1, 
                           id_night))
][, ":="(id_night = rleid(id_night_new))
  ][, -c("is_early", "id_night_new")
][, ":="(date_start_night = min(unique(date))),
  by = id_night
  ][, c(ids, "id_night", "dateTime", "date_start_night"), with = F]

{
  id <- "Trinity"
  mc_data <- (comb_m[, c(id, "dateTime", "id_night", "date_start_night"), with = F
                         ] |>
                rename(state = id))[!is.na(state)
                           ][order(dateTime)
                             ][, ":="(diff = c(diff(dateTime), 1)) # consider last one as part of the continous behaviour
  ][, ":="(unit = rleid(diff))
  ][, ":="(is_transition = ifelse(diff == 1, T, F)) # max 6 seconds F per night to have a valid ONE SECOND RESOLUTION transition (there are some small gaps in dataset)
  ][, ":="(unit = ifelse(is_transition == F, unit - 1, unit)) 
  ][, -c("is_transition", "diff")
    # create group sleep configuration
  ][, ":="(next_state = lead(state)),
              by = unit
  ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                  state == 1 & next_state == 0 ~ "p_sa",
                                  state == 0 & next_state == 0 ~ "p_aa",
                                  state == 0 & next_state == 1 ~ "p_as",
                                  T ~ NA)) # there will be 64 observations (should be the same for each id) coming from the non-continuous time series (max 6 breaks per night), not much
  ][!is.na(transition) # exclude them (in total I lose 64 seconds minute, who cares)
  ][, ":="(tot_state = .N),
    by = list(state, id_night)
  ][, ":="(tot_obs = .N),
    by = list(state, id_night) # here should be put all the covariates
  ][, .(tot_obs = unique(tot_obs),
        tot_transition = .N,
        freq_transition = .N / unique(tot_obs)),
    by = list(state, id_night, date_start_night, transition) # here should be put all the covariates and transition
  ][, ":="(id = id)]
  
  for(id in ids[-1]) {
    print(id)
    mc_data <- rbind(mc_data,
                     (comb_m[, c(id, "dateTime", "id_night", "date_start_night"), with = F
                     ] |>
                       rename(state = id))[!is.na(state)
                       ][order(dateTime)
                       ][, ":="(diff = c(diff(dateTime), 1)) # consider last one as part of the continous behaviour
                       ][, ":="(unit = rleid(diff))
                       ][, ":="(is_transition = ifelse(diff == 1, T, F)) # max 6 seconds F per night to have a valid ONE SECOND RESOLUTION transition (there are some small gaps in dataset)
                       ][, ":="(unit = ifelse(is_transition == F, unit - 1, unit)) 
                       ][, -c("is_transition", "diff")
                         # create group sleep configuration
                       ][, ":="(next_state = lead(state)),
                         by = unit
                       ][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                                       state == 1 & next_state == 0 ~ "p_sa",
                                                       state == 0 & next_state == 0 ~ "p_aa",
                                                       state == 0 & next_state == 1 ~ "p_as",
                                                       T ~ NA)) # there will be 64 observations (should be the same for each id) coming from the non-continuous time series (max 6 breaks per night), not much
                       ][!is.na(transition) # exclude them (in total I lose 64 seconds minute, who cares)
                       ][, ":="(tot_state = .N),
                         by = list(state, id_night)
                       ][, ":="(tot_obs = .N),
                         by = list(state, id_night) # here should be put all the covariates
                       ][, .(tot_obs = unique(tot_obs),
                             tot_transition = .N,
                             freq_transition = .N / unique(tot_obs)),
                         by = list(state, id_night, date_start_night, transition) # here should be put all the covariates and transition
                       ][, ":="(id = id)])
  }
}

to_save <- mc_data[, c("id_night", "date_start_night", "id",
                       "state", "transition", "tot_transition", "tot_obs")
][, ":="(id = ifelse(id == "Sheb", "Shebeleza", id),
         night_duration_seconds = sum(tot_transition),
         state_2 = ifelse(state == T, "rest", "active"),
         transition = case_when(transition == "p_as" ~ "active_to_rest",
                                transition == "p_aa" ~ "active_to_active",
                                transition == "p_sa" ~ "rest_to_active",
                                transition == "p_ss" ~ "rest_to_rest")),
  by = list(id_night, date_start_night, id)
][, ":="(seconds_in_state = sum(tot_transition)),
  by = list(id_night, date_start_night, id, state)
][, -c("state")
][, ":="(state = state_2)
][, -c("state_2")
][, .(night_duration_seconds = unique(night_duration_seconds),
      seconds_in_state = unique(seconds_in_state),
      number_of_transitions = sum(tot_transition)),
  by = list(id_night, date_start_night, id, state, transition)
][order(date_start_night, id, state, transition)
][, ":="(probability_transition = number_of_transitions / seconds_in_state,
         probability_state = seconds_in_state / night_duration_seconds)
][, c("id_night", "date_start_night", "id", 
      "state", "seconds_in_state", "probability_state", "night_duration_seconds",
      "transition", "number_of_transitions", "probability_transition")] |>
  merge(all_id[, -c("accelerometer", "gps")])

ggplot(to_save[state == "rest"]) +
  geom_point(aes(dominance, probability_state, color = id_night)) +
  geom_smooth(aes(dominance, probability_state),
              method = "lm", se = F) +
  background_grid()

test <- lm(data = to_save[state == "rest"],
   seconds_in_state ~ dominance, weights = night_duration_seconds)
summary(test)

ggplot(to_save[state == "rest"]) +
  geom_point(aes(dominance, seconds_in_state, color = id_night)) +
  geom_smooth(aes(dominance, seconds_in_state),
              method = "lm", se = F) +
  background_grid()

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
              values_from = c("number_of_transitions", "probability_transition")) |>
  arrange(id_night, dominance, desc(sex)) |>
  left_join(data_astronomical[, c("date", "fraction")] |>
              rename(date_start_night = date))  |>
  left_join(weather_data[-1,] |> # remove fist row, the night from 31-1 which is code as starting in 1 becuase no date of 31
              rename(date_start_night = date)) 

write.csv(to_save_wide,
          file = "data/night_info_wide_format.csv",
          row.names = F)

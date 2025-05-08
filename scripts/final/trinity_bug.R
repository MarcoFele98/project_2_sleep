assign(paste0("foraging", sub("trim2021.rds.*", "", "Azul")),
       fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                    "Baboon behaviour from acceleration (1)",
                                                                    "Azultrim2021.rds."))))[, Date := date(Timestamp)]) # change by reference


test <- as.data.table(readRDS(here::here("data", # fill each second
                   "Baboon behaviour from acceleration (1)",
                   "Trinitytrim2021.rds")))[order(Timestamp)][, ":="(Timestamp_2 = Timestamp + hours(1))]

test_2 <- as.data.table(readRDS(here::here("data", # fill each second
                                         "Baboon behaviour from acceleration (1)",
                                         "Azultrim2021.rds")))[order(Timestamp)]

# Trinity bug
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)),
         fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                      "Baboon behaviour from acceleration (1)",
                                                                      file))))[, Date := date(Timestamp)]) # change by reference
}
env <- .GlobalEnv
allForagingFileNames <- grep("foraging", ls(), value = TRUE)

bug_data <- copy(foragingAzul)[, ":="(foragingAzul = ifelse(mean.VeDBAs < 0.03, T, F))][, c("foragingAzul", "Timestamp")]
  
for(data in allForagingFileNames[-1]) {
  print(data)
  bug_data <- merge(bug_data,
                    (copy(env[[data]])[, ":="(culo = ifelse(mean.VeDBAs < 0.03, T, F))] |>
                      setnames(old = "culo", new = data))[, c(data, "Timestamp"), with = F], 
                    by = "Timestamp", all = T)
}

bug_data_m <- (copy(bug_data)[, ":="(n_individuals = apply(.SD[, allForagingFileNames, with = F], 1, sum))
                             ][!is.na(n_individuals)][, ":="(date = date(Timestamp))] |>
  merge(data_astronomical[, c("sunrise", "sunset", "date")],
        by = "date",
        all.y = F))[, ":="(is_night = ifelse(Timestamp < sunrise | Timestamp > sunset, T, F))
                    ][is_night == T]


id_behvaiour <- bug_data_m[, allForagingFileNames, with = F]
result_syncrony <- data.frame()
for(i in allForagingFileNames) {
  print(i)
  for(j in allForagingFileNames) {
    print(j)
    result_syncrony <- rbind(result_syncrony, 
                             data.frame(id = i, 
                                        id_pair = j) |>
                               cbind((id_behvaiour[, ..i] + id_behvaiour[, ..j])[, .(tot = .N), 
                                                                                 by = eval(i)] |>
                                       setnames(old = i, 
                                                new = "syncrony")))
  }
}

ggplot(as.data.table(result_syncrony)[syncrony == 1])  +
  geom_point(aes(id, tot)) +
  geom_smooth(aes(id, tot, group = id_pair), 
              method = "lm") +
  facet_wrap(~id_pair) +
  background_grid()















table(combined_m[, "Trinity"] + combined_m[, "Azul"]) |> prop.table()
table(combined_m[, "Trinity"] + combined_m[, "Patch"]) |> prop.table()
table(combined_m[, "Trinity"] + combined_m[, "Kangela"]) |> prop.table()

table(combined_m[, "Azul"] + combined_m[, "Patch"]) |> prop.table()
table(combined_m[, "Azul"] + combined_m[, "Kangela"]) |> prop.table()
table(combined_m[, "Kangela"] + combined_m[, "Patch"]) |> prop.table()
table(combined_m[, "Lola"] + combined_m[, "Kym"]) |> prop.table()
table(combined_m[, "Kangela"] + combined_m[, "Hanson"]) |> prop.table()


(test <- ggplot(combined_night_m[, c("Trinity", "date", "dateTime")][, ":="(time = as_hms(dateTime))]) +
  geom_tile(aes(time, date, fill = Trinity)))

ggsave(test, 
       filename = here("figures", "test", "Trinity.png"),
       height = 30,
       width = 30,
       bg = "white")

(test <- ggplot(combined_night_m[, c("Trinity", "date", "dateTime")][, ":="(time = as_hms(dateTime))]) +
    geom_tile(aes(time, date, fill = Trinity)))

ggsave(test, 
       filename = here("figures", "test", "Trinity.png"),
       height = 7,
       width = 10,
       bg = "white")

(test2 <- ggplot(combined_night_m[, c("Azul", "date", "dateTime")][, ":="(time = as_hms(dateTime))]) +
    geom_tile(aes(time, date, fill = Azul)))

ggsave(test2, 
       filename = here("figures", "test", "Azul.png"),
       height = 7,
       width = 10,
       bg = "white")

(test3 <- ggplot(combined_night_m[, c("Azul", "Trinity", "date", "dateTime")][, ":="(diff = Azul + Trinity,
                                                                                     time = as_hms(dateTime))]) +
    geom_tile(aes(time, date, fill = as.factor(diff))))

ggsave(test3, 
       filename = here("figures", "test", "diff1.png"),
       height = 7,
       width = 10,
       bg = "white")

(test4 <- ggplot(combined_night_m[, c("Azul", "Hanson", "date", "dateTime")][, ":="(diff = Azul + Hanson,
                                                                                    time = as_hms(dateTime))]) +
    geom_tile(aes(time, date, fill = as.factor(diff))))

ggsave(test4, 
       filename = here("figures", "test", "diff2.png"),
       height = 7,
       width = 10,
       bg = "white")

(test5 <- ggplot(combined_night_m[, c("Sheb", "Hanson", "date", "dateTime")][, ":="(diff = Sheb + Hanson,
                                                                                    time = as_hms(dateTime))]) +
    geom_tile(aes(time, date, fill = as.factor(diff))))

ggsave(test5, 
       filename = here("figures", "test", "diff3.png"),
       height = 7,
       width = 10,
       bg = "white")


# for daytime
id_behvaiour_day <- combined_night_m[order(dateTime)][is_night == T][, ids, with = F]
result_syncrony_day <- data.frame()
for(i in 1:length(idsRanked)) {
  print(i)
  for(j in 1:length(idsRanked)) {
    print(j)
    result_syncrony_day <- rbind(result_syncrony_day, 
                                 data.frame(id = colnames(id_behvaiour_day)[i], 
                                            id_pair = colnames(id_behvaiour_day)[j]) |>
                                   cbind(
                                     setnames(cbind(id_behvaiour_day[, ..i] + id_behvaiour_day[, ..j],
                                           combined_night_m[order(dateTime)][is_night == T][, "id_nightday"]),
                                           old = colnames(id_behvaiour_day)[i], 
                                           new = "syncrony")[, .(tot = .N), 
                                       by = list(id_nightday, syncrony)]
                                   ))
  }
} 

ggplot(as.data.table(result_syncrony_day)[syncrony == 1])  +
  geom_point(aes(id, tot, color = as.factor(id_nightday))) +
  geom_smooth(aes(id, tot, group = id_pair), 
              method = "lm") +
  facet_wrap(~id_pair) +
  background_grid()



assign(paste0("foraging", sub("trim2021.rds.*", "", "Trinitytrim2021.rds")),
       fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                    "Baboon behaviour from acceleration (1)",
                                                                    file))))) # change by reference


assign(paste0("foraging", sub("trim2021.rds.*", "", "Azultrim2021.rds")),
       fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                    "Baboon behaviour from acceleration (1)",
                                                                    "Azultrim2021.rds")))[, Timestamp := ymd_hms(Timestamp)
                                                                    ])[, Date := date(Timestamp)]) # change by reference



assign(paste0("foraging", sub("trim2021.rds.*", "", "Shebtrim2021.rds")),
       fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                    #"Baboon behaviour from acceleration (1)",
                                                                    "Shebtrim2021.rds")))[, Timestamp := ymd_hms(Timestamp)
                                                                    ])[, Date := date(Timestamp)]) # change by reference


assign(paste0("foraging", sub("trim2021.rds.*", "", "Trinitytrim2021.rds")),
       fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
                                                                    #"Baboon behaviour from acceleration (1)",
                                                                    "Trinitytrim2021.rds"))))[, Date := date(Timestamp)])



readRDS(here::here("data", # fill each second
                   "Baboon behaviour from acceleration (1)",
                   "Trinitytrim2021.rds"))

# create new correct files
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  if(file == "Trinitytrim2021.rds") {
    new_version <- readRDS(here::here("data", # fill each second
                                      #"Baboon behaviour from acceleration (1)",
                                      file)) |>
      mutate(date_time_UTC = with_tz(Timestamp,
                                     tz = "UTC"),
             date_UTC = date(date_time_UTC),
             time_UTC = as_hms(date_time_UTC),
             date_time_South_Africa = with_tz(date_time_UTC,
                                              tz = "Africa/Johannesburg"),
             date_South_Africa = date(date_time_South_Africa),
             time_South_Africa = as_hms(date_time_South_Africa)) |>
      select(-c("Timestamp", "SAtime", "SAdate", "Date", "Time_dd.hh.mm.ss.sss"))
    
  } else {
    new_version <- readRDS(here::here("data", # fill each second
                                      #"Baboon behaviour from acceleration (1)",
                                      file)) |>
      mutate(date_time_UTC = Timestamp,
             date_UTC = date(Timestamp),
             time_UTC = as_hms(Timestamp),
             date_time_South_Africa = with_tz(Timestamp,
                                              tz = "Africa/Johannesburg"),
             date_South_Africa = date(date_time_South_Africa),
             time_South_Africa = as_hms(date_time_South_Africa)) |>
      select(-c("Timestamp", "SAtime", "SAdate", "Date", "Time_dd.hh.mm.ss.sss"))
  }
  
  file_name <- sub("trim2021.rds.*", "", file)
  saveRDS(new_version, file = paste0("data/baboon_accelerometers_2024/", file_name, "trim2024.rds"))
}

new_version <- readRDS("data/baboon_accelerometers_2024/Trinitytrim2024.rds")
        
test <- rename(generative_data_prep[, c("Trinity", "unit", "id_night", "period", "dateTime", "group_configuration"), with = F
][, ":="(date = date(dateTime))
][, ":="(date_start_night = min(unique(date))),
  by = id_night
][, -c("dateTime", "date")], 
state = "Trinity"
)[, ":="(next_state = lead(state)), # could use lead(state)
            by = list(unit, id_night)
][, ":="(transition = case_when(state == 1 & next_state == 1 ~ "p_ss",
                                state == 1 & next_state == 0 ~ "p_sa",
                                state == 0 & next_state == 0 ~ "p_aa",
                                state == 0 & next_state == 1 ~ "p_as",
                                T ~ NA))
][!is.na(transition) # exclude 64 observations (should be the same for each id) coming from the non-continuous time series (max 6 breaks per night), not much
][, ":="(number_state = .N),
  by = list(state, id_night)
][, ":="(tot_obs = .N),
  by = list(state, id_night, period, group_configuration) # here should be put all the covariates
][, .(number_state = unique(number_state),
      tot_obs = unique(tot_obs),
      tot_transition = .N,
      freq_transition = .N / unique(tot_obs),
      date_start_night = unique(date_start_night)),
  by = list(state, id_night, period, group_configuration, transition) # here should be put all the covariates and transition
][, ":="(length_night = sum(tot_transition)),
  by = id_night][, ":="(id = "Trinity")
][, c("id", "id_night", "state", "period", "group_configuration", "transition", 
      "tot_obs", "tot_transition", "number_state", "length_night", 
      "freq_transition", "date_start_night")
][order(id_night, state, period, group_configuration, transition)]


test2 <- test[, .(number_state = unique(number_state),
                  length_night = unique(length_night)),
              by = list(date_start_night, state)]


correct <- (combined_m[, c("id_night", "date_start_night", ids), with = F
][, .(number_rest = apply(.SD[, ids, with = F], 2, FUN = sum),
      date_start_night = unique(date_start_night)), 
  by = id_night] |>
  cbind(combined_m[, c("id_night", ids), with = F
  ][, .(length_night = apply(.SD, 2, FUN = length)), 
    by = id_night][, -c("id_night")]) |>
  cbind(id = ids) 
)[, ":="(         number_active = length_night - number_rest,
                  probability_rest = number_rest / length_night,
                  time_rest = number_rest / (60*60),
                  id = ifelse(id == "Sheb", "Shebeleza", id))
][, c("id", "date_start_night", "probability_rest", "number_rest", "number_active", "length_night")]



ts <- data.table(Timestamp = seq.POSIXt(ymd_hms("2001-09-01 00:00:01"), 
                                    ymd_hms("2001-10-01 00:07:00"), 
                                    by = "sec") |>
                   ymd_hms())


data_with_missing_times <- full_join(df, original_data)



# data for Ines
astro_data <- (as.data.table(getSunlightTimes(
  date = seq.Date(as.Date("2018-07-24"), as.Date("2018-10-17"), by = 1),
  keep =  c("sunrise", "sunset",
            "dawn", "dusk", 
            "nightEnd", "night"),
  lat = -33.918861,
  lon = 18.423300,
  tz = "Africa/Johannesburg"
))[, ":="(sunrise_time = as_hms(sunrise),
          sunset_time = as_hms(sunset),
          dawn_time = as_hms(dawn),
          dusk_time = as_hms(dusk),
          night_time = as_hms(night),
          nightEnd_time = as_hms(nightEnd))
] |>
  merge(getMoonIllumination(date = seq.Date(as.Date("2018-07-24"), 
                                            as.Date("2018-10-17"), 
                                            by = 1),
                            keep = c("phase")),
        by = "date") |>
  rename(moon_phase = phase))

write.csv(astro_data,
          file = "figures/test/astro_south_africa.csv",
          row.names = F)

test <- data.table()
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
test <- rbind(test,
              as.data.table(readRDS(here::here("data", # fill each second
                                               file)))[, ":="(Timestamp = ymd_hms(Timestamp),
                                                              time_timestamp = as_hms(Timestamp),
                                                              time_SA = as_hms(SAtime),
                                                              date = date(Timestamp))
                                 ][date %in% c(ymd("2018-08-13"),
                                               ymd("2018-08-14"),
                                               ymd("2018-08-15"),
                                               ymd("2018-08-16"),
                                               ymd("2018-08-17"),                                           
                                               ymd("2018-08-18"),
                                               ymd("2018-08-19"),
                                               ymd("2018-08-20"),
                                               ymd("2018-08-21"),
                                               ymd("2018-08-22"),                                         
                                               ymd("2018-08-23"),                                      
                                               ymd("2018-08-24"))])
}

test_plot <- ggplot(test[time_SA > as_hms("05:30:00") &
                           time_SA < as_hms("08:30:00")
][, ":="(is_trinity = ifelse(ID == "Trinity", T, F))]) +
  geom_line(aes(time_SA, 
                mean.VeDBA, 
                group = ID,
                color = is_trinity)) +
  facet_wrap(~date) +
  ggtitle("Time from last column (South Africa time) - I think incorrect for Trinity") +
  background_grid()

ggsave(plot = test_plot,
       filename = here("figures", "test", "morning_Vedba_South_Africa_time.png"),
       height = 30,
       width = 30,
       bg = "white")

test_plot_2 <- ggplot(test[as_hms(Time_dd.hh.mm.ss.sss) > as_hms("03:30:00") &
                             as_hms(Time_dd.hh.mm.ss.sss) < as_hms("06:30:00")
][, ":="(is_trinity = ifelse(ID == "Trinity", T, F))]) +
  geom_line(aes(as_hms(Time_dd.hh.mm.ss.sss), 
                mean.VeDBA, 
                group = ID,
                color = is_trinity)) +
  ggtitle("Time from second column (UTC) - I think correct for all individuals (but in UTC rather than South Africa time)") +
  facet_wrap(~date) +
  background_grid()

ggsave(plot = test_plot_2,
       filename = here("figures", "test", "morning_Vedba_second_column_time.png"),
       height = 30,
       width = 30,
       bg = "white")


# Load datasets

# load(here("data", "sensitivity analysis", "data_t0.05.RData"))
load(here("data", "submission", "data_0.04.RData"))
# load(here("data", "sensitivity analysis", "data_t0.03.RData"))

#threshold <- 0.05 # 0.04

# Get astronomical and environmental info ----
data_astronomical <- (as.data.table(getSunlightTimes(
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
][, ":="(next_night_end_time = c(tail(nightEnd_time, -1), as_hms(NA)),
         duration_first_night_part = difftime(as_hms("24:00:00"), night_time, units = "secs"))
][, ":="(night_duration = duration_first_night_part + nightEnd_time)] |>
  merge(getMoonIllumination(date = seq.Date(as.Date("2018-07-24"), 
                                            as.Date("2018-10-17"), 
                                            by = 1),
                            keep = c("phase", "fraction")),
        by = "date")) # transform phase to indicate night light

ggplot(data_astronomical) +
  #geom_point(aes(date, phase)) +
  geom_line(aes(date, fraction), color = "red") 
  #geom_line(aes(date, night_duration), color = "red")



#weather_data_1 <- as.data.table(read_excel(here("data", "weather.xlsx")))[, ":="(date = date(date))]
temperature_data_import <- as.data.table(read_excel(here("data", "Marco Fele.xlsx"), sheet = "Temperature"))
str(temperature_data_import)
temperature_data <- as.data.table(temperature_data_import[complete.cases(temperature_data_import),
                                    ][LEGEND != "DD"
                                      ][, 1:25 # exclude dayly summaries
                                        ][, ":="(day_id = as.numeric(LEGEND))
                                        ][, ":="(end_block = ifelse(day_id - lead(day_id) > 0, T, F))
                                          ][, ":="(block_id = rleid(end_block))
                                            ][, ":="(block_id = ifelse(end_block == T | is.na(end_block), 
                                                                        block_id - 1, block_id))
                                              ][, ":="(block_id = rleid(block_id))
                                                ][, -c("LEGEND", "end_block")] |>
  pivot_longer(cols = 1:24,
               names_to = "time_of_day_id",
               values_to = "temperature"))[, ":="(time_of_day_id = 1:(.N)),
                                           by = list(block_id, day_id)
                                           ][, ":="(time_of_day_id = as.integer(time_of_day_id)),
                                           ][, ":="(temperature = ifelse(temperature == "-", NA, temperature))
                                             ][, ":="(temperature = as.numeric(temperature))
                                               ][block_id == 5
                                                 ] |>
  merge(data.table(day_id = 1:31,
                   date = seq(ymd("2018-08-01"),
                       ymd("2018-08-31"),
                       by = "days"))) |>
  merge(data.table(time_of_day_id = 1:24,
                   time = seq(as.POSIXct("2012-01-01 00:00:00", tz = "Africa/Johannesburg"),
                             as.POSIXct("2012-01-01 23:00:00", tz = "Africa/Johannesburg"),
                             by = "hour") |>
                     as_hms()),
        by = "time_of_day_id")
setkey(temperature_data, NULL)

rain_data_import <- as.data.table(read_excel(here("data", "Marco Fele.xlsx"), sheet = "Rain"))
str(temperature_data_import)

rain_data <- as.data.table(rain_data_import[LEGEND %in% 1:31
][, 1:25 # exclude dayly summaries
][, ":="(day_id = as.numeric(LEGEND))
][, ":="(end_block = ifelse(day_id - lead(day_id) > 0, T, F))
][, ":="(block_id = rleid(end_block))
][, ":="(block_id = ifelse(end_block == T | is.na(end_block), 
                           block_id - 1, block_id))
][, ":="(block_id = rleid(block_id))
][, -c("LEGEND", "end_block")] |>
  pivot_longer(cols = 1:24,
               names_to = "time_of_day_id",
               values_to = "rain"))[, ":="(time_of_day_id = 1:(.N)),
                                           by = list(block_id, day_id)
               ][, ":="(time_of_day_id = as.integer(time_of_day_id)),
               ][, ":="(rain = ifelse(is.na(rain), 0, rain))
               ][, ":="(rain = ifelse(rain == "-", NA, rain))
               ][, ":="(rain = as.numeric(rain))
               ][block_id == 5
               ] |>
  merge(data.table(day_id = 1:31,
                   date = seq(ymd("2018-08-01"),
                             ymd("2018-08-31"),
                             by = "days"))) |>
  merge(data.table(time_of_day_id = 1:24,
                   time = seq(as.POSIXct("2012-01-01 00:00:00", tz = "Africa/Johannesburg"),
                              as.POSIXct("2012-01-01 23:00:00", tz = "Africa/Johannesburg"),
                              by = "hour") |>
                     as_hms()),
        by = "time_of_day_id")
setkey(rain_data, NULL)
str(rain_data)

weather_data <- (merge(rain_data, temperature_data) |>
  merge(data_astronomical[, c("date", "night_time", "nightEnd_time")])
  )[, ":="(is_night = if_else(time < nightEnd_time | time > night_time, T, F))
    ][order(date, time)
  ][, ":="(id_night = rleid(is_night))
    ][is_night == T
      ][, .(date = min(date),
            mean_temp = mean(temperature),
            min_temp = min(temperature),
            precipitation_mm = sum(rain)),
      by = id_night][, -c("id_night")
                     ][, ":="(is_rain = ifelse(precipitation_mm == 0, F, T))]

ggplot(weather_data) +
  geom_point(aes(date, as.numeric(is_rain)), color = "blue") +
  geom_line(aes(date, precipitation_mm)) 


# create  datasets ----
## For mean.VeBDAs ----
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
  
### Plot ----
# allForagingFileNames <- c("foragingAzul",      "foragingGabrielle", "foragingHanson",    "foragingKangela",   "foragingKym",      
#  "foragingLola" ,     "foragingLuna" ,     "foragingNelly" ,    "foragingNikNak" ,   "foragingPatch"  ,  
# "foragingSheb"  ,    "foragingTrinity") this is the correct order
allForagingFileNames_nice <- c("F17", "F6", "F4", "M1", "F8", "F16", "F7", "F11", "F9", "F21", "M2", "F12")
for(i in 1:length(allForagingFileNames)) {
  obj <- allForagingFileNames[i]
  print(obj)
  p1 <- ggplot(env[[obj]]) +
    geom_histogram(aes(x = mean.VeDBAs)) +
    geom_vline(aes(xintercept = 0.05), linewidth = 1, color = "red") +
    geom_vline(aes(xintercept = 0.03), linewidth = 1, color = "red") +
    geom_vline(aes(xintercept = 0.04), linewidth = 1, color = "red") +
    scale_x_log10(limits = c(0.01, 4.1)) +
    ggtitle(allForagingFileNames_nice[i])+
    background_grid()

  ggsave(plot = p1,
         filename = here("figures", "mc_results", "sensisitivty", "manuscript_1", "supp", "threshold distribution", paste0(obj, ".png")),
         height = 3,
         width = 10,
         bg = "white")
}

# Modify datasets ----
## For mean.VeBDAs ----
env <- .GlobalEnv
for(obj in allForagingFileNames) {
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
                                           ][, ":="(inactive = if_else(mean.VeDBAs < threshold, T, F))]
}

test <- foragingAzul[is.na(mean.VeDBAs)]

ggplot(test) +
  geom_histogram(aes(as_hms(dateTime))) +
  scale_x_time(limits = c(as_hms("01:00:00"), 
                          as_hms("04:00:00")))

ggplot(test) +
  geom_histogram(aes(dateTime))

### Create data set for knowing average movement tendencies of the individuals ----
thresholds_data <- (env[[allForagingFileNames[1]]] |>
                      rename(id = ID,
                             date = Date))[, ":="(is_early = ifelse(time < as_hms("12:00:00"), T, F),  # thing to identify night identity
                                                  date = ymd(date))
                             ][order(dateTime)
                             ][, ":="(id_night = rleid(is_night, is_early))
                             ][, ":="(id_night = ifelse(is_early == T & is_night == T, 
                                                        id_night - 1, 
                                                        id_night))
                             ][, -c("is_early")
                             ][, ":="(date_start_night = min(unique(date))), ### ATTENTION! NOW THE DATE IS THE DATE OF START NIGHT
                               by = id_night
                             ][, ":="(id_median_VeDBA = median(mean.VeDBAs, na.rm = T),
                                      id_mean_VeDBA = mean(mean.VeDBAs, na.rm = T)),
                             ][, .(id_median_VeDBA = unique(id_median_VeDBA),
                                   id_mean_VeDBA = unique(id_mean_VeDBA),
                                   date_median_VeDBA = median(mean.VeDBAs, na.rm = T),
                                   date_mean_VeDBA = mean(mean.VeDBAs, na.rm = T),
                                   date_total_VeDBA = sum(mean.VeDBAs, na.rm = T)),
                               by = list(id, date_start_night, is_night)
                               ][!is.na(id)]

test <- (env[[allForagingFileNames[1]]] |>
           rename(id = ID,
                  date = Date))[, ":="(is_early = ifelse(time < as_hms("12:00:00"), T, F),  # thing to identify night identity
                                       date = ymd(date))
                  ][order(dateTime)
                  ][, ":="(id_night = rleid(is_night, is_early))
                  ][, ":="(id_night = ifelse(is_early == T & is_night == T, 
                                             id_night - 1, 
                                             id_night))
                  ][, -c("is_early")
                  ][, ":="(date_start_night = min(unique(date))), ### ATTENTION! NOW THE DATE IS THE DATE OF START NIGHT
                    by = id_night
                  ]

ggplot(unique(test[, c("id_night", "date_start_night")])) +
  geom_point(aes(id_night, date_start_night))

# check that this is correct
# ggplot(thresholds_data[date_start_night  %in% ymd(c("2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11"))]) +
#   geom_tile(aes(time, date, fill = as.factor(is_early)))
# 
# ggplot(thresholds_data[date %in% ymd(c("2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11"))]) +
#   geom_tile(aes(time, date, fill = as.factor(is_night)))
# 
# ggplot(thresholds_data[date %in% ymd(c("2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11"))]) +
#   geom_tile(aes(time, date, fill = as.factor(id_night)))
#   
for(obj in allForagingFileNames[-1]) {
  print(obj)
  thresholds_data <- rbind(thresholds_data,
                           (env[[obj]] |>
                                 rename(id = ID,
                                        date = Date))[, ":="(is_early = ifelse(time < as_hms("12:00:00"), T, F),  # thing to identify night identity
                                                             date = ymd(date))
                                        ][order(dateTime)
                                        ][, ":="(id_night = rleid(is_night, is_early))
                                        ][, ":="(id_night = ifelse(is_early == T & is_night == T, 
                                                                   id_night - 1, 
                                                                   id_night))
                                        ][, -c("is_early")
                                        ][, ":="(date_start_night = min(unique(date))), ### ATTENTION! NOW THE DATE IS THE DATE OF START NIGHT
                                          by = id_night
                                        ][, ":="(id_median_VeDBA = median(mean.VeDBAs, na.rm = T),
                                                 id_mean_VeDBA = mean(mean.VeDBAs, na.rm = T)),
                                        ][, .(id_median_VeDBA = unique(id_median_VeDBA),
                                              id_mean_VeDBA = unique(id_mean_VeDBA),
                                              date_median_VeDBA = median(mean.VeDBAs, na.rm = T),
                                              date_mean_VeDBA = mean(mean.VeDBAs, na.rm = T),
                                              date_total_VeDBA = sum(mean.VeDBAs, na.rm = T)),
                                          by = list(id, date_start_night, is_night)
                                          ][!is.na(id)])
}

thresholds_data_w <- thresholds_data[!is.na(date_start_night)
                                     ][, ":="(is_night = ifelse(is_night == T, "night", "day"))] |>
  pivot_wider(values_from = c("date_median_VeDBA", "date_mean_VeDBA", "date_total_VeDBA"),
              names_from = "is_night",
              values_fill = NA)

# Filter and combine datasets ----
## For random forest and mean.VeBDAs ----
env <- .GlobalEnv
combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "inactive")] |>
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  
  id_name <- str_replace(obj, "foraging", "")
  combined <- env[[obj]][is_night == T][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined, by = "dateTime", all = TRUE)
}
rm(env)

str(foragingAzul)

# Final dataset ----
combined_m <- copy(combined)[, ":="(number_individuals = apply(.SD[, -c("dateTime")], 1, 
                                                               function(x) {sum(!is.na(x))}),
                                    number_sleeping = apply(.SD[, -c("dateTime")], 1, 
                                                            sum, na.rm = T))
][number_individuals == 12
][, ":="(number_awake = 12 - number_sleeping,
         date = date(dateTime),
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
  by = id_night]
            
# check this is correct
ggplot(combined_m[date %in% ymd(c("2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11"))]) +
  geom_tile(aes(time, date, fill = as.factor(id_night)))


# Null model infos stuff ----
data_n_awake <- combined_m[, ":="(tot = .N)
][, .(freq_number_awake = .N / unique(tot)), 
  by = number_awake
][, ":="(check = sum(freq_number_awake),
         avg = sum(freq_number_awake * number_awake))]

data_n_sleeping <- combined_m[, ":="(tot = .N) # find how many times n individuals are sleeping
][, .(freq_number_sleeping = .N / unique(tot)), 
  by = number_sleeping
][, ":="(check = sum(freq_number_sleeping),
         avg = sum(freq_number_sleeping * number_sleeping))]

# Compare with daytime ----
env <- .GlobalEnv # filter only for the days I found before were all 13 individuals

combined_night <- env[[allForagingFileNames[1]]][Date %in% unique(combined_m[number_individuals == 12][["date"]])][, c("dateTime", "inactive")] |> # I also need the day here
  setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  id_name <- str_replace(obj, "foraging", "")
  combined_night <- env[[obj]][Date %in% unique(combined_m[number_individuals == 12][["date"]])][, c("dateTime", "inactive")] |>
    setnames(old = "inactive", new = id_name) |>
    merge(combined_night, by = "dateTime", all = TRUE)
}
rm(env)

# Modify and filter
combined_night_m <- (copy(combined_night)[, ":="(number_individuals = apply(.SD[, -c("dateTime")], 1, 
                                                                            function(x) {
                                                                              sum(!is.na(x))
                                                                            }),
                                                 number_sleeping = apply(.SD[, -c("dateTime")], 1, 
                                                                         sum, na.rm = T),
                                                 avg_dominance = apply((.SD |> rename(Shebeleza = Sheb))[, dominance_id$id, with = F], 1, 
                                                                       function(x) {
                                                                         mean(dominance_id$dominance[x], na.rm = T)
                                                                       }))
][, ":="(simpsons_diversity = 1 - number_sleeping * (number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)) + 
           (number_individuals - number_sleeping) * (number_individuals - number_sleeping - 1) / 
           (number_individuals * (number_individuals - 1)),
         freq_sleeping = number_sleeping / number_individuals)
][, ":="(date = date(dateTime),
         time = as_hms(dateTime))
][, ":="(avg_dom_norm = (avg_dominance - min(avg_dominance, na.rm = T)) / (max(avg_dominance, na.rm = T) - min(avg_dominance, na.rm = T))), 
  by = freq_sleeping] |>
  merge(data_astronomical[, c("sunrise", "sunset", "date")],
        by = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
        ][order(dateTime)
          ][, ":="(id_nightday = rleid(is_night))
        ][number_individuals == 12]

env <- .GlobalEnv
data_GPS <- data.table(id = character(),
                       date_time_South_Africa = as.POSIXct(character(), 
                                                           tz = "Africa/Johannesburg"),
                       date = as.Date(x = integer(0), origin = "1970-01-01"),
                       night_10_min = as.POSIXct(character(), 
                                                           tz = "Africa/Johannesburg"),
                       lon = numeric(),
                       lat = numeric())
for(file in list.files("data/gps")) {
  name <- sub("_2024_mf.RData", "", file)
  name <- sub("gps_", "", name)
  
  print(name)
  
  if(name %in% idsRanked) {
    load(paste0("data/gps/", file))
    
    data_GPS <- rbind(data_GPS,
                      (data.table(date_time_South_Africa = seq(as.POSIXct("2018-08-02 00:00:00", tz = "Africa/Johannesburg"),
                                                               as.POSIXct("2018-08-28 00:00:00", tz = "Africa/Johannesburg"),
                                                               by = "sec")) |>
                         left_join(
                           (env[[sub(".RData", "", file)]] |>
                              as.data.table()
                           )[, ":="(time = as_hms(date_time_South_Africa),
                                    date = date(date_time_South_Africa))]
                         )  |>
                         merge(data_astronomical[, c("date", "night")])
                      )[, ":="(night_10_min = night - min(10),
                               night_hour = night - hours(2))
                      ][date_time_South_Africa >= night_hour
                        ][, c("date_time_South_Africa", "date", "night_10_min", "id", "lon", "lat")])
    
    rm(file)
  }
}

for(focus_date in as.character(unique(data_GPS$date))) {
  
p <- ggplot(data_GPS[date == ymd(focus_date)
                     ][date_time_South_Africa >= night_10_min
] |> merge(dominance_id)) +
  geom_path(aes(lon, lat, color = dominance, group = id)) +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(~date) +
  ggspatial::annotation_scale() +
  background_grid()
  
  ggsave(plot = p,
         filename = paste0("figures/sleep_location/dominance_", focus_date, ".png"),
         height = 10,
         width = 10,
         bg = "white")
}

data_GPS_m <- data_GPS[date_time_South_Africa >= night_10_min] |>
  filter(!is.na(lon) & !is.na(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), dim = "XY") |>
  sf::st_set_crs(32733) |>
  sf::st_transform(crs = 3857) |>
  as.data.frame() |>
  extract(geometry,
          c('Longitude', 'Latitude'),
          '\\((.*), (.*)\\)',
          convert = TRUE) |>
  mutate(time = as_hms(date_time_South_Africa))

write.csv(data_GPS_m,
          file = "data/maps/last_ten_mins.csv")

data_GPS_s <- copy(data_GPS)[, ":="(displacement = (lead(lat)-lat)^2 + (lead(lon)-lon)^2,
                                    one_min = format(as.POSIXct(date_time_South_Africa), 
                                                     format = "%H:%M")),
                             by = list(date, id)
][, ":="(min_id = rleid(one_min)),
  by = list(date, id)
][, ":="(ten_min_id = min_id %/% 10)
  ][, .(mean_disp = mean(displacement, na.rm = T)),
  by = list(date, id, min_id)]

ggplot(data_GPS_s[date == ymd("2018-08-24")]) +
  geom_point(aes(ten_min_id, mean_disp, color = id)) +
  facet_wrap(~date)


for(focus_date in as.character(unique(data_GPS$date))) {
  print(focus_date)
  
  p <- ggplot(data_GPS_m[date == ymd(focus_date)]) +
    geom_line(aes(min_id, mean_disp, color = id)) +
    facet_wrap(~date)
  
  ggsave(plot = p,
         filename = paste0("figures/sleep_location/10_min_msd_", focus_date, ".png"),
         height = 10,
         width = 10,
         bg = "white")
}


### Accelerometers
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

env <- .GlobalEnv
for(obj in allForagingFileNames) {
  print(obj)
  env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
                   rename(dateTime = Timestamp) |>
                   merge(data_astronomical[, c("night", 
                                               "date")],
                         all.x = TRUE,
                         by.x = "Date",
                         by.y = "date")
                 )[, ":="(is_night = if_else(dateTime < (night + hours(2)) & dateTime > (night - hours(2)), T, F))]
}

env <- .GlobalEnv
combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "mean.VeDBAs")] |>
  setnames(old = "mean.VeDBAs", new = str_replace(allForagingFileNames[1], "foraging", ""))
for(obj in allForagingFileNames[-1]) {
  print(obj)
  
  id_name <- str_replace(obj, "foraging", "")
  combined <- env[[obj]][is_night == T][, c("dateTime", "mean.VeDBAs")] |>
    setnames(old = "mean.VeDBAs", new = id_name) |>
    merge(combined, by = "dateTime", all = TRUE)
}
rm(env)

combined_f <- copy(combined)[, ":="(number_individuals = apply(.SD[, idsRanked_wrong_names, with = F], 1, 
                                                               function(x) {sum(!is.na(x))}),
                                    date = date(dateTime),
                                    time = as_hms(dateTime))
][number_individuals == 12
][, c(paste0(idsRanked_wrong_names, "_avg")) := lapply(.SD[, idsRanked_wrong_names, with = F], 
                                                       FUN = zoo::rollmean,
                                                       k = 121,
                                                       na.pad = T),
  by = date] |>
  merge(data_astronomical[, c("night_time", 
                              "date")],
        by = "date")

for(focus_date in as.character(unique(combined_f$date))) {
  print(focus_date)
  
  p <- ggplot(combined_f[date == ymd(focus_date)] |>
           pivot_longer(cols = paste0(idsRanked_wrong_names, "_avg"),
                        names_to = "id",
                        values_to = "vedba")) +
    geom_line(aes(time, vedba, color = id)) +
    scale_color_viridis_d(option = "turbo") +
    geom_vline(aes(xintercept = night_time),
               color = "black", linewidth = 1)  +
    geom_hline(aes(yintercept = 0.3),
               color = "red", linewidth = 1) +
    ggtitle(focus_date)
  
  ggsave(plot = p,
         filename = paste0("figures/sleep_location/movements_", focus_date, ".png"),
         height = 10,
         width = 10,
         bg = "white")
}

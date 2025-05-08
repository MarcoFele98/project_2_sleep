## Compare with daytime ----
### Combine datasets ----
env <- .GlobalEnv
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

### Modify and filter ----
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
        ][, ":="(id_nightday = rleid(is_night))
        ][number_individuals == 12]



plot <- ggplot(copy(combined_night_m)[, ":="(time = (as.numeric(seconds(time)) + 12 * 60 * 60) %% (24 * 60 * 60)
)][, ":="(date = if_else(time > 12 * 60 * 60, 
                         date - days(1), 
                         date))
]) +
  geom_tile(aes(time, date, fill = avg_dom_norm)) +
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

ggsave(plot = plot,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "avg_dominance_norm~time.png"), # "freq-sleep~dominance_INT_sex.png"
       height = 30,
       width = 30,
       bg = "white")

plot <- ggplot(combined_night_m) +
  #geom_line(aes(time, avg_dominance, color = date)) +
  geom_smooth(aes(time, avg_dom_norm)) +
  facet_wrap(~date) +
  guides(color = "none")

ggsave(plot = plot,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "avg_dominance_norm_gam~time_series_by_day.png"), # "freq-sleep~dominance_INT_sex.png"
       height = 40,
       width = 40,
       bg = "white")

plot <- ggplot(combined_night_m) +
  geom_line(aes(time, freq_sleeping, color = date)) +
  guides(color = "none")

ggsave(plot = plot,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "freq_sleping~time_series.png"), # "freq-sleep~dominance_INT_sex.png"
       height = 30,
       width = 30,
       bg = "white")

plot <- ggplot(combined_night_m) +
  geom_point(aes(freq_sleeping, avg_dominance), 
             alpha = 0.1) +
  geom_smooth(aes(freq_sleeping, avg_dominance, 
                  group = is_night),
              method = "lm") +
  facet_wrap(~is_night) +
  background_grid()

ggsave(plot = plot,
       filename = here("figures", "mean.VeBDAs threshold 0.03", "freq_sleping~avg_dominance.png"), # "freq-sleep~dominance_INT_sex.png"
       height = 30,
       width = 30,
       bg = "white")

### Compare night day
day_inactive <- copy(dominance_id)[, ":="(sleep = apply((combined_night_m[is_night == F][, ids, with = F] |>
                                                           rename(Shebeleza = Sheb))[, idsRanked, with = F],
                                                        2, FUN = sum),
                                          tot = apply((combined_night_m[is_night == F][, ids, with = F] |>
                                                         rename(Shebeleza = Sheb))[, idsRanked, with = F],
                                                      2, FUN = length))][, ":="(freq_sleep = sleep / tot,
                                                                                time_sleep = sleep / 60)]

ggplot(day_inactive) +
  geom_point(aes(dominance, freq_sleep, color = id), size = 3) +
  geom_smooth(aes(dominance, freq_sleep, 
                  #group = sex,
                  #lty = sex,
                  weight = tot ), 
              method = "glm", color = "red",
              method.args = list(family = "binomial")) +
  scale_color_viridis_d(option = "turbo",
                        name = "") +
  guides(linetype = "none") +
  background_grid()

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "freq_sleep~dominance_day.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 3,
       width = 5,
       bg = "white")

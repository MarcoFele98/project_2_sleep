# Part 1: figures ----

# modify this dataset
behaviours_inActive_m_splitted <- data.table()
for(i in 1:length(unique(behaviours_inActive_m$sunrise_time))) { 
  print(i/length(unique(behaviours_inActive_m$sunrise_time)))
  behaviours_inActive_m_splitted <- rbind(behaviours_inActive_m_splitted,
                                          split_behaviours(behaviours_inActive_m[sunrise_time == unique(sleep_ammount$sunrise_time)[i]],
                                                           date_time = quote(Timestamp),
                                                           start_time_col = quote(time_start), 
                                                           start_time_col_2 = time_start,
                                                           end_time_col = quote(time_end), 
                                                           end_time_col_2 = time_end,
                                                           split = unique(sleep_ammount$sunrise_time)[i]) |>
                                            split_behaviours(date_time = quote(Timestamp),
                                                             start_time_col = quote(time_start), 
                                                             start_time_col_2 = time_start,
                                                             end_time_col = quote(time_end), 
                                                             end_time_col_2 = time_end,
                                                             split = unique(sleep_ammount$sunset_time)[i]))
}
# identify night period
sleep_ammount_m <- sleep_ammount_m[, ":="(is_night = if_else(ref_time_start < sunrise | ref_time_start > sunset, T, F))
][order(date_start, ref_time_start) # both bc start is wrong when splitted
][, ":="(number_night = rleid(is_night))
][, ":="(first_night = min(number_night)), 
  by = id][number_night != first_night]


# plot results
ggplot(behaviours_inActive_m[date %in% c(ymd("2018-07-30"):ymd("2018-08-10"))]) +
  geom_step(aes(refTime, number_inactive, group = date)) +
  facet_wrap(~date)


# modify group data and filter
behaviours_inActive_m2 <- merge(
  data.table(
    start = behaviours_inActive_m[, .SD[-.N, c("Timestamp")]],
    end = behaviours_inActive_m[-1, c("Timestamp")],
    behaviours_inActive_m[, .SD[-.N, c("number_inactive")]], 
    behaviours_inActive_m[-1, c("number_individuals")],
    behaviours_inActive_m[, .SD[-.N, c("simpson_diversity")]]
  )[, ":="(date_start = date(start.Timestamp),
           date_end = date(end.Timestamp),
           ref_time_start = as_hms(start.Timestamp),
           ref_time_end = as_hms(end.Timestamp))
  ][c(1:.N, which(date_start != date_end)) # split different days 
  ][order(start.Timestamp)
  ][, ":="(duplicated = ifelse(date_start != date_end, T, F)) 
  ][, ":="(first_duplicated = rleid(duplicated))
  ][,  ":="(number = seq_len(.N)), by = first_duplicated
  ][duplicated & number == 1, ":="(ref_time_end = as_hms("23:59:59"),
                                   end.Timestamp = ymd_hms(paste(date_start, "23:59:59", sep="-")))
  ][duplicated & number == 2, ":="(ref_time_start = as_hms("00:00:00"),
                                   date_start = date_end,
                                   start.Timestamp = ymd_hms(paste(date_end, "00:00:00", sep="-")))
  ][, ":="(repeated_start = rleid(start.Timestamp)) # eliminate repated start 
  ][, .SD[.N], by = repeated_start # takes 30 seconds
  ][, !c("duplicated", "number", "first_duplicated", "repeated_start")
  ][, ":="(proportion = number_inactive / max(number_inactive)), 
    by = date_start], # 7 minutes)
  data_astronomical, 
  by.x = "date_start", by.y = "date")

# plot results
plot1 <- split_behaviours(behaviours_inActive_m2,#[behaviours_inActive_m2[, .I[max(number_inactive) >= 10], 
                          #                        by = date_start]$V1], 
                          date_time = quote(start.Timestamp),
                          start_time_col = quote(ref_time_start), 
                          start_time_col_2 = ref_time_start,
                          end_time_col = quote(ref_time_end), 
                          end_time_col_2 = ref_time_end,
                          split = as_hms("12:00:00"))

plot1 <- plot1[ref_time_end == as_hms("12:00:00"), 
               ":="(ref_time_start = as_hms("11:59:59"))
][ref_time_end == as_hms("12:00:00"), 
  ":="(ref_time_end = as_hms("11:59:59"))
][, ":="(start_plot = (as.numeric(seconds(ref_time_start)) + 12 * 60 * 60) %% (24 * 60 * 60),
         end_plot = (as.numeric(seconds(ref_time_end)) + 12 * 60 * 60) %% (24 * 60 * 60),
         sunrise_time_plot = (as.numeric(seconds(sunrise_time)) + 12 * 60 * 60) %% (24 * 60 * 60),
         sunset_time_plot = (as.numeric(seconds(sunset_time)) + 12 * 60 * 60) %% (24 * 60 * 60),
         night_time_plot = (as.numeric(seconds(night_time)) + 12 * 60 * 60) %% (24 * 60 * 60),
         nightEnd_time_plot = (as.numeric(seconds(nightEnd_time)) + 12 * 60 * 60 ) %% (24 * 60 * 60)
)][, ":="(date_start = if_else(start_plot >= 0 &
                                 start_plot < 12 * 60 * 60, 
                               date_start - days(1), # minus bc in the plot in am inverting the scales
                               date_start))
][, ":="(date_start_plot = as.numeric(as.factor(date_start)))
][, ":="(date_start_plot = max(date_start_plot) - date_start_plot)
][!(date_start_plot %in% c(0:3, max(plot1$date_start_plot, max(plot1$date_start_plot) - 1)))] # filter what I want to see

# proportion
p1 <- ggplot(plot1) + 
  geom_segment(aes(y = date_start_plot, yend = date_start_plot,
                   x = start_plot, xend = end_plot,
                   color = proportion),
               linewidth = 10) +
  geom_line(aes(sunrise_time_plot, date_start_plot), linewidth = 5, color = "orange") + #, lty = "dashed"
  geom_line(aes(sunset_time_plot, date_start_plot), linewidth = 5, color = "darkred") + #, lty = "dashed"
  geom_line(aes(night_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  geom_line(aes(nightEnd_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  scale_x_continuous(name = "Time",
                     breaks = seq(0, 24 * 60 * 60, by = 4 * 60 * 60),
                     labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00")) +
  scale_y_continuous(name = "Date start of night",
                     breaks = seq(min(plot1$date_start_plot), max(plot1$date_start_plot), by = 7),
                     labels = seq.Date(max(plot1$date_start),
                                       min(plot1$date_start),
                                       by = "-1 weeks") |> strftime("%b %d")) +
  # scale_y_date(name = "Date start of night",
  #              breaks = "1 week",
  #              date_labels = "%b %d") +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_c(option = "viridis") +
  guides(color = "none") +
  ggtitle("Proportion inactive") +
  background_grid()

ggsave(plot = p1,
       filename = here("figures", "proportion_sleeping_inv.png"),
       bg = "white",
       width = 30,
       height = 30)

# simspson

p1 <- ggplot(plot1) + 
  geom_segment(aes(y = date_start_plot, yend = date_start_plot,
                   x = start_plot, xend = end_plot,
                   color = simpson_diversity),
               linewidth = 10) +
  geom_line(aes(sunrise_time_plot, date_start_plot), linewidth = 5, color = "orange") + #, lty = "dashed"
  geom_line(aes(sunset_time_plot, date_start_plot), linewidth = 5, color = "darkred") + #, lty = "dashed"
  geom_line(aes(night_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  geom_line(aes(nightEnd_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  scale_x_continuous(name = "Time",
                     breaks = seq(0, 24 * 60 * 60, by = 4 * 60 * 60),
                     labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00")) +
  scale_y_continuous(name = "Date start of night",
                     breaks = seq(min(plot1$date_start_plot), max(plot1$date_start_plot), by = 7),
                     labels = seq.Date(max(plot1$date_start),
                                       min(plot1$date_start),
                                       by = "-1 weeks") |> strftime("%b %d")) +
  # scale_y_date(name = "Date start of night",
  #              breaks = "1 week",
  #              date_labels = "%b %d") +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_c(option = "viridis") +
  guides(color = "none") +
  ggtitle("Simpson's index") +
  background_grid()

p1 <- ggplot(plot1) + 
  geom_segment(aes(y = date_start_plot, yend = date_start_plot,
                   x = start_plot, xend = end_plot,
                   color = simpson_diversity),
               linewidth = 10) +
  geom_line(aes(sunrise_time_plot, date_start_plot), linewidth = 5, color = "orange") + #, lty = "dashed"
  geom_line(aes(sunset_time_plot, date_start_plot), linewidth = 5, color = "darkred") + #, lty = "dashed"
  geom_line(aes(night_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  geom_line(aes(nightEnd_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  scale_x_continuous(name = "Time",
                     breaks = seq(0, 24 * 60 * 60, by = 4 * 60 * 60),
                     labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00")) +
  scale_y_continuous(name = "Date start of night",
                     breaks = seq(min(plot1$date_start_plot), max(plot1$date_start_plot), by = 7),
                     labels = seq.Date(max(plot1$date_start),
                                       min(plot1$date_start),
                                       by = "-1 weeks") |> strftime("%b %d")) +
  # scale_y_date(name = "Date start of night",
  #              breaks = "1 week",
  #              date_labels = "%b %d") +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_c(option = "viridis") +
  guides(color = "none") +
  ggtitle("Simpson's index") +
  background_grid()

ggsave(plot = p1,
       filename = here("figures", "simpson_index_test.png"),
       bg = "white",
       width = 30,
       height = 30)

# number of individuals 

p1 <- ggplot(plot1) + # [date_start %in% ymd("2018-09-10"):ymd("2018-09-10")] [start.Timestamp <= ymd_hms("2018-09-10 00:00:10") & start.Timestamp >= ymd_hms("2018-09-10 00:00:00")]
  geom_segment(aes(y = date_start_plot, yend = date_start_plot,
                   x = start_plot, xend = start_plot,
                   color = number_individuals),
               linewidth = 10, alpha = 1) +
  geom_line(aes(sunrise_time, date_start_plot), linewidth = 5, color = "orange") + #, lty = "dashed"
  geom_line(aes(sunset_time, date_start_plot), linewidth = 5, color = "darkred") + #, lty = "dashed"
  geom_line(aes(night_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  geom_line(aes(nightEnd_time_plot, date_start_plot), linewidth = 5, color = "darkblue") +
  scale_x_continuous(name = "Time",
                     breaks = seq(0, 24 * 60 * 60, by = 4 * 60 * 60),
                     labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00")) +
  scale_y_continuous(name = "Date start of night",
                     breaks = seq(min(plot1$date_start_plot), max(plot1$date_start_plot), by = 7),
                     labels = seq.Date(max(plot1$date_start),
                                       min(plot1$date_start),
                                       by = "-1 weeks") |> strftime("%b %d")) +
  # scale_y_date(name = "Date start of night",
  #              breaks = "1 week",
  #              date_labels = "%b %d") +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_c(option = "viridis") +
  guides(color = "none") +
  ggtitle("Number of individuals") +
  background_grid()

ggsave(plot = p1,
       filename = here("figures", "number_individuals_inv.png"),
       bg = "white",
       width = 30,
       height = 30)




# Part 2: sleep ammount and stats ----

# calculate sleep ammount
sleep_ammount <- merge(behaviours_inActive_m |>
                         dcast(id + id_behaviour + date ~ point, value.var = "Timestamp")|>
                         rename(date_start = date),
                       data_astronomical, 
                       by.x = "date_start", 
                       by.y = "date")[, -c("id_behaviour")
                       ][, ":="(date_end = date(end),
                                time_start = as_hms(start),
                                time_end = as_hms(end))
                       ][, ":="(is_night = if_else(start < sunrise | start > sunset, T, F))
                         ]

# modify this dataset
sleep_ammount_m <- data.table()
for(i in 1:length(unique(sleep_ammount$sunrise_time))) { 
  print(i/length(unique(sleep_ammount$sunrise_time)))
  sleep_ammount_m <- rbind(sleep_ammount_m,
                           split_behaviours(sleep_ammount[sunrise_time == unique(sleep_ammount$sunrise_time)[i]],
                                            date_time = quote(start),
                                            start_time_col = quote(time_start), 
                                            start_time_col_2 = time_start,
                                            end_time_col = quote(time_end), 
                                            end_time_col_2 = time_end,
                                            split = unique(sleep_ammount$sunrise_time)[i]) |>
                             split_behaviours(date_time = quote(start),
                                              start_time_col = quote(time_start), 
                                              start_time_col_2 = time_start,
                                              end_time_col = quote(time_end), 
                                              end_time_col_2 = time_end,
                                              split = unique(sleep_ammount$sunset_time)[i]))
}
# identify night period
sleep_ammount_m <- sleep_ammount_m[, ":="(duration = difftime(end, start, units = "secs"))
                                   ][order(date_start, time_start) # both bc start is wrong when splitted
                                   ][, ":="(number_night = rleid(is_night))]

# filter and save data 
sleep_ammount_f <- sleep_ammount_m[, ":="(first_night = min(number_night)), 
                                   by = id][number_night != first_night]
unique(sleep_ammount_f[, c("id", "date_start")])[order(id)]
unique(copy(sleep_ammount)[, ":="(first_night = min(date_start)), 
                                 by = id][, c("id", "first_night")])[order(id)]
unique(copy(sleep_ammount_m)[, ":="(first_night = min(date_start)), 
                           by = id][, c("id", "first_night")])[order(id)]
unique(copy(behaviours_inActive)[, ":="(first_night = min(Date)), 
                                 by = ID][, c("ID", "first_night")])[order(ID)]
unique(copy(behaviours_inActive_m)[, ":="(first_night = min(date)), 
                                 by = id][, c("id", "first_night")])[order(id)]

sleep_ammount_s <- sleep_ammount_f[is_night == T
][, .(date_night_start = min(date_start),
      sunset_time = min(sunset_time),
      sunrise_time = max(sunrise_time),
      night_end = max(nightEnd_time),
      night_start = min(night_time),
      tot_inactive = sum(duration),
      number_of_wakes = length(unique(start))),  # to not conisder two times splitted behaviours
  by = list(id, number_night)
][order(number_night, id)
  ][, ":="(night_l = 24 * 60 * 60 - sunset_time + sunrise_time)
    ][, ":="(others_wake = sum(number_of_wakes) - number_of_wakes,
             others_inactive = sum(tot_inactive) - tot_inactive), 
      by = number_night]

write.csv(sleep_ammount_s, here("data", "summary.csv"))

ggplot(sleep_ammount_s) +
  geom_boxplot(aes(id, tot_inactive)) +
  background_grid()

ggsave(here("figures", "total_inactivity_during_one_night.png"),
       height = 2,
       width = 10,
       bg = "white")

ggplot(sleep_ammount_s) +
  geom_boxplot(aes(id, number_of_wakes)) +
  background_grid()

ggsave(here("figures", "total_waking_events_during_one_night.png"),
       height = 2,
       width = 10,
       bg = "white")
  
ggplot(sleep_ammount_s) +
  geom_line(aes(id, tot_inactive, color = number_night, group = number_night))

ggplot(sleep_ammount_s) +
  geom_point(aes(night_l, tot_inactive)) +
  geom_smooth(aes(night_l, tot_inactive),
              method = "lm") +
  background_grid()

ggplot(sleep_ammount_s) +
  geom_point(aes(night_l, number_of_wakes, color = id)) +
  geom_smooth(aes(night_l, number_of_wakes, color = id),
              method = "lm") +
  geom_smooth(aes(night_l, number_of_wakes), 
              linewidth = 2, color = "black",
              method = "lm") +
  background_grid()

ggsave(here("figures", "number_of_wakes~night_duration_by_id.png"),
       height = 5,
       width = 5,
       bg = "white")

ggplot(sleep_ammount_s) +
  geom_point(aes(night_l, number_of_wakes)) +
  geom_smooth(aes(night_l, number_of_wakes), 
              method = "lm") +
  background_grid()

ggsave(here("figures", "number_of_wakes~night_duration.png"),
       height = 3,
       width = 3,
       bg = "white")


ggplot(sleep_ammount_s) +
  geom_boxplot(aes(as.factor(date_night_start), 
                   number_of_wakes)) +
  background_grid()


ggplot(sleep_ammount_s) +
  geom_boxplot(aes(id, 
                   number_of_wakes)) +
  background_grid()

test_lm <- lm(data = sleep_ammount_s,
   number_of_wakes ~  as.factor(number_night) + id + others_wake)
summary(test_lm)

test_lm2 <- lm(data = sleep_ammount_s,
               as.numeric(tot_inactive) ~ id + as.numeric(others_inactive))
summary(test_lm2)


# plot when they go to sleep 
p1 <- ggplot(sleep_ammount) +
  geom_segment(aes(x = time_start, xend = time_end,
                y = id, yend = id), linewidth = 5, alpha = 0.01)

ggsave(here("figures", "test", "when_sleep.png"),
       height = 5,
       width = 10,
       bg = "white")

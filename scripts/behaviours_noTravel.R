# BEHAVIOURS BETWEEN TRAVEL ----

## Filter and combine data sets (~10 minutes to do) ----
env <- .GlobalEnv # set global environment as variable so that I can subset it later (cfr: https://stackoverflow.com/questions/39521038/apply-common-function-to-all-data-frames-and-return-data-frames-with-same-name)
obj <- allForagingFileNames[1]
behaviours_interval <- merge.data.table(env[[obj]][, c("Timestamp", "Category", "ID", "Date")][, date := date(Date)][,!c("Date")], 
                                        nonTravel_m, # cool way to filter data set 
                                        by = "date", 
                                        allow.cartesian = TRUE, # tells me to do it...
                                        all = TRUE)[Timestamp >= startInterval & Timestamp <= endInterval]

# Reduce behaviors to continuous behaviors events. It takes 30 seconds but the database is so much more usable and small
behaviours_interval <- find_continuous_behaviours_filter(behaviours_interval, minDate = "2016-08-08", maxDate = "2019-08-09")

# check data set 
ggplot(behaviours_interval[interval_id %in% 190:200]) +
  geom_segment(aes(y = ID, yend = ID,
                   x = start, 
                   xend = end, # I consider the behaviours happeing from start to -epsilon of next second for visualization purposes
                   color = Category),
               linewidth = 10) +
  # geom_rect(data = travel_dataset_s,
  #           aes(ymin = "AA", ymax = "ZZ",
  #               xmin = start, xmax = end), alpha = 0.2) +
  # scale_x_datetime(limits = c(ymd_hms("2018-08-24 00:00:00"),
  #                             ymd_hms("2018-08-25 00:00:00"))) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  background_grid() 

# loop through the rest
for(obj in allForagingFileNames[-1]) {
  print(obj)
  newOne <- merge.data.table(env[[obj]][, c("Timestamp", "Category", "ID", "Date")][, date := date(Date)][,!c("Date")], 
                             nonTravel_m, # cool way to filter data set 
                             by = "date", 
                             allow.cartesian = TRUE, # tells me to do it...
                             all = TRUE)[Timestamp >= startInterval & Timestamp <= endInterval]
  behaviours_interval <- rbind(behaviours_interval, 
                               find_continuous_behaviours_filter(newOne, minDate = "2016-08-08", maxDate = "2019-08-09"))
}
rm(newOne, env)

# create a summary for each non-travel event ----
nonTravel_sumary <- copy(behaviours_interval)[, .(duration = unique(as.numeric(difftime(endInterval, startInterval, units = "secs"))),
                                                  startInterval = unique(startInterval),
                                                  endInterval = unique(endInterval),
                                                  nPartecipating = length(unique(ID))),
                                              by = interval_id][order(interval_id)]
fwrite(nonTravel_sumary, file = (here::here("outputs", "data_summaries", "nonTravel_summary.csv")))

# Modify data ----
nonTravel_m <- copy(behaviours_interval)[, ':='(time_spent = difftime(end, start),
                                                ref_start = as.numeric(difftime(start, startInterval, units = "secs")),
                                                nPartecipating = length(unique(ID))),
                                         by = list(interval_id, ID, Category)
][, ':='(tot_spent = difftime(end, start),
         cum_time_spent = cumsum(as.numeric(time_spent))),
  by = list(interval_id, ID, Category)
][, ':='(SD_tot_spent = sd(tot_spent)),
  by = list(interval_id, ID, Category)
]

# Plot time spent in each behaviour per individual
ggplot(nonTravel_m[interval_id %in% c(153:154)]) +
  geom_col(aes(ID, tot_spent, fill = Category)) +
  facet_wrap(~startInterval)

# Plot cumulative time spent foraging
ggplot(nonTravel_m[Category == "FOR" & interval_id %in% 153:154]) +
  geom_step(aes(ref_start/(60*60), cum_time_spent, 
                color = ID, group = ID)) +
  facet_wrap(~interval_id) +
  scale_color_viridis_d(option = "turbo") +
  background_grid()

# Plot cumulative time spent foraging
ggplot(nonTravel_m[interval_id %in% 130]) +
  geom_step(aes(ref_start/(60*60), cum_time_spent, 
                color = Category, group = Category)) +
  facet_wrap(~ID) +
  background_grid()








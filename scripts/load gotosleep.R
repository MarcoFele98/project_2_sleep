load(here("data", "data.RData"))

# get astronomical info
data_astronomical <-
  as.data.table(getSunlightTimes(
    date = seq.Date(as.Date("2018-07-25"), as.Date("2018-10-16"), by = 1),
    keep =  c("solarNoon", "sunrise", "sunset", "sunriseEnd", "sunsetStart",
              "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night"),
    lat = -33.918861,
    lon = 18.423300,
    tz = "UTC"
  ))[, ":="(sunrise_time = as_hms(sunrise),
            sunset_time = as_hms(sunset),
            nauticalDawn_time = as_hms(nauticalDawn),
            nauticalDusk_time = as_hms(nauticalDusk),
            night_time = as_hms(night),
            nightEnd_time = as_hms(nightEnd))] |>
  merge(getMoonIllumination(date = seq.Date(as.Date("2018-07-25"), 
                                            as.Date("2018-10-16"), 
                                            by = 1), keep = c("phase")),
        by = "date")


# get dominance rank
info_baboons <- as.data.table(readxl::read_excel(here::here("data", 
                                                            "Proximity,Rank,Leadership_Data_210504.xlsx")))[Area == "Urban"
                                                                                                            ][, c("Bab_Name", "Rank")
                                                                                                              ][, ':='(Bab_Name = case_when(Bab_Name == "NN" ~"NikNak", 
                                                                                                                       Bab_Name == "Sheb" ~"Shebeleza",
                                                                                                                       Bab_Name == "Trin" ~"Trinity",
                                                                                                                       T ~ Bab_Name))
                                                                                                                ][Bab_Name %in% idsRanked
                                                                                                                  ][, ":="(sex = if_else(Bab_Name %in% c("Shebeleza", "Kangela"), "male", "female"))] |> 
  rename(id = Bab_Name, dominance = Rank)


test <- readRDS(here::here("data", # fill each second 
                   "Baboon behaviour from acceleration (1)", 
                   "Azultrim2021.rds"))

# Load and modify behaviour data sets ----
# (~10 minutes to do) 
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
         fill_empty_time(as.data.table(readRDS(here::here("data", # fill each second 
                                                          "Baboon behaviour from acceleration (1)", 
                                                          file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
                                                                        
         
}
allForagingFileNames <- grep("foraging", ls(), value = TRUE)

# read one 
# test <- as.data.table(readRDS(here::here("data", # fill each second
#                                  "Baboon behaviour from acceleration (1)",
#                                  "Azultrim2021.rds")))
# 
# test <- test[Timestamp >= ymd_hms("2018-08-23 00:00:00") &
#                Timestamp <= ymd_hms("2018-08-24 00:00:00")]
                              

# REDUCE AND COMBINE ALL BEHAVIOURS ----
# Combine and reduce data sets (~10 minutes to do) 
allForagingFileNames <- grep("foraging", ls(), value = TRUE)
env <- .GlobalEnv # set global environment as variable so that I can subset it later (cfr: https://stackoverflow.com/questions/39521038/apply-common-function-to-all-data-frames-and-return-data-frames-with-same-name)
obj <- allForagingFileNames[1]

behaviours_noFilter <- find_continuous_behaviours(env[[obj]][, c("Timestamp", "Category", "ID", "Date")], 
                                                  minDate = "2016-04-08", maxDate = "2019-12-09")

# check one individual
ggplot(behaviours_noFilter[start >= ymd_hms("2018-07-26 00:00:00") &
                               start <= ymd_hms("2018-07-27 00:00:00")]) + #  & Category %in% c("FOR", "TRA")
  geom_segment(aes(y = ID, yend = ID,
                   x = start, xend = end,
                   color = Category),
               linewidth = 10) +
  #scale_x_datetime(breaks = date_breaks(width = "4 hour")
  # limits = c(ymd_hms("2018-08-25 00:00:00"),
  #                            ymd_hms("2018-08-26 00:00:00"))
  #            ) +
  scale_x_time(breaks = date_breaks(width = "4 hour")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_d(option = "turbo") +
  #facet_wrap(~date) +
  background_grid() 

# loop through the rest
for(obj in allForagingFileNames[-1]) {
  print(obj)
  behaviours_noFilter <- rbind(behaviours_noFilter,  
                    find_continuous_behaviours(env[[obj]][, c("Timestamp", "Category", "ID", "Date")], 
                                                          minDate = "2016-08-08", maxDate = "2019-08-09"))
}
rm(env)

# modify data set
behaviours_noFilter_m <- copy(behaviours_noFilter)[, ':='(date = date(start),
                                                          refTimeStart = as_hms(start),
                                                          refTimeEnd = as_hms(end),
                                                          inactive = if_else(Category %in% c("RG", "REST"), 
                                                                             T,F))
                                                        ][, ":="(inactive = if_else(is.na(Category), 
                                                                                    NA, inactive))][order(start)]

test <- behaviours_noFilter_m[ID == "Azul" &
                                start >= ymd_hms("2018-07-27 00:00:00") &
                                end < ymd_hms("2018-07-28 00:00:00")]

# Plot behaviours in time 
ggplot(test) + #& Category %in% c("FOR", "TRA")
  geom_segment(aes(y = ID, yend = ID,
                   x = refTimeStart, xend = refTimeEnd,
                   color = inactive, group = Category),
               linewidth = 10) +
  scale_x_time(breaks = date_breaks(width = "4 hour")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3, 
                                                   alpha = 1))) +
  scale_color_viridis_d(option = "viridis") +
  background_grid() 



# REDUCE AND COMBINE ALL INACTIVE BEHAVIOURS ----
# Combine and reduce data sets (~10 minutes to do) 
allForagingFileNames <- grep("foraging", ls(), value = TRUE)
env <- .GlobalEnv # set global environment as variable so that I can subset it later (cfr: https://stackoverflow.com/questions/39521038/apply-common-function-to-all-data-frames-and-return-data-frames-with-same-name)
obj <- allForagingFileNames[1]

behaviours_inActive <- find_continuous_inactive_behaviours(env[[obj]][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))
                                                                      ][, ":="(inactive = if_else(is.na(Category), 
                                                                                                  NA, inactive))
                                                                        ][, c("Timestamp", "inactive", "ID", "Date")], 
                                                  minDate = "2016-04-08", maxDate = "2019-12-09")
# This is just to check the results
# check_behaviours_inActive <- dcast(behaviours_inActive, 
#                                    ID + id_behaviour + inactive ~ point, value.var = "Timestamp")[, end := end + duration(1, "secs")
#                                                                                                   ][, ":="(date = date(start),
#                                                                                                   refTimeStart = as_hms(start),
#                                                                                                   refTimeEnd = as_hms(end))][
#                                                                                                     start >= ymd_hms("2018-07-27 00:00:00") &
#                                                                                                       end < ymd_hms("2018-07-28 00:00:00")]
# # check one individual
# ggplot(check_behaviours_inActive[start >= ymd_hms("2018-07-26 00:00:00") &
#                              start <= ymd_hms("2018-07-28 00:00:00")]) + #  & Category %in% c("FOR", "TRA")
#   geom_segment(aes(y = ID, yend = ID,
#                    x = refTimeStart, xend = refTimeEnd,
#                    color = inactive),
#                linewidth = 10) +
#   scale_x_time(breaks = date_breaks(width = "4 hour")) +
#   guides(colour = guide_legend(override.aes = list(linewidth = 3, 
#                                                    alpha = 1))) +
#   scale_color_viridis_d(option = "viridis") +
#   background_grid() 

# loop through the rest
for(obj in allForagingFileNames[-1]) {
  print(obj)
  behaviours_inActive <- rbind(behaviours_inActive,  
                               find_continuous_inactive_behaviours(env[[obj]][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))
                               ][, ":="(inactive = if_else(is.na(Category), 
                                                           NA, inactive))
                               ][, c("Timestamp", "inactive", "ID", "Date")], 
                               minDate = "2016-04-08", maxDate = "2019-12-09")
  )
}
rm(env)

# modify and filter individual data
behaviours_inActive_m <- merge((behaviours_inActive |>
                                 merge(dominance_id[, c("id", "dominance")] |>
                                         rename(adding_dominance = dominance),
                                       by.x = "ID", by.y = "id"))[point == "end", ":="(Timestamp = Timestamp + duration(1, "secs"),
                                                                                       adding_dominance = -adding_dominance) # because by transforming seconds to period with start end I consider the end the "start of the next second"
                                                   ][order(Timestamp, ID)
                                                   ][inactive == T
                                                     # calculate number inactive
                                                     ][, ":="(number_inactive = if_else(point == "start", 1, -1))
                                                     ][, ":="(number_inactive = cumsum(number_inactive))
                                                     # calculate other interesting things
                                                     ][!is.na(adding_dominance), ":="(dominance_inactive = cumsum(adding_dominance))
                                                     ][!is.na(adding_dominance), ":="(number_inactive_excluding_dominance = if_else(point == "start", 1, -1))
                                                     ][!is.na(adding_dominance), ":="(number_inactive_excluding_dominance = cumsum(number_inactive_excluding_dominance))
                                                     ][, ":="(number_individuals = max(number_inactive)), by = Date
                                                       ][, ":="(simpson_diversity = (2 * number_inactive * (number_individuals - number_inactive)) / 
                                                                (number_individuals * (number_individuals - 1)))
                                                         ] |> 
  rename(date = Date, id = ID),
  data_astronomical, 
  by.x = "date", by.y = "date")[, ":="(time_start = as_hms(Timestamp))
  ][, ":="(is_night = if_else(time_start < sunrise_time | time_start > sunset_time, T, F))]


str(behaviours_inActive_m)

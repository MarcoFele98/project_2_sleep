# Packages #

library(pals) # for cool parula palette
library(here) # for "project-based" relative pathways
library(tidyverse) # no need to explain
library(cowplot) # sexy
library(lubridate) # for dates and times
library(hms) # for within day date and time
library(ggspatial) # for scale in plot
library(viridis) # sexy
library(sp) # for correct georeferenced points
library(scales) # for databraks in scale
library(data.table) # for faster all
library(suncalc) 
library(tidygraph) # for networks
library(ggraph)
library(igraph) # for networks
#library(markovchain) # for Markov chains
library(ggpubr) # for ggarrange
#library(patchwork) # for plot_spacer()
#library(png) # for 
library(pipeR) # for %>>%
library(readxl) # for reading excel
library(tinytable)
# library(runner) # for rolling functions but fuck you so slow
#library(zoo)
#library(gtools) # for finding permutations
library(matrixStats) # for weighted median
library(Rcpp)
library(broom.mixed)
# stats
library(glmmTMB)
#library(Matrix)
#library(AICcmodavg) # for AIC and model comparison
library(grid)
library(gridExtra)


theme_set(theme_cowplot())
options(scipen = 999)

idsRanked <- c(#"Cindy", Patch"
               "Patch",
               #"Sunny",
               "Azul",
               "Lola",
               #"Don",
               "Trinity",
               "Nelly",
               "NikNak", 
               "Kym",
               "Luna",
               "Gabrielle",
               "Hanson", 
               "Shebeleza",
               "Kangela")

idsRanked_wrong_names <- c(#"Cindy", Patch"
  "Patch",
  "Azul",
  "Lola",
  "Trinity",
  "Nelly",
  "NikNak", 
  "Kym",
  "Luna",
  "Gabrielle",
  "Hanson", 
  "Sheb",
  "Kangela")

ids_distances_ranked <- c("Azul",
                          "Lola",
                          "Trinity",
                          "Nelly",
                          "NikNak", 
                          "Kym",
                          "Luna",
                          "Hanson", 
                          "Sheb",
                          "Kangela")

ids_distances_ranked_correct_names <- c("Azul",
                                        "Lola",
                                        "Trinity",
                                        "Nelly",
                                        "NikNak", 
                                        "Kym",
                                        "Luna",
                                        "Hanson", 
                                        "Shebeleza",
                                        "Kangela")

ids <- c("Trinity", "Sheb", "Patch", "NikNak", "Nelly", "Luna", "Lola", "Kym", "Kangela", "Hanson", "Gabrielle", "Azul")
# column order of combined_m
column_order <-  c("Trinity", "Shebeleza", "Patch", "NikNak", "Nelly", "Luna", "Lola", "Kym", "Kangela", "Hanson", "Gabrielle", "Azul")


dominance_ranked <- c(0, 0.22222222, 0.27777778, 
                      # 0.33333333, 
                      0.5000000, 0.55555556, 0.66666667, 0.72222222, 0.77777778, 0.83333333, 0.94444444, 0.94444444, 1.00000000)

dominance_ranked_columns <- c(0.22222222, 0.27777778, 
                      # 0.33333333, 
                      0.5000000, 0.55555556, 0.66666667, 0.72222222, 0.77777778, 0.94444444, 0.94444444, 1.00000000)

possible_periods <- c("night_time", "wakey_time", "bed_time")

dominance_id <- data.table(id = idsRanked,
                           dominance = dominance_ranked,
                           sex = c("female", "female", "female", "female", "female", "female", "female", "female", "female", "female", "male", "male"),
                           #sex_type = c(NA, "f", "f_infants", "f_infants", "f", "f", "f_infants", "f_infants", NA, "f_infants", "m", "m"),
                           baboon = c("F19","F15","F14","F10","F9","F7","F6","F5","F4","F2","M2","M1"))[, ":="(id = factor(id, levels = idsRanked),
                                                                                                            baboon = factor(baboon, 
                                                                                                                            levels = c("F19","F15","F14","F10","F9","F7","F6","F5","F4","F2","M2","M1") |> rev()))]

all_id <- data.table(id = c("Patch",
                            "Cindy",
                            "Sunny",
                            "Philomena",
                            "Azul",
                            "Lola",
                            "Don",
                            "Madra",
                            "Dennis",
                            "Trinity",
                            "Nelly",
                            "Omega",
                            "NikNak", 
                            "Kym",
                            "Luna",
                            "Gabrielle",
                            "Iver",
                            "Hanson", 
                            "Fatty",
                            "Shebeleza",
                            "Kangela"),
                     dominance = c(seq(0, 1, l = 19), 
                                   c(0.9444444, 1)),
                     sex = c(rep("female", 19), 
                             rep("male", 2)),
                     baboon = c(paste0("F", 19:1),
                                c("M2", "M1")),
                     accelerometer = c(T, F, F, F, T, T, F, F, F, T, T, F, T, T, T, T, F, T, F, T, T),
                     gps = c(F, T, T, F, T, T, T, F, F, T, F, F, T, T, T, F, F, T, F, T, T))

write.csv(all_id,
          file = "data/baboon_info.csv",
          row.names = F)

dominance_id_columns <- data.table(id = c(  
  "Patch",
  "Cindy", 
  "Sunny",
  "Azul",
  "Lola",
  "Don",
  "Trin",
  #"Nelly",
  "NikNak", 
  "Kym",
  "Luna",
  #"Gabrielle",
  "Hanson", 
  "Sheb",
  "Kan"),
                           #dominance = dominance_ranked_columns,
                           #sex = c("female", "female", "female", "female", "female", "female", "female", "female", "male", "male"),
                           #sex_type = c(NA, "f", "f_infants", "f_infants", "f", "f", "f_infants", "f_infants", NA, "f_infants", "m", "m"),
                           baboon = c("F21", "F20","F19", "F17","F16", "F15", "F12","F9","F8","F7", "F4","M2","M1"))
                                                                                                                

n_individuals <- length(column_order)

# # get astronomical info
# data_astronomical <- as.data.table(getSunlightTimes(
#     date = seq.Date(as.Date("2018-07-24"), as.Date("2018-10-17"), by = 1),
#     keep =  c("solarNoon", "sunrise", "sunset", "sunriseEnd", "sunsetStart",
#               "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night"),
#     lat = -33.918861,
#     lon = 18.423300,
#     tz = "UTC"
#   ))[, ":="(sunrise_time = as_hms(sunrise),
#             sunset_time = as_hms(sunset),
#             nauticalDawn_time = as_hms(nauticalDawn),
#             nauticalDusk_time = as_hms(nauticalDusk),
#             night_time = as_hms(night),
#             nightEnd_time = as_hms(nightEnd))
#      ][, ":="(next_sunrise_time = c(tail(sunrise_time, -1), as_hms(NA)))
#        ][, ":="(night_duration = as_hms(difftime(next_sunrise_time, sunset_time)))] |>
#   merge(getMoonIllumination(date = seq.Date(as.Date("2018-07-24"), 
#                                             as.Date("2018-10-17"), 
#                                             by = 1),
#                             keep = c("phase")),
#         by = "date")

dates_all <- c("2018-08-02", "2018-08-03", "2018-08-04", "2018-08-05", "2018-08-06", "2018-08-07", "2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11", "2018-08-12", "2018-08-13" ,"2018-08-14" ,"2018-08-15" ,"2018-08-16" ,"2018-08-17",
               "2018-08-18", "2018-08-19", "2018-08-20", "2018-08-21", "2018-08-22", "2018-08-23", "2018-08-24", "2018-08-25", "2018-08-26")

# get weather data

weather_data <- as.data.table(read_excel(here("data", "weather.xlsx")))[, ":="(date = date(date))]


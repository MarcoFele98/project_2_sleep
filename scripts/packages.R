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
library(markovchain) # for Markov chains
library(ggpubr) # for ggarrange
library(patchwork) # for plot_spacer()
library(png) # for 
library(pipeR) # for %>>%
# library(runner) # for rolling functions but fuck you so slow
library(zoo)

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

dominance_id <- data.table(id = idsRanked,
                           dominance = c(0, 0.22222222, 0.27777778, 
                                        # 0.33333333, 
                                         0.5000000, 0.55555556, 0.66666667, 0.72222222, 0.77777778, 0.83333333, 0.94444444, 0.94444444, 1.00000000),
                           sex = c("female", "female", "female", "female", "female", "female", "female", "female", "female", "female", "male", "male"),
                           sex_type = c(NA, "f", "f_infants", "f_infants", "f", "f", "f_infants", "f_infants", NA, "f_infants", "m", "m"),
                           baboon = c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","M1","M2"))[, ":="(id = factor(id, levels = idsRanked),
                                                                                                            baboon = factor(baboon, levels = c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","M1","M2")))]

ids <- c("Trinity", "Sheb", "Patch", "NikNak", "Nelly", "Luna", "Lola", "Kym", "Kangela", "Hanson", "Gabrielle", "Azul")


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
                                            by = 1),
                            keep = c("phase")),
        by = "date")



dates_all <- c("2018-08-02", "2018-08-03", "2018-08-04", "2018-08-05", "2018-08-06", "2018-08-07", "2018-08-08", "2018-08-09", "2018-08-10", "2018-08-11", "2018-08-12", "2018-08-13" ,"2018-08-14" ,"2018-08-15" ,"2018-08-16" ,"2018-08-17",
               "2018-08-18", "2018-08-19", "2018-08-20", "2018-08-21", "2018-08-22", "2018-08-23", "2018-08-24", "2018-08-25", "2018-08-26")

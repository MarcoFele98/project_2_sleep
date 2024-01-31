# Load datasets

#load(here("data", "data_epoch.RData"))
#load(here("data", "data_meanVeBDAs_t0.03.RData"))


# create  datasets ----
## For random forest ----
# for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
#   print(file)
#   assign(paste0("foraging", sub("trim2021.rds.*", "", file)), 
#          fill_empty_time(as.data.table(readRDS(here::here("data", # fill each second 
#                                                           "Baboon behaviour from acceleration (1)", 
#                                                           file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
#   
#   
# }

## For mean.VeBDAs ----
# for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
#   print(file)
#   assign(paste0("foraging", sub("trim2021.rds.*", "", file)),
#          fill_empty_time_mean_VeDBAs(as.data.table(readRDS(here::here("data", # fill each second
#                                                                       "Baboon behaviour from acceleration (1)",
#                                                                       file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference
# 
# 
# }
# # plot
# env <- .GlobalEnv
# allForagingFileNames <- grep("foraging", ls(), value = TRUE)
# for(obj in allForagingFileNames) {
#   print(obj)
#   p1 <- ggplot(env[[obj]]) +
#     geom_histogram(aes(mean.VeDBAs)) +
#     scale_x_log10(breaks = c(0.01, 0.02, 0.03, 0.04, 0.1, 0.3, 1, 1.5, 2, 3, 4)) +
#     geom_vline(aes(xintercept = 0.03), linewidth = 2, color = "red") +
#     ggtitle(obj)+
#     background_grid()
# 
#   ggsave(plot = p1,
#          filename = here("figures", "mean.VeBDAs threshold 0.03", "distribution", paste0(obj, ".png")),
#          height = 3,
#          width = 10,
#          bg = "white")
# }

## For epoch ----
for(file in list.files(here::here("data", "Baboon behaviour from acceleration (1)"))) {
  print(file)
  assign(paste0("foraging", sub("trim2021.rds.*", "", file)),
         fill_empty_time_mean_VeDBA(as.data.table(readRDS(here::here("data", # fill each second
                                                                      "Baboon behaviour from acceleration (1)",
                                                                      file)))[, Timestamp := ymd_hms(Timestamp)])[, Date := date(Timestamp)]) # change by reference


}
# plot
env <- .GlobalEnv
allForagingFileNames <- grep("foraging", ls(), value = TRUE)
for(obj in allForagingFileNames) {
  print(obj)
  p1 <- ggplot(env[[obj]]) +
    geom_histogram(aes(mean.VeDBA)) +
    scale_x_log10(breaks = c(0.01, 0.02, 0.03, 0.04, 0.1, 0.3, 1, 1.5, 2, 3, 4)) +
    geom_vline(aes(xintercept = 0.03), linewidth = 2, color = "red") +
    ggtitle(obj)+
    background_grid()

  ggsave(plot = p1,
         filename = here("figures", "random forest", "distribution", paste0(obj, ".png")),
         height = 3,
         width = 10,
         bg = "white")
}

# Modify datasets ----
env <- .GlobalEnv
## For random forest ----
# for(obj in allForagingFileNames) {
#   print(obj)
#   env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
#                              rename(dateTime = Timestamp) |>
#                              merge(data_astronomical[, c("sunrise", "sunset", "date")],
#                                    all.x = TRUE,
#                                    by.x = "Date", 
#                                    by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
#                                    ][, ":="(inactive = if_else(Category %in% c("RG", "REST"), T, F))]
# }

## For mean.VeBDAs ----
# for(obj in allForagingFileNames) {
#   print(obj)
#   env[[obj]] <- (env[[obj]][, ":="(time = as_hms(Timestamp))] |>
#                    rename(dateTime = Timestamp) |>
#                    merge(data_astronomical[, c("sunrise", "sunset", "date")],
#                          all.x = TRUE,
#                          by.x = "Date", 
#                          by.y = "date"))[, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))
#                                            ][, ":="(inactive = if_else(mean.VeDBAs < 0.03, T, F))]
# }

## For epoch ----
for(obj in allForagingFileNames) { # takes a shit ton of time (more than half a day), so I first do this than save than do the rest
  print(obj)
  env[[obj]] <- copy(env[[obj]])[, ":="(time = as_hms(Timestamp),
                                  median_9min_VeBDA = rollapply(mean.VeDBA, 
                                                                FUN = median, na.rm = T,
                                                                width = 60*9 + 1, 
                                                                fill = NA))] |> # by reference
    rename(dateTime = Timestamp) |>
    merge(data_astronomical[, c("sunrise", "sunset", 
                                #"night", "nightEnd","sunrise_time", "sunset_time", "night_time", "nightEnd_time", 
                                "date")],
          all.x = TRUE,
          by.x = "Date", 
          by.y = "date")
}
#save.image(here("data", "data_epoch.RData"))

for(obj in allForagingFileNames) {
  print(obj)
  env[[obj]] <- copy(env[[obj]])[, ":="(candidate_block = if_else(median_9min_VeBDA < quantile(median_9min_VeBDA, probs = 0.1, na.rm = T) * 1.125, 
                                                      T, 
                                                      F))][, ":="(length_candidate_block = rep(rle(candidate_block)[[1]], 
                                                                                               rle(candidate_block)[[1]]))
                                                           ][, ":="(block = ifelse(length_candidate_block > 60 * 30 & candidate_block == T,
                                                                                   T, 
                                                                                   F))
                                                             ][, ":="(length_candidate_united = rep(rle(block)[[1]], 
                                                                                                    rle(block)[[1]]))
                                                                 ][, ":="(period = ifelse(block == F & length_candidate_united < 60 * 45 | # candidate_united == F is always separated by two sleep blocks, so just the ones longer than 45 minutes will suffice
                                                                                            block == T,
                                                                                                    T,
                                                                                                    F))
                                                                   ][, ":="(inactive = ifelse(candidate_block == T & length_candidate_block > 60 * 3,
                                                                                                 T,
                                                                                                 F))
                                                                     ][, ":="(sunrise_time = as_hms(sunrise),
                                                                              sunset_time = as_hms(sunset))
                                                                       ][, ":="(is_night = if_else(dateTime < sunrise | dateTime > sunset, T, F))]
                                                                        
}

# Plot
for(obj in allForagingFileNames) {
  print(obj)
  
p1 <- ggplot(env[[obj]]) +
  geom_tile(aes(time, Date, fill = is_sleeping)) +
  geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
  geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
  ggtitle(obj) +
  scale_fill_viridis_d(direction = -1) 

ggsave(plot = p1,
       filename = here("figures", "epoch", paste0(obj, "_epoch.png")),
       height = 30,
       width = 25,
       bg = "white")  

p1 <- ggplot(env[[obj]]) +
  geom_tile(aes(time, Date, fill = period)) +
  geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
  geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
  ggtitle(obj) +
  scale_fill_viridis_d(direction = -1) 

ggsave(plot = p1,
       filename = here("figures", "epoch", paste0(obj, "_period.png")),
       height = 30,
       width = 25,
       bg = "white") 

p1 <- ggplot(env[[obj]]) +
  geom_tile(aes(time, Date, fill = block)) +
  geom_line(aes(sunrise_time, Date), color = "darkred", linewidth = 2) +
  geom_line(aes(sunset_time, Date), color = "darkred", linewidth = 2) +
  ggtitle(obj) +
  scale_fill_viridis_d(direction = -1) 

ggsave(plot = p1,
       filename = here("figures", "epoch", paste0(obj, "_block.png")),
       height = 30,
       width = 25,
       bg = "white")
}

# Filter and combine datasets ----
## For random forest and mean.VeBDAs ----
# env <- .GlobalEnv
# combined <- env[[allForagingFileNames[1]]][is_night == T][, c("dateTime", "inactive")] |>
#   setnames(old = "inactive", new = str_replace(allForagingFileNames[1], "foraging", ""))
# for(obj in allForagingFileNames[-1]) {
#   print(obj)
#   id_name <- str_replace(obj, "foraging", "")
#   combined <- env[[obj]][is_night == T][, c("dateTime", "inactive")] |>
#     setnames(old = "inactive", new = id_name) |>
#     merge(combined, by = "dateTime", all = TRUE)
# }
# rm(env)

## For epoch ----
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
            


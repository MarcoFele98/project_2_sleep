# Permutation tests
test <- night_sleep[!(id_night %in% c(1, 49))][, c("id", "time_sleep", "id_night")] # exclude incomplete nights

rest_quantity <- (expand.grid(id = idsRanked, permutation = 1:10000) |> as.data.table())[, ":="(avg_sleep = NA)]

for(permutation in 1:10000) {
  print(permutation)
  rest_quantity[permutation:(permutation+11), "avg_sleep"] <- test[, ":="(time_sleep = sample(time_sleep, 
                                            size = .N, 
                                            replace = FALSE))
                 ][, .(avg_sleep = mean(time_sleep)), 
                   by = id][order(id)]$avg_sleep
}

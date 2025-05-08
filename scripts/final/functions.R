# Functions #

# Behavior ----

find_continuous_behaviours <- function(dataset, minDate, maxDate){
  #browser()
  dataset2 <- dataset[Timestamp >= ymd(minDate) & Timestamp <= ymd(maxDate)#, c("Timestamp", "Category", "ID") # filter by date and remove useless columns
  ][, id_behavoiur := rleid(Category, Date), by = ID # give same number to each consecutive observations sequence of each combination of category and interval
  ][, .SD[c(1,.N)], by = id_behavoiur # keep only the first and last of consecutive observations (which will be end and start). duplicate if one second behvaiour
  ][, point := rep(c("start", "end"), (.N)/2) # things to put in nice format
  ][, id_behaviour := rep(1:((.N)/2), each = 2)] %>%
    dcast(ID + id_behaviour + Category ~ point, value.var = "Timestamp")
  
  dataset2[, end := end + duration(1, "secs")] # by reference, must add one second to the end point because I am converting an event per second in a span from/to
}

find_continuous_inactive_behaviours <- function(dataset, minDate, maxDate){
  #browser()
  dataset2 <- dataset[Timestamp >= ymd(minDate) & Timestamp <= ymd(maxDate)#, c("Timestamp", "Category", "ID") # filter by date and remove useless columns
  ][order(Timestamp)
  ][, id_behaviour := rleid(inactive, Date), by = ID # give same number to each consecutive observations sequence of each combination of category and interval
  ][, .SD[c(1,.N)], by = id_behaviour # keep only the first and last of consecutive observations (which will be end and start). duplicate if one second behvaiour
  ][, point := rep(c("start", "end"), (.N)/2) # things to put in nice format
  ][, id_behaviour := rep(1:((.N)/2), each = 2)]
}

fill_empty_time <- function(datatable)
{
  browser()
  if (length(unique(datatable$ID)) > 1) {
    stop('Df needs to be of 1 individual.')
  }
  misidxs <- which(diff(datatable$Timestamp) > 1)
  if (length(misidxs) > 0)
  {
    mistimes <- .POSIXct(character(0)) 
    for (misidx in misidxs)
    {
      mistime <- (datatable$Timestamp[misidx + 1] - datatable$Timestamp[misidx]) - 1 # minus 1 accounts for excluding the end of the interval
      mistimes <- c(mistimes, datatable$Timestamp[misidx] + 1:mistime)
    }
    toadd <- data.frame(ID = datatable$ID[1], Timestamp = mistimes, Category = NA)
    datatable <- rbind(datatable[, c("Timestamp", "ID", "Category")], 
                toadd)[order(Timestamp)]
  }
  return(datatable)
}

# fill_empty_time_mean_VeDBAs <- function(datatable) {
#   browser()
#   if (length(unique(datatable$ID)) > 1) {
#     stop('Df needs to be of 1 individual.')
#   }
#   #datatable <- datatable[, ":="(Timestamp = ymd_hms(Timestamp))][!is.na(Timestamp)]
#   misidxs <- which(diff(datatable$Timestamp) > 1)
#   if (length(misidxs) > 0) {
#     mistimes <- .POSIXct(character(0)) # maybe as.POSIXct
#     for (misidx in misidxs)
#     {
#       mistime <- (datatable$Timestamp[misidx + 1] - datatable$Timestamp[misidx]) - 1 # minus 1 accounts for excluding the end of the interval
#       mistimes <- c(mistimes, datatable$Timestamp[misidx] + 1:mistime)
#     }
#     toadd <- data.frame(ID = datatable$ID[1], Timestamp = mistimes, mean.VeDBAs = NA)
#     datatable <- rbind(datatable[, c("Timestamp", "ID", "mean.VeDBAs")], 
#                        toadd)[order(Timestamp)]
#   }
#   return(datatable)
# }

fill_empty_time_mean_VeDBAs <- function(datatable) {
  #browser()
  datatable <- datatable[, c("Timestamp", "ID", "mean.VeDBAs")][!is.na(Timestamp)
                                                                ][, ":="(Timestamp = force_tz(Timestamp, 
                                                                                              tz = "UTC"))]
  full <- data.table(Timestamp = seq(min(datatable$Timestamp), 
                                     max(datatable$Timestamp), 
                                     by = "sec"))
  combined <- merge(datatable, full, all = T)
  return(combined)
}

fill_empty_time_mean_VeDBA <- function(datatable)
{
  browser()
  if (length(unique(datatable$ID)) > 1) {
    stop('Df needs to be of 1 individual.')
  }
  misidxs <- which(diff(datatable$Timestamp) > 1)
  if (length(misidxs) > 0)
  {
    mistimes <- .POSIXct(character(0)) 
    for (misidx in misidxs)
    {
      mistime <- (datatable$Timestamp[misidx + 1] - datatable$Timestamp[misidx]) - 1 # minus 1 accounts for excluding the end of the interval
      mistimes <- c(mistimes, datatable$Timestamp[misidx] + 1:mistime)
    }
    toadd <- data.frame(ID = datatable$ID[1], Timestamp = mistimes, mean.VeDBA = NA)
    datatable <- rbind(datatable[, c("Timestamp", "ID", "mean.VeDBA")], 
                       toadd)[order(Timestamp)]
  }
  return(datatable)
}




split_behaviours <- function(dataset, 
                             date_time,
                             start_time_col, 
                             start_time_col_2,
                             end_time_col, 
                             end_time_col_2,
                             split_time) {
  #browser()
  
  start_time_col_2 <- deparse(substitute(start_time_col_2))
  end_time_col_2 <- deparse(substitute(end_time_col_2))
  
  d <- dataset[c(1:.N, which(get(start_time_col) < split_time & 
                          get(end_time_col) > split_time))
  ][order(get(date_time))
  ][, ":="(duplicated = if_else(get(start_time_col) < split_time & 
                                  get(end_time_col) > split_time, T, F))  
  ][, ":="(first_duplicated = rleid(duplicated))
  ][,  ":="(number = seq_len(.N)), by = first_duplicated]
  
  d[duplicated & number == 1] <- set(d[duplicated & number == 1], 
      j = end_time_col_2, 
      value = eval(substitute(split_time), d, parent.frame()))
  
  d[duplicated & number == 2] <- set(d[duplicated & number == 2], 
           j = start_time_col_2, 
           value = eval(substitute(split_time), d, parent.frame()))
  
  return(d[, -c("duplicated", "number", "first_duplicated")])
}


split_behaviours_long <- function(dataset, 
                             id,
                             date_time,
                             split_time) {
  #browser()
  
  date_time <- deparse(substitute(date_time))

  d <- dataset[c(1:.N, which(get(start_time_col) < split_time & 
                               get(end_time_col) > split_time))
  ][order(get(date_time))
  ][, ":="(duplicated = if_else(get(start_time_col) < split_time & 
                                  get(end_time_col) > split_time, T, F))  
  ][, ":="(first_duplicated = rleid(duplicated))
  ][,  ":="(number = seq_len(.N)), by = first_duplicated]
  
  d[duplicated & number == 1] <- set(d[duplicated & number == 1], 
                                     j = end_time_col_2, 
                                     value = eval(substitute(split_time), d, parent.frame()))
  
  d[duplicated & number == 2] <- set(d[duplicated & number == 2], 
                                     j = start_time_col_2, 
                                     value = eval(substitute(split_time), d, parent.frame()))
  
  return(d[, -c("duplicated", "number", "first_duplicated")])
}


normalise <- function (x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}

# for summing digits of number
digitsum <-  function(n) ifelse(n < 10, n, n %% 10 + sum(floor(n / 10)))

phi_max <- function(table) {
  #browser()
  dims <- dim(table)
  if(dims[1] == 1) table <- rbind(table, c(0, 0))
  if(dims[2] == 1) table <- cbind(table, c(0, 0))
  
  p2 <- sum(table[, 2])
  p1 <- sum(table[2, ])
  q2 <- sum(table[, 1])
  q1 <- sum(table[1, ])
  
  if(p2 >= p1) {
    return(sqrt(q2 * p1) / sqrt(q1 * p2))
  }
  else {
    return(sqrt(q1 * p2) / sqrt(q2 * p1))
  }
}

#source("scripts/phi_max.cpp")

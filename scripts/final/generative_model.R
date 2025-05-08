# parameters
max_time <- 200
n_replicates <- 1
possible_initial_conditions <- rbind(rep(F, n_individuals),
                                     rep(T, n_individuals),
                                     !diag(1, n_individuals, n_individuals))

# pre calculate response probabilities
look_up_data <- unique(as.data.table((expand.grid(id = idsRanked, 
                                                  id_night = 666, # with unknown new level best guess is 0 effect unique(mc_data_covariates_rest$id_night),
                                                  period = "night_time",
                                                  previous_sleep.z = 0,
                                                  Patch = c(0, -1),
                                                  Azul = c(0, 0.22222222),
                                                  Lola = c(0, 0.27777778),
                                                  Trinity = c(0, 0.5000000),
                                                  Nelly = c(0, 0.55555556),
                                                  NikNak = c(0, 0.66666667),
                                                  Kym = c(0, 0.72222222),
                                                  Luna = c(0, 0.77777778),
                                                  Gabrielle = c(0, 0.83333333),
                                                  Hanson = c(0, 0.94444444),
                                                  Shebeleza = c(0, 0.94444444),
                                                  Kangela = c(0, 1.00000000),
                                                  precipitation_mm = 0,
                                                  min_temp = mean(mc_data_covariates_rest$min_temp),
                                                  phase = 0) |>
                                        merge(dominance_id[, c("dominance", "id")]) |>
                                        merge(unique(as.data.table(thresholds_data_w[, c("id", "id_median_VeDBA")]))[!is.na(id)])))
)[, ":="(row_number = 1:.N,
         number_sleeping = apply(.SD[, idsRanked, with = F], 1, 
                                 function(x) {length(x[x > 0.01 | x < -0.01])}))
][, ":="(sum_dominance_sleep_excluding_focus = apply(.SD[, idsRanked[-which(idsRanked == id)], with = F], 1, 
                                                     function(x) {sum(x[x > 0.01])})),
  by = id
][, ":="(maximum_dominance_sleep = sum(dominance_id$dominance) - dominance)
][, ":="(proportion_dominance_sleep_excluding_focus = sum_dominance_sleep_excluding_focus / maximum_dominance_sleep)
][, ":="(number_sleeping_excluding_focus = ifelse(.SD[, get(idsRanked[which(idsRanked == id)])] < -0.01 |
                                                    .SD[, get(idsRanked[which(idsRanked == id)])] > 0.01, 
                                                  number_sleeping - 1,
                                                  number_sleeping)), 
  by = id
][, ":="(avg_dominance_sleep = ifelse(number_sleeping_excluding_focus != 0,
                                      sum_dominance_sleep_excluding_focus / number_sleeping_excluding_focus,
                                      0))]

look_up <- as.data.table(cbind(look_up_data, # confidence intervals need bootstrapping
                               p_as = predict(object = m_active_simple, 
                                                  newdata = look_up_data, 
                                                  type = "response",
                                                  allow.new.levels = T),
                               p_sa = predict(object = m_rest_simple, 
                                                  newdata = look_up_data, 
                                                  type = "response",
                                                  allow.new.levels = T)))


set.seed(666)
# simulate
simulate <- function(replicate, initial_condition, max_time, return_group_configuration = F) {
  #browser()
  # data
  generative_data <- matrix(NA, 
                            ncol = 6,
                            nrow = max_time)
  colnames(generative_data) <- c("initial_condition", "replicate", "time", "individuals_left", "number_sleeping", "sum_dominance_sleep")
  
  generative_group_configuration = matrix(NA, 
                                          ncol = n_individuals,
                                          nrow = max_time) 
  colnames(generative_group_configuration) <- idsRanked
  
  individuals_left_counter <- rep(1, n_individuals - 1) # not the focus individual (the starting condition of one awake)
  
  # set initial condition
  generative_group_configuration[1, ] <- possible_initial_conditions[initial_condition, ]
  generative_data[1, ] <- c(initial_condition = initial_condition,
                            replicate = replicate,
                            time = 0,
                            individuals_left = n_individuals - 1,
                            number_sleeping = sum(generative_group_configuration[1, ]),
                            sum_dominance_sleep = sum(dominance_ranked[generative_group_configuration[1, ]]))
  for(t in 2:max_time) {
    print(t/max_time)
    # usefull variables
    current_group_configuration <- generative_group_configuration[t - 1, ]
    current_generative_data <- generative_data[t - 1, ]
    
    # find event probabilities
    change_probability <- sapply(1:12, FUN = function(id_number) {
      
      if(current_group_configuration[id_number]) { # if focus individual is sleeping
      return(look_up[id == idsRanked[id_number] & # when is sleeping find the precalculated probabilites of changing state from the model
                       number_sleeping_excluding_focus == current_generative_data["number_sleeping"] - 1 &
                       (avg_dominance_sleep > (avg_dominance_sleep - 0.001)) & # do this way for numeric stability
                       (avg_dominance_sleep < (avg_dominance_sleep + 0.001)),
                     "p_sa"][[1]] |> round(digits = 5) |> unique())
      }
      else {
        return(look_up[id == idsRanked[id_number] & # when is sleeping find the precalculated probabilites of changing state from the model
                         number_sleeping_excluding_focus == current_generative_data["number_sleeping"] &
                         (avg_dominance_sleep > (avg_dominance_sleep - 0.001)) & # do this way for numeric stability
                         (avg_dominance_sleep < (avg_dominance_sleep + 0.001)),
                       "p_sa"][[1]] |> round(digits = 5) |> unique())
      }
    })

    # draw whether to change state
    is_changed <- sapply(change_probability, FUN = function(p) rbinom(1, 1, p = p))
    
    individuals_left_counter <- individuals_left_counter * !is_changed[-initial_condition]
    
    # save data
    updated_group_configuration <- xor(current_group_configuration, is_changed)
    generative_group_configuration[t, ] <- updated_group_configuration
    generative_data[t, ] <- c(initial_condition = initial_condition,
                              replicate = replicate,
                              time = t,
                              individuals_left = sum(individuals_left_counter),
                              number_sleeping = sum(updated_group_configuration),
                              sum_dominance_sleep = sum(dominance_ranked[updated_group_configuration]))
  }
  
  if(return_group_configuration == F) {
    return(generative_data)
  }
  else {
    return(generative_group_configuration)
  }
}

data_simulation <- simulate(replicate = 0, max_time = max_time, initial_condition = 1)

for(initial_condition in 1:nrow(possible_initial_conditions)) {
    for(replicate in 1:n_replicates) {
      print(replicate)
     data_simulation <- rbind(data_simulation,
                              simulate(replicate,
                                       initial_condition))
    }
}

ggplot(as.data.table(data_simulation)) +
  geom_step(aes(time, individuals_left, 
                color = initial_condition, 
                group = interaction(initial_condition, replicate)),
            linewidth = 1) +
  scale_color_viridis_c(option = "plasma", direction = 1) +
  facet_wrap(~initial_condition) +
  background_grid()

ggplot(as.data.table(data_simulation)[, ":="(avg_individuals_left = mean(individuals_left)),
                                      by = list(time, initial_condition)]) + #[initial_condition %in% c(1,2)]
  geom_step(aes(time, individuals_left, 
                color = initial_condition, 
                group = interaction(initial_condition, replicate)),
            alpha = 0.3) +
  geom_step(aes(time, avg_individuals_left, 
                color = initial_condition, 
                group = interaction(initial_condition, replicate)),
            linewidth = 1) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  background_grid()

ggsave(here("figures", "mc_results", "functional", "change.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 6,
       width = 10,
       bg = "white")

ggplot(as.data.table(data_simulation)[, ":="(avg_number_sleeping = mean(number_sleeping)),
                                      by = list(time, initial_condition)]) +
  geom_step(aes(time, number_sleeping,
                color = initial_condition,
                group = interaction(initial_condition, replicate)),
            alpha = 0.3) +
  geom_line(aes(time, avg_number_sleeping, 
                color = initial_condition, 
                group = interaction(initial_condition, replicate)),
            linewidth = 1) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  background_grid()

# detailed simulation (for checking distribution on length behaviour) ----

data_simulation_group_configuration <- simulate(replicate = 0, 
                                                initial_condition = 1, 
                                                max_time = 100000,  
                                                return_group_configuration = T)

check_length <- mapply(
  lapply(data_simulation_group_configuration |> as.data.table(),
         FUN = function(x) {
           rle(x)[[1]] |> 
             as.data.table() |> 
             rename(duration = V1)
         }),
  lapply(data_simulation_group_configuration |> as.data.table(),
         FUN = function(x) {
           rle(x)[[2]] |> 
             as.data.table() |> 
             rename(state = V1)
         }), 
  FUN = function(x, y) {
    cbind(x, y)[, .(frequency = .N), 
                by = list(state, duration)]
  }, 
  SIMPLIFY = F) |> 
    rbindlist(idcol = "id")

ggplot(check_length) +
  geom_point(aes(duration, frequency,
                 color = state)) +
  facet_wrap(~id) +
  scale_x_log10() +
  scale_y_log10() +
  background_grid()






# predict temporal distribution of when the group is most profoundly asleep, hence more vulnerable to external attacks


# check dynamics if no social influence (dominance or quourm or both)
# test null model ---- 
p1 <- 0.2
p2 <- 0.5
grid <- 0:100
probability_both_sleep <- dbinom(x = grid, size = 100, prob = p1 * p2)
probability_syncronised <- dbinom(x = grid, size = 100, prob = 1 - (p1 * (1 - p2) + (1 - p1) * p2))

plot(grid, probability_both_sleep, type="b", col = "blue",
     xlab="Number of times",
     ylab="probability")
lines(grid, probability_syncronised, col='red', type = "b")

# markov method to test hypothesis
mm <- data.table(n_both_sleep = replicate(100000, sum(rbinom(n = 100, size = 1, prob = p1) * 
                                                        rbinom(n = 100, size = 1, prob = p2))),
                 n_syncronised = replicate(100000, sum(xor(rbinom(n = 100, size = 1, prob = p1),
                                                           rbinom(n = 100, size = 1, prob = p2))))
) 

hist(mm$n_both_sleep, xlim = c(0, 100))
hist(mm$n_syncronised, xlim = c(0, 100))
# works great!!

# check if temporal structure changes null expectation (very sleazy)
probs <- seq(0.1, 0.9, by = 0.1)
r <- 10 # autocorrelation of successes and not succesess
l <- 100 # autocorrelation of just succecess
n <- 839352 # sample size

sim_data <- sapply(probs, FUN = function(x) {rbinom(n, 1, x)})
sim_data_str <- sapply(probs, FUN = function(x) {
  rbinom(n / r, 1, x) %>>%
    rep(each = r)})
sim_data_str2 <- sapply( 1 / (l * (1 - probs) / probs + 1), FUN = function(x) {
  res <- rbinom(n + n*l, 1, x)
  rep(res, ifelse(res == 1, l, 1))[1:n]
})

# check 
apply(sim_data, 2, mean)
apply(sim_data_str, 2, mean)
apply(sim_data_str2, 2, mean)

# P(N)
hist(apply(sim_data, 1, sum), breaks = 8)
hist(apply(sim_data_str, 1, sum), breaks = 8)
hist(apply(sim_data_str2, 1, sum), breaks = 8)
# nice

# P(ID|N)
sim_data_d <- data.frame()
colnames(sim_data) <- c("a", "b","c","d","e","f","g","h","i")
for(id in colnames(sim_data)) {
  print(id)
  sim_data_d <- rbind(sim_data_d, 
                           as.data.table(sim_data)[, ":="(number_sleeping = apply(sim_data, 1, sum))
                           ][, .(count = .N),
                             by = list(number_sleeping, get(id))
                           ][, ":="(id = id)] |>
                             setnames("get", "is_sleeping"))
}

sim_data_d_m <- copy(sim_data_d)[, ':='(is_sleeping = case_when(is_sleeping == 1 ~ "Sleeping",
                                                                          is_sleeping == 0 ~ "Awake"))
][is_sleeping == "Sleeping",
  # ":="(number_sleeping = number_sleeping - 1), # exclude focus individual
  ":="(number_sleeping = number_sleeping ), # dont exclude focus individual
  # likelihood calculations
][, ":="(tot_id = sum(count)), 
  by = list(is_sleeping, id)
][, ":="(freq_sleep_by_id = count / tot_id), 
  by = list(is_sleeping, id, number_sleeping)
  # posterior calculations
][, ":="(tot_sleeping = sum(count)),
  by = list(is_sleeping, number_sleeping)
][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
  by = is_sleeping
][, ":="(freq_id_by_sleep = count / tot_sleeping), 
  by = list(is_sleeping, number_sleeping, id)
][, ":="(check = sum(freq_sleep_by_id)), 
  by = list(is_sleeping, id)
][, ":="(check2 = sum(freq_id_by_sleep)), 
  by = list(is_sleeping, number_sleeping)]


ggplot(sim_data_d_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, group = id),
            linewidth = 1) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  background_grid()



sim_data_str_d <- data.frame()
colnames(sim_data_str) <- c("a", "b","c","d","e","f","g","h","i")
for(id in colnames(sim_data_str)) {
  print(id)
  sim_data_str_d <- rbind(sim_data_str_d, 
                           as.data.table(sim_data_str)[, ":="(number_sleeping = apply(sim_data_str, 1, sum))
                           ][, .(count = .N),
                             by = list(number_sleeping, get(id))
                           ][, ":="(id = id)] |>
                             setnames("get", "is_sleeping"))
}

sim_data_str_d_m <- copy(sim_data_str_d)[, ':='(is_sleeping = case_when(is_sleeping == 1 ~ "Sleeping",
                                                                        is_sleeping == 0 ~ "Awake"))
][is_sleeping == "Sleeping",
  # ":="(number_sleeping = number_sleeping - 1), # exclude focus individual
  ":="(number_sleeping = number_sleeping ), # dont exclude focus individual
  # likelihood calculations
][, ":="(tot_id = sum(count)), 
  by = list(is_sleeping, id)
][, ":="(freq_sleep_by_id = count / tot_id), 
  by = list(is_sleeping, id, number_sleeping)
  # posterior calculations
][, ":="(tot_sleeping = sum(count)),
  by = list(is_sleeping, number_sleeping)
][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
  by = is_sleeping
][, ":="(freq_id_by_sleep = count / tot_sleeping), 
  by = list(is_sleeping, number_sleeping, id)
][, ":="(check = sum(freq_sleep_by_id)), 
  by = list(is_sleeping, id)
][, ":="(check2 = sum(freq_id_by_sleep)), 
  by = list(is_sleeping, number_sleeping)]


ggplot(sim_data_str_d_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, group = id),
            linewidth = 1) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  background_grid()


sim_data_str2_d <- data.frame()
colnames(sim_data_str2) <- c("a", "b","c","d","e","f","g","h","i")
for(id in colnames(sim_data_str2)) {
  print(id)
  sim_data_str2_d <- rbind(sim_data_str2_d, 
                           as.data.table(sim_data_str2)[, ":="(number_sleeping = apply(sim_data_str2, 1, sum))
                                                        ][, .(count = .N),
                          by = list(number_sleeping, get(id))
                        ][, ":="(id = id)] |>
                          setnames("get", "is_sleeping"))
}

sim_data_str2_d_m <- copy(sim_data_str2_d)[, ':='(is_sleeping = case_when(is_sleeping == 1 ~ "Sleeping",
                                 is_sleeping == 0 ~ "Awake"))
][is_sleeping == "Sleeping",
  # ":="(number_sleeping = number_sleeping - 1), # exclude focus individual
  ":="(number_sleeping = number_sleeping ), # dont exclude focus individual
  # likelihood calculations
][, ":="(tot_id = sum(count)), 
  by = list(is_sleeping, id)
][, ":="(freq_sleep_by_id = count / tot_id), 
  by = list(is_sleeping, id, number_sleeping)
  # posterior calculations
][, ":="(tot_sleeping = sum(count)),
  by = list(is_sleeping, number_sleeping)
][, ":="(freq_number_sleeping = tot_sleeping / sum(count)),
  by = is_sleeping
][, ":="(freq_id_by_sleep = count / tot_sleeping), 
  by = list(is_sleeping, number_sleeping, id)
][, ":="(check = sum(freq_sleep_by_id)), 
  by = list(is_sleeping, id)
][, ":="(check2 = sum(freq_id_by_sleep)), 
  by = list(is_sleeping, number_sleeping)]


ggplot(sim_data_str2_d_m) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, group = id),
            linewidth = 1) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  background_grid()

test_null_combined <- rbind(sim_data_d_m |> mutate(try = 1),
                            sim_data_str_d_m |> mutate(try = 2),
                            sim_data_str2_d_m |> mutate(try = 3))


ggplot(test_null_combined) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = as.factor(try), group = interaction(try, id)),
            linewidth = 1) +
  ylab("Probability") +
  facet_wrap(~is_sleeping) +
  background_grid()

# check if shift leads to null model
sequence <- sapply(0.7, FUN = function(x) {
  rbinom(n / r, 1, x) %>>%
    rep(each = r)})

sequence2 <- sapply(0.5, FUN = function(x) {
  rbinom(n / r, 1, x) %>>%
    rep(each = r)})

test <- data.table()
for(shift in 1:1000) {
  print(shift)
  test <- rbind(test, table(head(sequence, -shift) + tail(sequence, -shift)))
}

test_m <- copy(test)[, ":="(V1 = as.factor(V1),
                      N = as.numeric(N))
               ][, ":="(shift = rep(1:(.N/3), each = 3))
                 ][, ":="(tot = sum(N)), by = shift
                   ][, ":="(prop = N/tot)]

ggplot(test_m) +
  geom_line(aes(shift, prop, color = V1)) +
  geom_hline(aes(yintercept = 0.7*0.7))







# Null model pairwise interactions ----
## find data associations ---
# for nighttime
id_behvaiour <- combined_m[, ids, with = F]
result_syncrony <- data.frame()
for(i in 1:length(idsRanked)) {
  print(i)
  for(j in 1:length(idsRanked)) {
    print(j)
    result_syncrony <- rbind(result_syncrony, 
                             data.frame(id = colnames(id_behvaiour)[i], 
                                        id_pair = colnames(id_behvaiour)[j]) |>
                               cbind((id_behvaiour[, ..i] + id_behvaiour[, ..j])[, .(tot = .N), 
                                                                                 by = eval(colnames(id_behvaiour)[i])] |>
                                       setnames(old = colnames(id_behvaiour)[i], 
                                                new = "syncrony")))
  }
}

ggplot(as.data.table(result_syncrony)[syncrony == 1
][, ":="(id = factor(id, 
                     levels = idsRanked_wrong_names),
         id_pair = factor(id_pair, 
                     levels = idsRanked_wrong_names))
][order(id, id_pair)])  +
  geom_point(aes(id, tot)) +
  geom_smooth(aes(id, tot, group = id_pair), 
              method = "lm") +
  facet_wrap(~id_pair) +
  background_grid()

ggsave("figures/test/trinity_bug/syncro.png",
       bg = "white",
       height = 15,
       width = 15)

# for daytime and night
# id_behvaiour_day <- combined_night_m[order(dateTime)][is_night == T][, ids, with = F]
# result_syncrony_day <- data.frame()
# for(i in 1:length(idsRanked)) {
#   print(i)
#   for(j in 1:length(idsRanked)) {
#     print(j)
#     result_syncrony_day <- rbind(result_syncrony_day, 
#                                  data.frame(id = colnames(id_behvaiour_day)[i], 
#                                             id_pair = colnames(id_behvaiour_day)[j]) |>
#                                    cbind(
#                                      setnames(cbind(id_behvaiour_day[, ..i] + id_behvaiour_day[, ..j],
#                                                     combined_night_m[order(dateTime)][is_night == T][, "id_nightday"]),
#                                               old = colnames(id_behvaiour_day)[i], 
#                                               new = "syncrony")[, .(tot = .N), 
#                                                                 by = list(id_nightday, syncrony)]
#                                    ))
#   }
# } 
# 
# ggplot(as.data.table(result_syncrony_day)[syncrony == 1])  +
#   geom_point(aes(id, tot, color = as.factor(id_nightday))) +
#   geom_smooth(aes(id, tot, group = id_pair), 
#               method = "lm") +
#   facet_wrap(~id_pair) +
#   background_grid()

result_syncrony_m <- (as.data.table(result_syncrony)[!is.na(syncrony), ":="(coocurrences = sum(tot)), 
                                                     by = list(id, id_pair)
][, ":="(association = tot/coocurrences)
][, ':='(id = case_when(id == "Sheb" ~"Shebeleza",
                        T ~ id))
][, ':='(id_pair = case_when(id_pair == "Sheb" ~"Shebeleza",
                             T ~ id_pair))
] |> #[id != id_pair][order(syncrony, association, id, id_pair) # keep only one of the symmetric interactions that are present][seq(1, nrow(result_syncrony), by = 2)]
  merge(dominance_id |> rename(dominance_id = dominance, sex_id = sex), 
        by = "id",
        all.x = TRUE) |>
  merge(dominance_id |> rename(dominance_pair = dominance, sex_pair = sex), 
        by.x = "id_pair",
        by.y = "id",
        all.x = TRUE))[, ":="(id = factor(id, levels = idsRanked),
                              id_pair = factor(id_pair, levels = idsRanked))
        ][, ":="(test = sum(association)),
          by = list(id, id_pair)
        ]

ggplot(result_syncrony_m[syncrony == 1])  +
  geom_point(aes(id, association)) +
  geom_smooth(aes(id, association, group = id_pair), 
              method = "lm") +
  facet_wrap(~id_pair) +
  background_grid()

ggsave(here("figures", "test", "t0.03.png"), #"freq-sleep~dominance_INT_sex.png"
       height = 10,
       width = 10,
       bg = "white")

## Create null model ----
null_model_components <- result_syncrony_m[id == id_pair & syncrony == 2
                                           ][, c("id", "coocurrences", "tot", "association")] |>
  rename(n_sleep = tot, tot = coocurrences, prob_sleep = association) |>
  mutate(id = factor(id, levels = idsRanked)) |>
  arrange(id)

# checks
ggplot(null_model_components |>
         merge(dominance_id)) +
  geom_point(aes(dominance, prob_sleep))

ggplot(night_sleep) +
  geom_point(aes(dominance, mean_freq_sleep)) 

observed_data <- result_syncrony_m[, c("id", "id_pair", "syncrony", "tot", "association")] |> #[id != id_pair]
  rename(count = tot, prob = association) 

lk_data <- (as.data.table(expand.grid(id = null_model_components$id, 
                                      id_pair = null_model_components$id,
                                      possible_number_of_events = seq(0, max(null_model_components$tot), 100))) |> # [id != id_pair]
              merge(null_model_components[, -c("tot")] |>
                      rename(id_pair = id, 
                             n_sleep_pair = n_sleep, 
                             prob_sleep_pair = prob_sleep), 
                    by = "id_pair") |>
              merge(null_model_components, 
                    by = "id"))[, ":="(neg_lk_both_sleep = -dbinom(x = possible_number_of_events, 
                                                                   size = tot,
                                                                   prob = ifelse(id != id_pair, 
                                                                                 prob_sleep * prob_sleep_pair,
                                                                                 prob_sleep), 
                                                                   log = T),
                                       neg_lk_both_awake = -dbinom(x = possible_number_of_events, 
                                                                   size = tot,
                                                                   prob = ifelse(id != id_pair, 
                                                                                 (1 - prob_sleep) * (1 - prob_sleep_pair),
                                                                                 1 - prob_sleep),
                                                                   log = T),
                                       neg_lk_both_asyncro = ifelse(id != id_pair,
                                                                    -dbinom(x = possible_number_of_events, 
                                                                            size = tot,
                                                                            prob = 1 - (prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair), 
                                                                            log = T),
                                                                    NA),
                                       neg_lk_both_syncro = ifelse(id != id_pair,
                                                                   -dbinom(x = possible_number_of_events, 
                                                                           size = tot,
                                                                           prob = prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair, 
                                                                           log = T),
                                                                   NA),
                                       # find 95% confidence intervals
                                       low_both_sleep = qbinom(p = 0.025, 
                                                               size = tot, 
                                                               prob = ifelse(id != id_pair, 
                                                                             prob_sleep * prob_sleep_pair,
                                                                             prob_sleep), 
                                                               lower.tail = TRUE),
                                       high_both_sleep = qbinom(p = 0.975, 
                                                                size = tot, 
                                                                prob = ifelse(id != id_pair, 
                                                                              prob_sleep * prob_sleep_pair,
                                                                              prob_sleep), 
                                                                lower.tail = TRUE),
                                       low_both_awake = qbinom(p = 0.05, 
                                                               size = tot, 
                                                               prob = ifelse(id != id_pair, 
                                                                             (1 - prob_sleep) * (1 - prob_sleep_pair),
                                                                             1 - prob_sleep), 
                                                               lower.tail = TRUE),
                                       high_both_awake = qbinom(p = 0.975, 
                                                                size = tot, 
                                                                prob = ifelse(id != id_pair, 
                                                                              (1 - prob_sleep) * (1 - prob_sleep_pair),
                                                                              1 - prob_sleep), 
                                                                lower.tail = TRUE),
                                       low_both_asyncro = qbinom(p = 0.05, 
                                                                 size = tot, 
                                                                 prob =  1 - (prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair), 
                                                                 lower.tail = TRUE),
                                       high_both_asyncro = qbinom(p = 0.975, 
                                                                  size = tot, 
                                                                  prob =  1 - (prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair), 
                                                                  lower.tail = TRUE),
                                       low_both_syncro = qbinom(p = 0.05, 
                                                                size = tot, 
                                                                prob = prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair, 
                                                                lower.tail = TRUE),
                                       high_both_syncro = qbinom(p = 0.975, 
                                                                 size = tot, 
                                                                 prob = prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair, 
                                                                 lower.tail = TRUE))] |>
  merge(observed_data[syncrony == 2, c("id", "id_pair", "count")] |>
          rename(data_both_sleep = count),
        by = c("id", "id_pair")) |>
  merge(observed_data[syncrony == 0, c("id", "id_pair", "count")] |>
          rename(data_both_awake = count),
        by = c("id", "id_pair")) |>
  merge(observed_data[syncrony == 1, c("id", "id_pair", "count")] |>
          rename(data_both_asyncro = count),
        by = c("id", "id_pair"),
        all.x = T) |>
  merge(observed_data[syncrony != 1 & id != id_pair
  ][, .(data_both_syncro = sum(count)),
    by = c("id", "id_pair")],
  all.x = T) |>
  mutate(obs_neg_lk_both_sleep = -dbinom(x = data_both_sleep, 
                                         size = tot,
                                         prob = ifelse(id != id_pair, 
                                                       prob_sleep * prob_sleep_pair,
                                                       prob_sleep), 
                                         log = T),
         obs_neg_lk_both_awake = -dbinom(x = data_both_awake, 
                                         size = tot,
                                         prob = ifelse(id != id_pair, 
                                                       (1 - prob_sleep) * (1 - prob_sleep_pair),
                                                       1 - prob_sleep), 
                                         log = T),
         obs_neg_lk_both_asyncro = -dbinom(x = data_both_asyncro, 
                                           size = tot,
                                           prob = 1 - (prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair), 
                                           log = T),
         obs_neg_lk_both_syncro = -dbinom(x = data_both_syncro, 
                                          size = tot,
                                          prob = prob_sleep * (1 - prob_sleep_pair) + (1 - prob_sleep) * prob_sleep_pair, 
                                          log = T)) |>
  group_by(id, id_pair) |>
  mutate(
    # null_expectation_sleep = possible_number_of_events[which(neg_lk_both_sleep == min(neg_lk_both_sleep))], # this is the null expectation
    #      null_expectation_awake = possible_number_of_events[which(neg_lk_both_awake == min(neg_lk_both_awake))],
    #      null_expectation_asyncro = ifelse(id != id_pair,
    #                                        possible_number_of_events[which(neg_lk_both_asyncro == min(neg_lk_both_asyncro))],
    #                                        NA),
    #      null_expectation_syncro = ifelse(id != id_pair,
    #                                       data_both_syncro / possible_number_of_events[which(neg_lk_both_syncro == min(neg_lk_both_syncro))],
    #                                       NA),
    null_expectation_sleep = tot * prob_sleep * prob_sleep_pair,
    null_expectation_awake = tot * (1 - prob_sleep) * (1 - prob_sleep_pair),
    null_expectation_asyncro =  ifelse(id != id_pair,
                                       tot * ((1 - prob_sleep) * (prob_sleep_pair) + (prob_sleep) * (1 - prob_sleep_pair)),
                                       NA),
    null_expectation_syncro = ifelse(id != id_pair,
                                     tot  * ((1 - prob_sleep) * (1 - prob_sleep_pair) + (prob_sleep) * (prob_sleep_pair)),
                                     NA),
    ratio_both_sleep = data_both_sleep / null_expectation_sleep,
    ratio_both_awake = data_both_awake / null_expectation_awake,
    ratio_both_asyncro = ifelse(id != id_pair,
                                data_both_asyncro / null_expectation_asyncro,
                                NA),
    ratio_both_syncro = ifelse(id != id_pair,
                               data_both_syncro / null_expectation_syncro,
                               NA),
    data_state_sleep = case_when(data_both_sleep < low_both_sleep ~ "Lower",
                                 data_both_sleep > high_both_sleep ~ "Higher",
                                 T ~ "non-significant"),
    data_state_awake = case_when(data_both_awake < low_both_awake ~ "Lower",
                                 data_both_awake > high_both_awake ~ "Higher",
                                 T ~ "non-significant"),
    data_state_asyncro = case_when(data_both_asyncro < low_both_asyncro ~ "Lower",
                                   data_both_asyncro > high_both_asyncro ~ "Higher",
                                   T ~ "non-significant"),
    data_state_syncro = case_when(data_both_syncro < low_both_syncro ~ "Lower",
                                  data_both_syncro > high_both_syncro ~ "Higher",
                                  T ~ "non-significant")) |>
  as.data.table()

ggplot(lk_data) +
  geom_line(aes(possible_number_of_events, neg_lk_both_asyncro)) +
  geom_hline(aes(yintercept = obs_neg_lk_both_asyncro), color = "red", lty = "dashed") +
  geom_vline(aes(xintercept = data_both_asyncro), color = "blue", lty = "dashed") +
  # geom_rect(data = lk_data[, .(low_both_asyncro = unique(low_both_asyncro),
  #                              high_both_asyncro = unique(high_both_asyncro)), 
  #                          by = list(id, id_pair)],
  #           aes(xmin = low_both_asyncro, xmax = high_both_asyncro, 
  #               ymin = 0, ymax = 1250000), 
  #           alpha = 0.3, color = NA, fill = "black") +
  geom_point(data = lk_data[, .(data_both_asyncro = unique(data_both_asyncro),
                                obs_neg_lk_both_asyncro = unique(obs_neg_lk_both_asyncro),
                                data_state_asyncro = unique(data_state_asyncro)), 
                            by = list(id, id_pair)],
             aes(data_both_asyncro, obs_neg_lk_both_asyncro, 
                 color = data_state_asyncro), 
            size = 4) +
  scale_color_viridis_d(name = "") +
  ylab("Negative log likelihood") +
  xlab("Number of asynchronous events") +
  facet_grid(cols = vars(id),
             rows = vars(id_pair)) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "null_model_asyncro.png"),
       height = 25,
       width = 30,
       bg = "white")  

ggplot(lk_data) +
  geom_line(aes(possible_number_of_events, neg_lk_both_sleep)) +
  geom_hline(aes(yintercept = obs_neg_lk_both_sleep), color = "red", lty = "dashed") +
  geom_vline(aes(xintercept = data_both_sleep), color = "blue", lty = "dashed") +
  # geom_rect(data = lk_data[, .(low_both_sleep = unique(low_both_sleep),
  #                              high_both_sleep = unique(high_both_sleep)), 
  #                          by = list(id, id_pair)],
  #           aes(xmin = low_both_sleep, xmax = high_both_sleep, 
  #               ymin = 0, ymax = 1250000), 
  #           alpha = 0.3, color = NA, fill = "black") +
  geom_point(data = lk_data[, .(data_both_sleep = unique(data_both_sleep),
                                obs_neg_lk_both_sleep = unique(obs_neg_lk_both_sleep),
                                data_state_sleep = unique(data_state_sleep)), 
                            by = list(id, id_pair)],
             aes(data_both_sleep, obs_neg_lk_both_sleep, 
                 color = data_state_sleep), 
             size = 4) +
  scale_color_viridis_d(name = "") +
  ylab("Negative log likelihood") +
  xlab("Number of synchronous sleeping events") +
  facet_grid(cols = vars(id),
             rows = vars(id_pair)) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "null_model_sleep.png"),
       height = 30,
       width = 25,
       bg = "white")

ggplot(lk_data) +
  geom_line(aes(possible_number_of_events, neg_lk_both_awake)) +
  geom_hline(aes(yintercept = obs_neg_lk_both_awake), color = "red", lty = "dashed") +
  geom_vline(aes(xintercept = data_both_awake), color = "blue", lty = "dashed") +
  # geom_rect(data = lk_data[, .(low_both_awake = unique(low_both_awake),
  #                              high_both_awake = unique(high_both_awake)), 
  #                          by = list(id, id_pair)],
  #           aes(xmin = low_both_awake, xmax = high_both_awake, 
  #               ymin = 0, ymax = 1250000), 
  #           alpha = 0.3, color = NA, fill = "black") +
  geom_point(data = lk_data[, .(data_both_awake = unique(data_both_awake),
                                obs_neg_lk_both_awake = unique(obs_neg_lk_both_awake),
                                data_state_awake = unique(data_state_awake)), 
                            by = list(id, id_pair)],
             aes(data_both_awake, obs_neg_lk_both_awake, 
                 color = data_state_awake), 
             size = 4) +
  scale_color_viridis_d(name = "") +
  ylab("Negative log likelihood") +
  xlab("Number of synchronous awake events") +  
  facet_grid(cols = vars(id),
             rows = vars(id_pair)) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "null_model_awake.png"),
       height = 30,
       width = 30,
       bg = "white")

ggplot(lk_data) +
  geom_line(aes(possible_number_of_events, neg_lk_both_syncro)) +
  geom_hline(aes(yintercept = obs_neg_lk_both_syncro), color = "red", lty = "dashed") +
  geom_vline(aes(xintercept = data_both_syncro), color = "blue", lty = "dashed") +
  # geom_rect(data = lk_data[, .(low_both_syncro = unique(low_both_syncro),
  #                              high_both_syncro = unique(high_both_syncro)), 
  #                          by = list(id, id_pair)],
  #           aes(xmin = low_both_syncro, xmax = high_both_syncro, 
  #               ymin = 0, ymax = 1250000), 
  #           alpha = 0.3, color = NA, fill = "black") +
  geom_point(data = lk_data[, .(data_both_syncro = unique(data_both_syncro),
                                obs_neg_lk_both_syncro = unique(obs_neg_lk_both_syncro),
                                data_state_syncro = unique(data_state_syncro)),
                            by = list(id, id_pair)],
             aes(data_both_syncro, obs_neg_lk_both_syncro,
                 color = data_state_syncro),
             size = 4) +
  scale_color_viridis_d(name = "") +
  ylab("Negative log likelihood") +
  xlab("Number of synchronous events") +
  facet_grid(cols = vars(id),
             rows = vars(id_pair)) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "null_model_syncro.png"),
       height = 30,
       width = 25,
       bg = "white")


## Create and visualize network ----
lk_network <- (lk_data[, .(nlk_sleep = unique(obs_neg_lk_both_sleep),
                           nlk_awake = unique(obs_neg_lk_both_awake),
                           nlk_asyncro = unique(obs_neg_lk_both_asyncro),
                           nlk_syncro = unique(obs_neg_lk_both_syncro),
                           ratio_sleep = unique(ratio_both_sleep),
                           ratio_awake = unique(ratio_both_awake),
                           ratio_asyncro = unique(ratio_both_asyncro),
                           ratio_syncro = unique(ratio_both_syncro)),
                       by = list(id, id_pair)
] |>
  merge(dominance_id,
        by = "id") |>
  merge(dominance_id |> rename(id_pair = id, sex_pair = sex, dominance_pair = dominance),
        by = "id_pair") |>
  pivot_longer(cols = matches("nlk|ratio"),
               names_to = c(".value", "syncrony"),
               names_pattern = "(.+)_(.+)") |>
  as.data.table())[, ":="(id = factor(id, levels = idsRanked),
                          id_pair = factor(id_pair, levels = idsRanked))]

ggplot(lk_network) +
  geom_point(aes(dominance_pair, nlk, color = syncrony)) +
  geom_smooth(aes(dominance_pair, nlk, color = syncrony),
              method = "lm") +
  scale_color_discrete(name = "") +
  facet_wrap(~id) +
  background_grid()

ggplot(lk_network) +
  geom_point(aes(dominance_pair, ratio, color = syncrony)) +
  geom_smooth(aes(dominance_pair, ratio, color = syncrony),
              method = "lm") +
  scale_color_discrete(name = "") +
  facet_wrap(~id) +
  background_grid()

# Asynchro
ggplot(lk_network[syncrony == "asyncro"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  facet_wrap(~sex) +
  ggtitle("Asynchrony") +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "asyncro~dominance_pair_TIMES_dominance_id_null_model.png"),
       height = 5,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "asyncro"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  facet_wrap(~sex) +
  ggtitle("Asynchrony") +
  background_grid()

ggplot(lk_network[syncrony == "asyncro"]) +
  geom_point(aes(dominance_pair, ratio, color = id_pair, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = id_pair, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~id) +
  ggtitle("Asynchrony") +
  background_grid()

# synchro
ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  facet_wrap(~sex) +
  ggtitle("Synchrony") +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "syncro~dominance_pair_TIMES_dominance_id_null_model.png"),
       height = 5,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  facet_wrap(~sex) +
  ggtitle("Synchrony") +
  background_grid()

ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance), size = 2) +
  #geom_line(aes(dominance_pair, ratio, color = dominance, group = id, lty = sex)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2, fullrange = T,
              se = F) +
  scale_color_viridis_c() +
  ylab("Observed synchrony over null model syncrony") +
  facet_wrap(~id) +
  #ggtitle("Synchrony") +
  background_grid()

ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance), size = 2) +
  #geom_line(aes(dominance_pair, ratio, color = dominance, group = id, lty = sex)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2, fullrange = T,
              se = F) +
  scale_color_viridis_c() +
  ylab("Observed synchrony over null model syncrony") +
  facet_wrap(~id) +
  #ggtitle("Synchrony") +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "syncro_ratio~dominance_pair_TIMES_dominance_id_null_model.png"),
       height = 5,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "syncro"]) +
  geom_point(aes(dominance_pair, ratio, color = id_pair, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = id_pair, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~id) +
  ggtitle("Synchrony") +
  background_grid()

# Both sleep
ggplot(lk_network[syncrony == "sleep"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both sleeping") +
  facet_wrap(~id) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "both_sleep~dominance_pair_null_model.png"),
       height = 7,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "sleep"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both sleeping") +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "both_sleep~dominance_pair_TIMES_dominance_id_null_model.png"),
       height = 5,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "sleep"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both sleeping") +
  facet_wrap(~id) +
  background_grid()

# Both awake
ggplot(lk_network[syncrony == "awake"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both awake") +
  facet_wrap(~id) +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "both_awake~dominance_pair_null_model.png"),
       height = 7,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "both_awake"]) +
  geom_point(aes(dominance_pair, nlk, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, nlk, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both awake") +
  background_grid()

ggsave(here("figures", "sleep", "null_model", "both_awake~dominance_pair_TIMES_dominance_id_null_model.png"),
       height = 5,
       width = 7,
       bg = "white")

ggplot(lk_network[syncrony == "awake"]) +
  geom_point(aes(dominance_pair, ratio, color = dominance, group = id)) +
  geom_smooth(aes(dominance_pair, ratio, color = dominance, group = id),
              method = "lm", linewidth = 2,
              se = F) +
  scale_color_viridis_c() +
  ggtitle("Both awake") +
  facet_wrap(~id) +
  background_grid()


### Plot network ----
net_null_model <- as_tbl_graph(lk_network[id != id_pair
                                          ][, ":="(from = as.character(id),
                                                     to = as.character(id_pair))
                                            ][, c("from", "to", "ratio", "syncrony")],
  nodes = copy(dominance_id)[, ":="(id = as.character(id))]) |>
  activate(nodes) |>
  left_join(dominance_id[, ":="(name = as.character(id))][, -c("id")])

ggraph(net_null_model |>   
         activate(edges)  |>
         filter(syncrony == "syncro",
                ratio > quantile(ratio, probs = 0)),
       layout = "stress") +#"kk"
  geom_edge_link(
    aes(color = as.factor(ratio),
        width = ratio)) + 
  scale_edge_color_manual(values = viridis(n = 263)) + 
  geom_node_point(
    aes(#fill = as.factor(node_number),
      # fill = as.factor(dominance),
      size = dominance), 
    #size = 8,
    fill = "black",
    color = "black", shape = 21) +
  scale_size_continuous(range = c(5, 15)) +
  #scale_fill_viridis_d(option = "viridis") +
  geom_node_label(
    aes(label = name),
    nudge_y = 0.05
  ) +
  ggtitle("Sleep") +
  theme(legend.position = 'none')

ggsave(here("figures", "sleep", "syncrony", "net_sleep_syncro.png"),
       height = 10,
       width = 10,
       bg = "white")

## Stats ----
glm_coordination <- glm(data = result_syncrony_m[id != id_pair][syncrony == 1] |>
                          merge(null_model_components[, c("id", "prob_sleep")]) |>
                          merge(null_model_components[, c("id", "prob_sleep")] |>
                                  rename(id_pair = id, prob_sleep_pair = prob_sleep),
                                by = "id_pair"), # here should be the ratio data ? 
                        association ~ dominance_id * dominance_pair + sex_id + sex_pair + prob_sleep_pair + prob_sleep,
                        weights = coocurrences)
summary(glm_coordination)

model_synchro <- lm(data = lk_network[id != id_pair][syncrony %in% c("syncro")
] |> merge(dominance_id), # here should be the ratio data ? 
ratio ~ dominance * dominance_pair)
summary(model_synchro)



# Null model for probability n are simultaneously sleeping P(N) ----
## Null expectation ----
data_n_sleeping <- combined_m[number_individuals == 12
][, ":="(tot = .N)
][, .(freq_number_sleeping = .N / unique(tot),
      tot = unique(tot),
      n_obs = .N), 
  by = number_sleeping]

{
  prob_ind_sleep <- null_model_components[order(id)]$prob_sleep
  prob_ind_awake <- 1 - prob_ind_sleep
  null_n_simultaneously_sleeping <- data.table(n = 0:12, prob_sleep = 0, is_sleeping = "Sleeping")
  for(i in 1:nrow(null_n_simultaneously_sleeping)) { # every possible n choose i, meaning combinations of i baboons simultaneously sleeping
    null_n_simultaneously_sleeping[i, "prob_sleep"] <- sum(apply(combn(12, i - 1), # create all combinations of i individuals simultaneously sleeping possible. i - 1 bc I start from n sleeping == 0
                                                                 2, # in the rows is the individuals selected, in the colums one specific combination of individuals
                                                                 FUN = function(chosen_individuals_sleeping) {
                                                                   output <- prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-chosen_individuals_sleeping]]) # since it is independent, I have to multiply the probability that each individual is in the specific state
                                                                   if(length(chosen_individuals_sleeping) == 0) {
                                                                     output <- prod(prob_ind_awake[1:12])
                                                                   }
                                                                   return(output)
                                                                 }))
  }
  # null_n_simultaneously_sleeping[1, 2] <- prod(prob_ind_awake)
  # null_n_simultaneously_sleeping[13, 2] <- prod(prob_ind_sleep)
  null_n_simultaneously_sleeping <- copy(null_n_simultaneously_sleeping)[, ":="(check_null = sum(prob_sleep)) # by reference
  ] |>
    merge(data_n_sleeping, by.x = "n", by.y = "number_sleeping") |>
    mutate(diff = freq_number_sleeping  - prob_sleep,
           avg_null = sum(prob_sleep * n))
}

ggplot(null_n_simultaneously_sleeping) +
  geom_line(aes(n, prob_sleep), 
            linewidth = 2, lty = "dashed") +
  geom_line(aes(n, freq_number_sleeping), 
            linewidth = 2) +
  geom_vline(aes(xintercept = avg), color = "red") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  geom_vline(aes(xintercept = 6)) +
  background_grid() 

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "P(IDgivenN)", "null_P(N).png"),
       height = 10,
       width = 10,
       bg = "white")

ggplot(null_n_simultaneously_sleeping) +
  geom_line(aes(n, prob_sleep), 
            linewidth = 2, lty = "dashed") +
  geom_line(aes(n, freq_number_sleeping), 
            linewidth = 2) +
  # geom_vline(aes(xintercept = avg), color = "red") +
  # geom_vline(aes(xintercept = 6)) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_line(aes(n, n_obs), 
            linewidth = 2) +
  geom_line(aes(n, freq_number_sleeping * tot), 
            linewidth = 2, , lty = "dashed", color = "red") +
  # geom_vline(aes(xintercept = avg), color = "red") +
  # geom_vline(aes(xintercept = 6)) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
           color = "black", size =1) +
  # geom_col(aes(n, prob_sleep), fill = NA,
  #          color = "red", size = 2) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  #geom_vline(aes(xintercept = 6)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
           color = "black", size =1) +
  geom_col(aes(n, prob_sleep), fill = NA,
           color = "red", size = 2) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  #geom_vline(aes(xintercept = 6)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
           color = "black", size =2) +
  geom_col(aes(n, prob_sleep), fill = NA,
           color = "red", size = 2) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  #geom_vline(aes(xintercept = 6)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_col(aes(n, freq_number_sleeping), fill = "black", alpha = 0.1,
           color = "black") +
  geom_segment(aes(xend = n, x = n, 
                   y = freq_number_sleeping, 
                   yend = freq_number_sleeping - diff), 
           color = "red", size = 2, arrow = arrow(length = unit(0.5, "cm"))) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  #geom_vline(aes(xintercept = 6)) +
  background_grid() 

ggplot(null_n_simultaneously_sleeping) +
  geom_line(aes(n, diff)) +
  geom_hline(aes(yintercept = 0))

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "P(IDgivenN)", "null_P(N)_diff.png"),
       height = 10,
       width = 10,
       bg = "white")

# Null model for probability n are simultaneously awake P(N) ----
## Null expectation ----
{
  prob_ind_sleep <- null_model_components[order(id)]$prob_sleep
  prob_ind_awake <- 1 - prob_ind_sleep
  null_n_simultaneously_awake <- data.table(n = 0:12, prob_awake = 0, is_sleeping = "Awake")
  for(i in 1:nrow(null_n_simultaneously_sleeping)) { # every possible n choose i, meaning combinations of i baboons simultaneously sleeping
    null_n_simultaneously_awake[i, "prob_awake"] <- sum(apply(combn(12, i - 1), # create all combinations of i individuals simultaneously awake possible. i - 1 bc I start from n awake == 0
                                                                 2, # in the rows is the individuals selected, in the colums one specific combination of individuals
                                                                 FUN = function(chosen_individuals_awake) {
                                                                   output <- prod(prob_ind_awake[chosen_individuals_awake]) * prod(prob_ind_sleep[(1:12)[-chosen_individuals_awake]]) # since it is independent, I have to multiply the probability that each individual is in the specific state
                                                                   if(length(chosen_individuals_awake) == 0) {
                                                                     output <- prod(prob_ind_sleep[1:12])
                                                                   }
                                                                   return(output)
                                                                 }))
  }
  # null_n_simultaneously_sleeping[1, 2] <- prod(prob_ind_awake)
  # null_n_simultaneously_sleeping[13, 2] <- prod(prob_ind_sleep)
  null_n_simultaneously_awake <- copy(null_n_simultaneously_awake)[, ":="(check_null = sum(prob_awake)) # by reference
  ] |>
    merge(data_n_awake, by.x = "n", by.y = "number_awake") |>
    mutate(diff = freq_number_awake  - prob_awake,
           avg_null = sum(prob_awake * n))
}

ggplot(null_n_simultaneously_awake) +
  geom_line(aes(n, prob_awake), 
            linewidth = 2, lty = "dashed") +
  geom_line(aes(n, freq_number_awake), 
            linewidth = 2) +
  geom_vline(aes(xintercept = 6)) +
  background_grid() +
  geom_vline(aes(xintercept = avg), color = "red") 

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "P(IDgivenN)", "null_P(N).png"),
       height = 10,
       width = 10,
       bg = "white")

ggplot(null_n_simultaneously_awake) +
  geom_line(aes(n, diff)) +
  geom_hline(aes(yintercept = 0))

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "P(IDgivenN)", "null_P(N)_diff.png"),
       height = 10,
       width = 10,
       bg = "white")

# Combine ----
null_n_simulataneous <- null_n_simultaneously_awake |> 
  rename(freq_data = freq_number_awake,
         freq_null = prob_awake) |> 
  rbind(null_n_simultaneously_sleeping |>
          rename(freq_data = freq_number_sleeping,
                 freq_null = prob_sleep)) 

ggplot(null_n_simulataneous) +
  geom_line(aes(n, freq_null, color = is_sleeping), linewidth = 3, lty = "dashed") +
  geom_line(aes(n, freq_data, color = is_sleeping), linewidth = 3) +
  geom_vline(aes(xintercept = 6)) +
  background_grid() # it inverts at precisely half
  

## Likelihood of data ----
combinatorics_data <- data.table(number_sleeping = 0:12, # pre-calculate this (I could also have used it for the other scenarios but it was not necessary speedwise)
                                 nllk = map(0:12, 
                                            function(number_sleeping) {
                                              apply(combn(12, number_sleeping), # create all combinations of individuals simultaneously sleeping possible.
                                                    2, # in the rows is the individuals selected, in the colums one specific combination of individuals
                                                    FUN = function(chosen_individuals_sleeping) {
                                                      output <- prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-chosen_individuals_sleeping]]) # since it is independent, I have to multiply the probability that each individual is in the specific state
                                                      if(length(chosen_individuals_sleeping) == 0) {
                                                        output <- prod(prob_ind_awake[1:12])
                                                      }
                                                      return(output)
                                                    }) |> 
                                                sum() |> 
                                                log() * -1
                                            }) |> 
                                   unlist())

ggplot(combinatorics_data) +
  geom_line(aes(number_sleeping, nllk)) +
  background_grid()


lk_n_sleeping <- data.table(number_sleeping = combined_m$number_sleeping,
                            nllk = combinatorics_data$nllk[combined_m$number_sleeping + 1]) |> # + 1 bc combined_m$number_sleeping starts from zero
                 mutate(total_nllk = sum(nllk))

# How to find confidence intervals ???
                          
# Null model for probability ID is sleeping when other N are doing so P(ID|N) ----
## calculate null expectation (use of Bayes theorem) ----
null_id_n_sleeping <- expand.grid(id = null_model_components$id, number_sleeping = 1:12) |> # possible number of individuals sleeping goes from 1 to 12. It is impossible that P(N = 0|sleeping)
  mutate(lk = -1,
         is_sleeping = "Sleeping") |> as.data.table() |>
  merge(null_model_components, on = "id") |>
  arrange(number_sleeping, id) |>
  mutate(prob_awake = 1 - prob_sleep)
null_id_n_awake <- expand.grid(id = null_model_components$id, number_sleeping = 0:11) |> # possible number of individuals sleeping goes from 1 to 12. It is impossible that P(N = 0|sleeping)
  mutate(lk = -1,
         is_sleeping = "Awake") |> as.data.table() |>
  merge(null_model_components, on = "id") |>
  arrange(number_sleeping, id) |>
  mutate(prob_awake = 1 - prob_sleep)

# use Bayes theorem: P(sleeping|N) = (P(sleeping)P(N|sleeping))/P(N) P(N) is the tricky one
for(i in 1:nrow(null_id_n_sleeping)) { # calculate likelihoods
  focus_id <- null_id_n_sleeping$id[i] |> as.numeric()
  
  # for sleep
  Nsleep <- null_id_n_sleeping$number_sleeping[i]
  null_id_n_sleeping[i, "lk"] <- sum(apply(combn((1:12)[-focus_id], # P(N|sleeping)
                                                 Nsleep-1), 
                                           2, 
                                           FUN = function(chosen_individuals_sleeping) { 
                                             prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
                                             ) 
                                           }))
  
  # for awake
  Nawake <- null_id_n_awake$number_sleeping[i]
  null_id_n_awake[i, "lk"] <- sum(apply(combn((1:12)[-focus_id], # P(N|sleeping)
                                              Nawake), 
                                           2, 
                                           FUN = function(chosen_individuals_sleeping) { 
                                             prod(prob_ind_sleep[chosen_individuals_sleeping]) * prod(prob_ind_awake[(1:12)[-c(chosen_individuals_sleeping, focus_id)]]
                                             ) 
                                           }))
}
# calculate remaining ingredients for Bayes formula 
null_id_n_sleeping_m <- copy(null_id_n_sleeping)[, ":="(p_data = sum(prob_sleep * lk)),
                                                 by = number_sleeping][, ":="(null_posterior = (prob_sleep * lk) / p_data)
                                                                       ][, ":="(check_null_posterior = sum(null_posterior)),
                                                                         by = number_sleeping]
null_id_n_awake_m <- copy(null_id_n_awake)[, ":="(p_data = sum(prob_awake * lk)),
                                                 by = number_sleeping][, ":="(null_posterior = (prob_awake * lk) / p_data)
                                                 ][, ":="(check_null_posterior = sum(null_posterior)),
                                                   by = number_sleeping]

quourm_sleep_null_model <- (quourm_sleep_m |>
  merge(null_id_n_sleeping_m[, c("id", "number_sleeping", "null_posterior", "is_sleeping")] |>
          rbind(null_id_n_awake_m[, c("id", "number_sleeping", "null_posterior", "is_sleeping")]),
        by = c("id", "number_sleeping", "is_sleeping")))[, ":="(diff = freq_id_by_sleep - null_posterior,
                                                                ratio = freq_id_by_sleep/null_posterior)]

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, freq_id_by_sleep, color = dominance, group = id),
            linewidth = 1) +
  geom_line(aes(number_sleeping, null_posterior, color = dominance, group = id),
            linewidth = 1, lty = "dashed") +
  ylab("Probability") +
  scale_color_viridis_c(option = "viridis") +
  facet_grid(cols = vars(id), row = vars(is_sleeping)) +
  background_grid()

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "diff_null_data~number_sleeping_by_id2.png"),
       height = 10,
       width = 10,
       bg = "white")

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, diff, color = dominance, group = id),
            linewidth = 1) +
  ylab("Difference") +
  scale_color_viridis_c(option = "viridis") +
  facet_grid(cols = vars(id), row = vars(is_sleeping)) +
  background_grid()

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, diff, color = dominance, group = id),
            linewidth = 1) +
  ylab("Difference") +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~is_sleeping) +
  background_grid()

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "diff_null_data~number_sleeping_by_id.png"),
       height = 10,
       width = 10,
       bg = "white")

ggplot(quourm_sleep_null_model) +
  geom_point(aes(number_sleeping, diff, color = dominance, group = id),
            size = 1, alpha = 0.3) +
  geom_smooth(aes(number_sleeping, diff, color = dominance, group = id),
              method = "lm") +
  ylab("Difference") +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~is_sleeping) +
  background_grid()

ggplot(quourm_sleep_null_model) +
  geom_point(aes(number_sleeping, ratio, color = dominance, group = id),
             size = 1, alpha = 0.3) +
  geom_smooth(aes(number_sleeping, ratio, 
                  color = dominance, group = id,
                  weight = count), se = F,
              method = "lm") +
  ylab("Difference") +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~is_sleeping) +
  background_grid()

ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, ratio, color = dominance, group = id),
            linewidth = 1) +
  ylab("Difference") +
  geom_hline(aes(yintercept = 1), size = 2, lty = "dotted") +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~is_sleeping) +
  background_grid()

ggsave(here("figures", "mean.VeBDAs threshold 0.03", "ratio_null_data~number_sleeping_by_id.png"),
       height = 10,
       width = 10,
       bg = "white")


ggplot(quourm_sleep_null_model) +
  geom_line(aes(number_sleeping, ratio, color = is_sleeping, group = is_sleeping),
            linewidth = 1) +
  ylab("ratio") +
  geom_hline(aes(yintercept = 1), size = 2, lty = "dotted") +
  facet_wrap(~id) +
  background_grid()


## Calculate likelihood of data ----


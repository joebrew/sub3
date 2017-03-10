library(tidyverse)

# Function evaluate ONE day's workout
evaluate <- function(percent_at_pace = 0,
                     total_time = 100){
  time_at_pace <- (percent_at_pace / 100) * total_time
  time_slow <- total_time - time_at_pace
  cost <- total_time + time_at_pace
  benefit <- time_slow + ((time_at_pace)^1.5)
  out <- data_frame(cost = cost,
                    benefit = benefit)
  return(out)
}

# Create training week data
create_tw <- function(training_days = 4,
                      times = c(25, 25, 25, 25),
                      percents = c(50, 50, 50, 50)){
  out <- data_frame(day = 1:7,
                    time = NA,
                    percent = NA)
  # Get some indexes for training days
  the_order <- c(4,2,6,7,1,3,5)
  indexes <- the_order[1:training_days]
  out$time[indexes] <- times
  out$percent[indexes] <- percents
  
  # NAs to 0s
  out$time[is.na(out$time)] <- 0
  out$percent[is.na(out$percent)] <- 0
  
  return(out)
}

# Wrapper to evaluate week
evaluate_week <- function(week){
  results <- list()
  for (i in 1:nrow(week)){
    results[[i]] <-
      evaluate(percent_at_pace = week$percent[i],
               total_time = week$time[i])
  }
  out <- bind_rows(results)
  
  # Aggregate
  out <- out %>% summarise(cost = sum(cost), benefit = sum(benefit))
  
  return(out)
}

# What is the effect of volume
volume <- data_frame(total_time = 1:100,
                     percent_at_pace = 0)
results <- list()
for (i in 1:nrow(volume)){
  results[[i]] <- 
    evaluate(percent_at_pace = volume$percent_at_pace[i],
           total_time = volume$total_time[i])
}
final <- bind_rows(results)
volume <- bind_cols(volume, final)
# Make long
volume <- gather(volume, key, value, cost:benefit)
ggplot(data = volume,
       aes(x = total_time,
           y = value,
           group = key,
           color = key)) +
  geom_jitter(alpha = 0.6, width = 2.5) +
  labs(x = 'Time spent running',
       y = 'Value',
       title = 'The benefits of volume',
       subtitle = 'Holding constant intensity and concentration') +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  theme_bw()

# What is the effect of intensity
intensity <- data_frame(total_time = rep(50, 100),
                     percent_at_pace = 1:100)
results <- list()
for (i in 1:nrow(intensity)){
  results[[i]] <- 
    evaluate(percent_at_pace = intensity$percent_at_pace[i],
             total_time = intensity$total_time[i])
}
final <- bind_rows(results)
intensity <- bind_cols(intensity, final)
# Make long
intensity <- gather(intensity, key, value, cost:benefit)
ggplot(data = intensity,
       aes(x = percent_at_pace,
           y = value,
           group = key,
           color = key)) +
  geom_jitter(alpha = 0.6) +
  geom_line() +
  labs(x = 'Percent of running at race pace',
       y = 'Value',
       title = 'The benefits of intensity',
       subtitle = 'Holding constant weekly volume and concentration') +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  theme_bw()

# What is the effect of concentration
concentration <- data_frame(workout_days = 1:7)
results <- list()
for (i in 1:nrow(concentration)){
  total_time <- 100
  percent_at_pace <- 50
  n <- i
  # Create a week with n days
  this_week <- create_tw(training_days = n,
                                     times = rep(total_time/n, n),
                                     percents = rep(percent_at_pace, n))
  results[[i]] <- evaluate_week(this_week) %>%
    mutate(workout_days = n)
}
concentration <- bind_rows(results)
concentration <- concentration %>%
  gather(key, value, cost:benefit)
ggplot(data = concentration,
       aes(x = workout_days,
           y = value,
           group = key,
           color = key)) +
  geom_jitter(alpha = 0.6) +
  geom_line() +
  labs(x = 'Number of workout days',
       y = 'Value',
       title = 'The benefits of concentration',
       subtitle = 'Holding constant weekly volume and intensity') +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  theme_bw()

# Some examples
plot_week <- function(week){
  the_eval <- evaluate_week(week)
  week$fast_time <- (week$percent / 100) * week$time
  week$slow_time <- week$time - week$fast_time
  week <- week %>%
    dplyr::select(day, slow_time, fast_time)
  week <- gather(week, key, value, slow_time:fast_time)
  week$key <- gsub('_time', '', week$key)
  week$key <- factor(week$key, levels = c('slow', 'fast'))
  ggplot(data = week,
         aes(x = day,
             y= value,
             group = key,
             fill = key)) +
    geom_bar(stat = 'identity',
             position = 'stack') +
    theme_bw() +
    scale_fill_manual(name = 'Pace',
                      values = c('darkgreen', 'darkorange')) +
    labs(x = 'Day of week',
         y = 'Volume',
         subtitle = paste0(length(unique(week$day[week$value > 0])),
                           ' days of running, Cost = ',
                           round(the_eval$cost),
                           ', Benefit = ',
                           round(the_eval$benefit)
                           
                           ))
}

progression <- data_frame(name = c('Baseline',
                                   'LSD',
                                   'Intense',
                                   'Busy',
                                   'Optimal'),
                          cost = NA,
                          benefit = NA)
progression <- data.frame(progression)

# Get a sample week
easy_week <- create_tw(training_days = 4,
                       times = c(25, 25, 25, 25),
                       percents = c(25, 25, 25, 25))
plot_week(easy_week) +
  labs(title = 'Baseline week')
this_eval <- evaluate_week(easy_week)
progression[progression$name == 'Baseline',
            c('cost',
              'benefit')] <- as.numeric(this_eval)

# Increase volume, keep same minutes of intensity
lsd_week <- create_tw(training_days = 4,
                      times = c(50, 50, 50, 50),
                      percents = rep(12.5, 4))
plot_week(lsd_week) +
  labs(title = 'Increasing volume (LSD approach)')
this_eval <- evaluate_week(lsd_week)
progression[progression$name == 'LSD',
            c('cost',
              'benefit')] <- as.numeric(this_eval)
progression_long <- progression %>%
  gather(key, value, cost:benefit)
ggplot(data = progression_long %>% filter(!is.na(value)),
       aes(x = name,
           y = value,
           group = key,
           color = key)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  labs(x = 'Training regimen',
       y = 'Value',
       title = 'Going from baseline to LSD',
       subtitle = 'More risk, more reward') +
  theme_bw()

# Keep volume same, increase minutes of intensity
intense_week <- create_tw(training_days = 4,
                          times = c(25, 25, 25, 25),
                          percents = rep(50, 4))
plot_week(intense_week)

this_eval <- evaluate_week(intense_week)
progression[progression$name == 'Intense',
            c('cost',
              'benefit')] <- as.numeric(this_eval)
progression_long <- progression %>%
  gather(key, value, cost:benefit)
progression_long$name <- factor(progression_long$name,
                               levels = progression$name)
ggplot(data = progression_long %>% filter(!is.na(value)),
       aes(x = name,
           y = value,
           group = key,
           color = key)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  labs(x = 'Training regimen',
       y = 'Value',
       title = 'Going from LSD to Intense',
       subtitle = 'Significant reduction in risk of injury')+
  theme_bw() 

# Create a busy week - just like intense week, but condensed
busy_week <- create_tw(training_days = 3,
                       times = c(33, 33, 34),
                       percents = c(50, 50, 50))
plot_week(busy_week)
this_eval <- evaluate_week(busy_week)
progression[progression$name == 'Busy',
            c('cost',
              'benefit')] <- as.numeric(this_eval)
progression_long <- progression %>%
  gather(key, value, cost:benefit)
progression_long$name <- factor(progression_long$name,
                                levels = progression$name)
ggplot(data = progression_long %>% filter(!is.na(value)),
       aes(x = name,
           y = value,
           group = key,
           color = key)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  labs(x = 'Training regimen',
       y = 'Value',
       title = 'Going from Intense to Busy',
       subtitle = 'Same risk, greater reward') +
  theme_bw()

# Modify the busy week to change the amounts and intensity run
optimal_week <- create_tw(training_days = 3,
                          times = c(20, 20, 60),
                          percents = c(5, 35, 70))
evaluate_week(optimal_week) 
this_eval <- evaluate_week(optimal_week)
progression[progression$name == 'Optimal',
            c('cost',
              'benefit')] <- as.numeric(this_eval)
progression_long <- progression %>%
  gather(key, value, cost:benefit)
progression_long$name <- factor(progression_long$name,
                                levels = progression$name)
final_plot <- ggplot(data = progression_long %>% filter(!is.na(value)),
                     aes(x = name,
                         y = value,
                         group = key,
                         color = key)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  labs(x = 'Training regimen',
       y = 'Value',
       title = 'Going from Busy to Optimal',
       subtitle = 'The power of concentration') +
  theme_bw()
final_plot

# Show all plots
Rmisc::multiplot(plot_week(easy_week) +
                   labs(title = 'Baseline'),
                 plot_week(lsd_week) +
                   labs(title = 'LSD'),
                 plot_week(intense_week) +
                   labs(title = 'Intense'),
                 plot_week(busy_week) +
                   labs(title = 'Busy'),
                 plot_week(optimal_week) +
                   labs(title = 'Optimal'),
                 final_plot,
                 cols = 2)

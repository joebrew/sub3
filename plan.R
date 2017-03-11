library(tidyverse)

# Create a sequence of dates from now to the marathon
dates <- seq(as.Date('2017-03-13'),
             as.Date('2017-10-15'),
             by = 1)
day_number <- 1:length(dates)

# Divide into weeks
weeks <- (day_number %/% 7) + 1

# Create a dataframe
df <- data_frame(date = dates,
                 week = weeks)

# Get day of week
df$dow <- weekdays(df$date)

# Idea: 3 workouts per week, with 1, 2 and 3 reps (respectively)
# of an identical distance at race pace.

# Tuesday: 2
# Thursday: 1
# Saturday: 3

# Progression should be non-linear (since it's easy to go from 1 to 2 k
# harder to go from 8 to 9)
# Progression is number of kilometers per rep
progression <- seq(1, 500, length = nrow(df))^0.4
progression <- round(progression)

df$km_per_rep <- progression

# Plot the progression
plot(df$date, df$km_per_rep, type = 'l')

# Get number of reps
df$number_of_reps <-
  ifelse(df$dow == 'Tuesday', 2,
         ifelse(df$dow == 'Thursday', 1,
                ifelse(df$dow == 'Saturday', 3, 0)))

# Get number of kilometers at race pace
df$km_at_race_pace <- df$number_of_reps * df$km_per_rep

# Remove the last day
df <- df %>% filter(date < '2017-10-15')

# Group by week and get total number of km at race pace per week
by_week <- df %>%
  group_by(week) %>%
  summarise(km_at_race_pace = sum(km_at_race_pace)) %>%
  ungroup

# Plot
ggplot(data = by_week,
       aes(x = week,
           y = km_at_race_pace)) +
  geom_line() +
  geom_area(alpha = 0.3)

library(tidyverse)

# Create a sequence of dates from now to the marathon
dates <- seq(as.Date('2017-03-13'),
             as.Date('2017-10-15'),
             by = 1)
day_number <- 1:length(dates)

# Divide into weeks
weeks <- ((day_number - 1) %/% 7) + 1

# Get number of weeks
max(weeks)

# Create a dataframe
df <- data_frame(date = dates,
                 week = weeks)

# Get day of week
df$dow <- weekdays(df$date)

# Days until marathon
days_until_marathon <- as.Date('2017-10-14') - Sys.Date()
weeks_until_marathon <- as.numeric(days_until_marathon) / 7
weeks_until_marathon
# Idea: 3 workouts per week, with 2, 2 and 4 reps (respectively)
# of an identical distance at race pace.

# Tuesday: 2
# Thursday: 2
# Saturday: 4

# Progression should be non-linear (since it's easy to go from 1 to 2 k
# harder to go from 8 to 9)
# Progression is number of kilometers per rep
# progression <- seq(1, 40, length = nrow(df))^0.7
# progression <- round(progression)
progression <- c(rep(2, 14),
                 rep(3, 7),
                 rep(4, 7),
                 rep(3, 7),
                 rep(4, 7),
                 rep(5, 7),
                 rep(4, 7),
                 rep(5, 7),
                 rep(6, 7),
                 rep(5, 7),
                 rep(6, 7),
                 rep(7, 7),
                 rep(6, 7),
                 rep(7, 7),
                 rep(8, 7),
                 rep(7, 7),
                 rep(8, 7),
                 rep(9,7),
                 rep(8, 7),
                 rep(9, 7), 
                 rep(10, 7),
                 rep(9, 7),
                 rep(10, 7),
                 rep(11, 7),
                 rep(10, 7),
                 rep(11, 7),
                 rep(12, 7),
                 # Begin taper
                 rep(10, 21))
                 
df$km_per_rep <- progression

# Plot the progression
plot(df$date, df$km_per_rep, type = 'l')

abline(v = Sys.Date(), 
       col = 'red')

# Get number of reps
df$number_of_reps <-
  ifelse(df$dow == 'Tuesday', 2,
         ifelse(df$dow == 'Thursday', 2,
                ifelse(df$dow == 'Saturday', 4, 0)))

# Get number of kilometers at race pace
df$km_at_race_pace <- df$number_of_reps * df$km_per_rep

# Remove km per rep, if no reps that day
df$km_per_rep[df$number_of_reps == 0] <- 0

# Pick some time trial dates
time_trial_dates <-
  as.Date(c(#'2017-04-30', 
            #'2017-05-30', 
            '2017-06-18',
            '2017-07-30',
            '2017-08-27',
            '2017-09-24'))

# Spread data
df$workout <- 
  ifelse(df$dow == 'Sunday', 'rest',
         ifelse(df$km_per_rep == 0, 
         'easy + sprints',
         paste0(df$number_of_reps, 'x',
                df$km_per_rep, 'k')))

# Update time trial dates
df$workout <-
  ifelse(df$date %in% time_trial_dates,
         'time trial',
         df$workout)


wide <- df %>%
  mutate(date = format(date, '%b %d')) %>%
  group_by(week) %>%
  summarise(dates = paste0(first(date), '-', last(date)),
            Monday = workout[dow == 'Monday'],
            Tuesday = workout[dow == 'Tuesday'],
            Wednesday = workout[dow == 'Wednesday'],
            Thursday = workout[dow == 'Thursday'],
            Friday = workout[dow == 'Friday'],
            Saturday = workout[dow == 'Saturday'],
            Sunday = workout[dow == 'Sunday'])


# Group by week and get total number of km at race pace per week
by_week <- df %>%
  group_by(week) %>%
  mutate(date_start = first(date)) %>%
  filter(km_per_rep > 0) %>%
  summarise(date_start = first(date_start),
            km_at_race_pace = sum(km_at_race_pace),
            km_per_rep = mean(km_per_rep),
            tuesday = number_of_reps[dow == 'Tuesday'],
            thursday = number_of_reps[dow == 'Thursday'],
            saturday = number_of_reps[dow == 'Saturday']) %>%
  ungroup

# Plot
ggplot(data = by_week,
       aes(x = week,
           y = km_per_rep)) +
  geom_line() +
  geom_area(alpha = 0.3) +
  geom_vline(xintercept = df$week[df$date == Sys.Date()],
             alpha = 0.5)

ggplot(data = df %>%
         dplyr::filter(dow %in% c('Tuesday', 'Thursday', 'Saturday')),
       aes(x = date,
           y = km_at_race_pace)) +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.2) +
  geom_vline(xintercept = as.numeric(Sys.Date()),
             color = 'darkred',
             alpha = 0.5) +
  geom_hline(yintercept = c(seq(0, 40, 10), 42.195),
             color = 'red', alpha = 0.2)


write_csv(df, '~/Desktop/sub3.csv')

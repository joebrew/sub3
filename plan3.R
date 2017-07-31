# Libraries
library(tidyverse)

start_date <- as.Date('2017-07-31')
end_date <- as.Date('2017-10-14')
dates <- seq(start_date,
             end_date,
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


# 3 workouts
# 1: track workout at 5k pace or faster (purpose to improve vo2max)
# (about 5k total of repeats)
# 2: tempo / mp
# 3: long

# Define paces
# http://www.runnersworld.com/race-training/run-your-best-marathon-with-less-training
# Assuming 10k pace is 39:00 / 3:55 per k
# Long Run	10-K pace + 60 to 75 seconds/mile
# Long Tempo	10-K + 30 to 35 seconds
# Mid Tempo	10-K + 15 to 20 seconds
# Short Tempo	10-K pace
# 1600m Repeats	10-K - 35 to 40 seconds
# 1200m Repeats	10-K - 40 to 45 seconds
# 800m Repeats	10-K - 45 to 50 seconds
# 400m Repeats	10-K - 55 to 60 seconds
#################################
# 5k pace: 3:40
# 10k pace: 3:55
# marathon pace: 4:16
###############
# Long run: 10-k pace + 40-50 seconds per k: 4:35-4:45
# Long tempo: 10-k pace + 20 seconds per k: 4:15
# Mid tempo: 10-k pace + 10-12 seconds per k: 4:05-4:07
# Short tempo: 10-k pace: 3:55
# 1600m repeats: 10-k - 22-25 seconds per k: 3:30-3:33 = 5:36-5:41 per 1600
# 1200m repeats: 10-k - 25-28 seconds per k: 3:27-3:30 = 4:08-4:12 per 1200
# 800m repeats: 10-k - 28-31 seconds per k: 3:24-3:27 = 2:43-2:46 per 800
# 400m repeats: 10-k - 34-37 seconds per k: 3:17-3:21 = 79-80 per 400

# Define function for generating pace based on input

# Define the key workouts
kw <-
  data_frame(short = c('8x800 at 2:43-2:46 (3:24-3:27 pace)', # 11
                       '2x1200 at 4:08-4:12 (3:27-3:30 pace) + 4X800 at 2:43-2:46 (3:24-3:27 pace)', # 10
                       '2x6x400 at 79-90 (3:17-3:21 pace)', # 9
                       '5x1600 at 5:36-5:41 (3:30-3:33 pace)', # 8
                       '6x1200 at 4:08-4:12 (3:27-3:30 pace)', # 7
                       '8x1000 acceleration', # 6
                       '5x1600 at 5:36-5:41 (3:30-3:33 pace)', # 5
                       '10x400 at 79-90 (3:17-3:21 pace)', # 4,
                       '8x800 at 2:43-2:46 (3:24-3:27 pace)', # 3
                       '5x1200 at 4:08-4:12 (3:27-3:30 pace)', # 2
                       '6x400 at 79-90 (3:17-3:21 pace)'), # 1
             medium = c('8k at MT (4:05-4:07 pace)',
                        '10k at LT (4:16 pace)',
                        '5k at ST (3:55 pace)',
                        '8k at MT (4:05-4:07 pace)',
                        '15k at MP (4:16 pace)',
                        '8k at MT (4:05-4:07 pace)',
                        '18k at MP (4:16 pace)',
                        '15k at MP (4:16 pace)',
                        '8k at MT (4:05-4:07 pace)',
                        '5k at ST (3:55 pace)',
                        '5k at MP (4:16 pace)'),
             long = c('35k',
                      '25k',
                      '35k',
                      '25k',
                      '32k',
                      '25k',
                      '32k',
                      '25k',
                      '30k',
                      '21k',
                      '21k')) %>%
  mutate(week = 1:11)

df$workout <- NA
kw$short_picked <- kw$medium_picked <- kw$long_picked <- FALSE
for (i in 1:nrow(df)){
  this_row <- df[i,]
  if(this_row$dow == 'Tuesday'){
    df$workout[i] <- kw$short[kw$week == this_row$week]
  }
  if(this_row$dow == 'Thursday'){
    df$workout[i] <- kw$medium[kw$week == this_row$week]
  }
  if(this_row$dow == 'Saturday'){
    df$workout[i] <- kw$long[kw$week == this_row$week]
  }
  if(this_row$dow %in% c('Monday', 'Wednesday', 'Friday')){
    df$workout[i] <- 'Strength'
  }
  if(this_row$dow == 'Sunday'){
    df$workout[i] <- 'Rest'
  }
}

# Make wide
df <- df %>%
  group_by(week) %>%
  mutate(starts_on = dplyr::first(date)) %>%
  mutate(ends_on = dplyr::last(date)) %>%
  ungroup %>%
  mutate(date = paste0(starts_on, ' to ', ends_on)) %>%
  dplyr::select(-starts_on, -ends_on) %>%
  mutate(dow = factor(dow, levels = c('Monday',
                                      'Tuesday',
                                      'Wednesday',
                                      'Thursday',
                                      'Friday',
                                      'Saturday',
                                      'Sunday'))) %>%
  tidyr::spread(key = dow,
                value = workout)

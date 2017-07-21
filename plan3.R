# Libraries
library(tidyverse)


start_date <- as.Date('2017-06-26')
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

# Define the key workouts

# library(cycleRtools)
library(xml2)
library(XML)
library(tidyverse)

# Define a directory with the txc files
tcx_dir <- '/home/joebrew/Dropbox/Aplicaciones/tapiriik/'
files <- dir(tcx_dir)
# files <- files[length(files)]
original_files <- files
files <- file.path(tcx_dir, files)
file <- files

# Define function for reading tcx files
read_tcx <- function(file){
  message(paste0('Reading file ', 
                 file))
  
  x <- xmlToList(file)
  y <- plyr::ldply(x, data.frame)
  # Keep only the first row
  y <- y[1,]
  # Keep lap information elsewhere
  lap_info <- y[,grepl('Activity.Lap', names(y)) & !grepl('Trackpoint', names(y))]
  # Keep only those columns which contain tracking
  y <- y[,grepl('Activity.Lap.Track.Trackpoint', names(y))]

  # Define helper for cleaning_up
  clean_up <- function(df){
    df <- t(df)
    df <- data.frame(df)
    df$indicator <- row.names(df)
    df$value <- df$X1
    row.names(df) <- NULL
    df$X1 <- NULL
    # Remove the useless parts of indicator
    df$indicator <- gsub('Activity.Lap.Track.Trackpoint.',
                        '',
                        df$indicator)
    
    # Extract the number from indicator
    numbers <- regmatches(df$indicator, 
                          gregexpr("[[:digit:]]+", df$indicator))
    numbers <- lapply(numbers, 
                      function(x){
                        out <- unlist(x)
                        if(length(out) == 0){
                          out <- NA
                        }
                        return(out)
                      })
    numbers <- unlist(numbers)
    df$number <- numbers
    df$number <- as.numeric(as.character(df$number))
    df$number[is.na(df$number)] <- 0
    # Increase number by 1
    df$number <- df$number + 1
    
    # Remove the number from indicator
    df$indicator <- gsub('[[:digit:]]+', 
                        '',
                        df$indicator)
    df$indicator <- as.character(df$indicator)
    df$value <- as.character(df$value)
    # remove trailing periods
    df$indicator <-
      gsub("\\.(?=\\.*$)", "", df$indicator, perl=TRUE)
    return(df)
  }
  
  # Clean up lap info
  lap_info <- clean_up(lap_info)
  # Clean up names
  lap_info <-
    lap_info %>%
    mutate(indicator = gsub('Activity.Lap.',
                            '',
                            indicator))
  # Keep only relevant indicators
  lap_info <-
    lap_info %>%
    filter(indicator == 'DistanceMeters')
  # Get cumulative distance
  # lap_info$cumulative_seconds <- cumsum(lap_info$value)
  lap_info$distance <- cumsum(lap_info$value)
  
  # Clean up trackpoint info
  z <- clean_up(y)
  
  # Group by number and get values
  out <- z %>%
    dplyr::group_by(number) %>%
    summarise(altitude = dplyr::first(value[indicator == 'AltitudeMeters']),
              distance = dplyr::first(value[indicator == 'DistanceMeters']),
              cadence = dplyr::first(value[indicator == 'Extensions.TPX.RunCadence']),
              speed = dplyr::first(value[indicator == 'Extensions.TPX.Speed']),
              latitude = dplyr::first(value[indicator == 'Position.LatitudeDegrees']),
              longitude = dplyr::first(value[indicator == 'Position.LongitudeDegrees']),
              time = dplyr::first(value[indicator == 'Time'])) %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           cadence = as.numeric(cadence),
           speed = as.numeric(speed),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))
  # Bring in lap info
  out$lap <- 0
  for (i in 1:nrow(lap_info)){
    if(i == 1){
      lower <- -1
    } else {
      lower <- lap_info$distance[i-1]
    }
    upper <- lap_info$distance[i]
    
    out$lap[out$distance <= upper &
              out$distance > lower] <-
      lap_info$number[i]
  }
  
  # Clarify that number is seconds
  out <-
    out %>%
    rename(seconds = number)
  return(out)
}

results_list <- list()
for (i in 1:length(files)){
  message(i)
  activity_name <- gsub('.tcx', '',
                        original_files[i])
  file_name <- paste0(activity_name,
                      '.RData')
  date <- as.Date(substr(activity_name, 1, 10))
  if(file_name %in% dir('rdatas')){
    load(paste0('rdatas/', file_name))
  } else {
    this_run <- read_tcx(files[i])
    save(this_run,
         file = paste0('rdatas/', file_name))
  }
  this_run$date <- date
  this_run$activity_name <- activity_name
  results_list[[i]] <- this_run
}
results <- bind_rows(results_list)



# # ###############################################
# # activity_dir <- '/media/joebrew/GARMIN/GARMIN/ACTIVITY/'
# # plugged_in <- dir.exists(activity_dir)
# # 
# # # If connected to device, copy the files to local hard drive
# # if(plugged_in){
# #   files <- dir(activity_dir)
# #   files <- file.path(activity_dir, files)
# #   
# #   # Copy to /data
# #   if(plugged_in){
# #     for(i in 1:length(files)){
# #       this_file <- files[i]
# #       file.copy(from=  this_file,
# #                 to = paste0('data', gsub(activity_dir, '', this_file)))
# #       message(this_file)
# #     }
# #   }
# # }
# # 
# # # Define local files
# # local_files <- dir('data')
# # 
# # # Read in local files
# # results <- list()
# # for (i in 1:length(local_files)){
# #   try({
# #     x <- cycleRtools::read_fit(paste0('data/', local_files[i]), 
# #                                format = TRUE)
# #     x$activity_number <- i
# #     x$file <- local_files[i]
# #     results[[i]] <- x
# #   })
# # }
# # 
# # # Bind all together
# # results <- bind_rows(results)
# 

# Define function for converting meters per second
# to minutes per kilometer
mps_to_pace <- function(x){
  1000 / x / 60
}
results$pace <- mps_to_pace(results$speed)
results$pace[is.infinite(results$pace)] <- NA
results$pace[results$pace > 100] <- NA

# Get week
previous_monday <- function(x) 7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4, origin = "1970-01-01")
results$week_start <- previous_monday(results$date)

# Get a distance_diff
results <-
  results %>%
  group_by(activity_name) %>%
  mutate(distance_diff = distance - lag(distance, 1, default = 0)) %>%
  ungroup

# Get marathon pace, etc.
results$pace_group <-
  ifelse(results$pace > 6.5, 'Very slow',
         ifelse(results$pace > 5, 'Slow',
                ifelse(results$pace > 4, 'Marathon pace',
                       'Fast')))
results$pace_group <- factor(results$pace_group,
                             levels = rev(sort(unique(results$pace_group))))

x <- results %>%
  filter(!is.na(pace_group)) %>%
  group_by(week_start, pace_group) %>%
  summarise(hours = n() / 60 / 60,
            km = sum(distance_diff) / 1000) %>%
  ungroup %>%
  group_by(week_start) %>%
  mutate(percent_time = hours / sum(hours) * 100,
         percent_distance = km / sum(km) * 100) %>%
  ungroup

library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(4)
cols <- rev(cols)
ggplot(data = x %>% filter(!is.na(pace_group)),
       aes(x = week_start,
           y = hours,
           fill = pace_group)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By time spent running')
ggplot(data = x %>% filter(!is.na(pace_group)),
       aes(x = week_start,
           y = km,
           fill = pace_group)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By distance')

ggplot(data = x %>% filter(!is.na(pace_group)),
       aes(x = week_start,
           y = percent_distance,
           fill = pace_group)) +
  geom_area(stat = 'identity') +
  labs(x = 'Week',
       y = 'Percent',
       title = 'Pace distribution',
       subtitle = 'Amount of time running at different paces') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By time spent running')

# Create pace curve
pc <- 
  results %>%
  arrange(activity_name, desc(speed)) %>%
  mutate(dummy = 1) %>%
  group_by(activity_name) %>%
  mutate(y = cumsum(dummy)) %>%
  mutate(p = y / max(y) * 100)

activity_names <- sort(unique(pc$activity_name))
ggplot(data = pc %>%
         filter(activity_name %in% activity_names[10:25]),
       aes(x = p,
           y = speed)) +
  geom_line() +
  facet_wrap(~activity_name)

# Histogram of pace
ggplot(data = results %>%
         filter(pace <= 8),
       aes(x = pace)) +
  geom_density(fill = 'darkblue',
               alpha = 0.5) +
  labs(x = 'Minutes per kilometer',
       y = 'Density',
       title = 'Overall distribution of pace',
       subtitle = 'Peak at marathon pace, plateau at easy run pace')
ggplot(data = results %>%
         filter(pace <= 8),
       aes(x = pace)) +
  geom_histogram(fill = 'darkblue',
               alpha = 0.5) 



# # Define function converting kph to pace per k
# kph_to_pace <- function(x){
#   60 / x
# }
# results$pace <- kph_to_pace(results$speed.kmh)
# results$pace[is.infinite(results$pace)] <- NA
# 
# 
# Define function for being in marathon pace zone
results <-
  results %>%
  mutate(marathon_pace = pace <= 4.6) #4:36
results$marathon_pace[is.na(results$marathon_pace) |
                        is.infinite(results$marathon_pace)] <-
  FALSE
# # Get a date
# results <- results %>%
#   mutate(date = as.Date(timestamp))
# 
# Get a diff on the difference
results <- results %>%
  group_by(activity_name) %>%
  mutate(diff_distance = distance - dplyr::lag(distance, default = 0))
# 
# Get how much was at marathon pace for each day
by_day <- results %>%
  group_by(date, marathon_pace) %>%
  summarise(distance = sum(diff_distance)) %>%
  ungroup
# 
library(ggplot2)
ggplot(data = by_day %>%
         mutate(marathon_pace = ifelse(marathon_pace,
                                       'Marathon pace',
                                       'Other')),
       aes(x = date,
           y = distance / 1000,
           group = marathon_pace,
           fill = marathon_pace)) +
  geom_bar(stat = 'identity', position = 'dodge',
           alpha = 0.6) +
  labs(x = 'Date',
       y = 'Kilometers') +
  scale_fill_manual(name = 'Pace',
                    values = c('blue', 'darkred'))

ggplot(data = results %>%
         filter(pace <= 10),
       aes(x = pace,
           group = activity_name)) +
  geom_density(alpha = 0.3,
               fill = 'grey') +
  labs(x = 'Minutes per kilometer',
       ylab = 'Density',
       title = 'Run-specific pace distribution',
       subtitle = 'Each shape is one run')

# 

# Get by week
by_week <- 
  results %>%
  mutate(week = format(date, '%U')) %>%
  group_by(week, marathon_pace) %>%
  summarise(distance = sum(diff_distance),
            runs = length(unique(activity_name))) %>%
  ungroup %>%
  mutate(week = as.numeric(week) - 11) %>%
  filter(week != 0)
ggplot(data = by_week,
       aes(x = week,
           y = distance,
           group = marathon_pace,
           fill = marathon_pace)) +
  geom_bar(stat = 'identity', position = 'stack')

library(cism)
library(sp)
library(ggthemes)
library(ggmap)
plot(man2)
# 
marathon_pace <- 14.0625
# 

# Create a pace curve
pc <- 

# For now, keep only manhica
# removing calanga, bilene, komati, etc.
results <-
  results %>%
  filter(round(longitude) == 33,
         (date < '2017-04-12' | date > '2017-04-13'),
         date != '2017-03-27')

# x <- get_map(location = results %>%
#                ungroup %>%
#                summarise(longitude = mean(longitude, na.rm = TRUE),
#                          latitude = mean(latitude, na.rm = TRUE)) %>%
#                unlist(),
#             zoom = 12,
#             maptype = 'toner',
#             source = 'stamen',
#             color = 'bw')
# y <- get_map(location = results %>%
#                 ungroup %>%
#                 summarise(longitude = mean(longitude, na.rm = TRUE),
#                           latitude = mean(latitude, na.rm = TRUE)) %>%
#                 unlist(),
#               zoom = 14,
#               maptype = 'toner',
#               source = 'stamen',
#               color = 'bw')
# 
# ggmap(x,
#       darken = 0.85) +
#   geom_path(data = results,
#             aes(x = longitude,
#                 y = latitude,
#                 group = activity_name),
#             alpha = 0.1,
#             color = 'darkorange',
#             size = 0.4) +
#   coord_map() +
#   theme_map()

# library(leaflet)
# leaflet() %>%
#   # addProviderTiles('Esri.WorldImagery') %>%
#   # addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
#   addProviderTiles('Stamen.Watercolor') %>%
#   addPolylines(data = results, lng = ~longitude,
#                lat = ~latitude, group = ~activity_name)
# 
# # Calanga
# leaflet() %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   # addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
#   # addProviderTiles('Stamen.Watercolor') %>%
#   addPolylines(data = results %>%
#                  filter(date == '2017-03-27') %>%
#                  filter(activity_name == min(activity_name)),
#                lng = ~longitude,
#                latitude = ~latitude, group = ~activity_name)
# ggplot() +
#   geom_path(data = results,
#             aes(x = longitude,
#                 y = latitude,
#                 group = activity_name),
#             alpha = 0.3,
#             color = 'darkred',
#             size = 0.3) +
#   coord_map() +
#   theme_map()
print(head(results))
ggplot(data = results,
       aes(x = longitude,
           y = latitude,
           group = activity_name)) +
  geom_path(#aes(color = pace),
            alpha = 0.7,
            color = 'black',
             size = 0.01) +
  coord_map() +
  theme_map() +
  # scale_color_continuous_tableau('Red',
  #                                name = 'Elevation') +
  theme(legend.key.size = unit(2, 'mm')) 


ggplot(data = results,
       aes(x = longitude,
           y = latitude)) +
  geom_point(
    alpha = 0.1,
    color = 'darkred',
    size = 0.02) +
  coord_map() +
  theme_map() 

# 
# ggplot(data = results %>%
#          group_by(activity_name) %>%
#          mutate(totalk = sum(distance.km)) %>%
#          mutate(date = first(date)) %>%
#          ungroup %>%
#          group_by(date) %>%
#          mutate(activity = 1 + activity_name - min(activity_name)) %>%
#          ungroup %>%
#          mutate(activity = letters[activity]) %>%
#          filter(totalk > 0) %>%
#          mutate(date_activity = paste0(date, ': ', activity)),
#        aes(x = timer.min,
#            y = speed.kmh)) +
#   geom_area(alpha = 0.6,
#             fill = 'darkorange') +
#   geom_hline(yintercept = marathon_pace,
#              lty = 2) +
#   facet_wrap(~date_activity) +
#   theme_bw()
# 
# x <- intervals %>%
#   group_by(marathon_pace) %>%
#   summarise(time = sum(delta.t) / 60,
#             dist = sum(diff_distance))
# 
# ggplot( data = intervals,
#         aes(x = timer.min,
#             y = marathon_pace)) +
#   geom_jitter(alpha = 0.3)



# library(cycleRtools)
library(xml2)
library(XML)
library(tidyverse)
library(ggmap)
library(RColorBrewer)
library(ggplot2)
library(cism)
library(sp)
library(ggthemes)
library(ggmap)

# Define a directory with the txc files
tcx_dir <- '/home/joebrew/Dropbox/Aplicaciones/tapiriik/'
files <- dir(tcx_dir)
# files <- files[length(files)]
original_files <- files
files <- file.path(tcx_dir, files)
file <- files

# Define function for reading tcx files
read_tcx <- function(file, reverse_geocode = TRUE){
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
  
  # Reverse geocode
  has_gps <- length(out$longitude[!is.na(out$longitude)]) > 0
  if(reverse_geocode){
    if(has_gps){
      gc <- revgeocode(location = c(out$longitude[!is.na(out$longitude)][1],
                                    out$latitude[!is.na(out$latitude)][1]),
                       output = 'more', 
                       messaging = TRUE)
      # Join to results
      out <- cbind(out, gc)
    }
  }
  
  return(out)
}

results_list <- list()
file_names_list <- c()
file_names_list_counter <- 0
files <- files[grepl('Running', files)]
original_files <- original_files[grepl('Running', original_files)]
for (i in 1:length(files)){
  message(i)
  activity_name <- gsub('.tcx', '',
                        original_files[i])
  file_name <- paste0(activity_name,
                      '.RData')
  date <- as.Date(substr(activity_name, 1, 10))
  file_names_list_counter <- file_names_list_counter + 1
  file_names_list[file_names_list_counter] <- file_name
  if(file_name %in% dir('rdatas')){
    load(paste0('rdatas/', file_name))
  } else {
    this_run <- read_tcx(files[i], reverse_geocode = TRUE)
    save(this_run,
         file = paste0('rdatas/', file_name))
  }
  this_run$date <- date
  this_run$activity_name <- activity_name
  results_list[[i]] <- this_run
}
# remove those files not in the list
rdatas <- dir('rdatas')
rdatas <- rdatas[grepl('.RData', rdatas)]
for (i in 1:length(rdatas)){
  this_file <- rdatas[i]
  if(!this_file %in% file_names_list){
    file.remove(paste0('rdatas/', this_file))
  }
}
results <- bind_rows(results_list)

# Keep only those dates on / after march 20th 
# (when i started measuring every second)
results <- results %>% filter(date >= '2017-03-20')

# Keep only running
results <- results %>%
  filter(grepl('Running', activity_name))



# Now defunct garmin reading direct
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

# Calculate speed when not already calculated
results <-
  results %>%
  arrange(seconds) %>%
  group_by(activity_name) %>%
  mutate(distance_diff = distance - dplyr::lag(distance, n = 1, default = 0),
         seconds_diff = seconds - dplyr::lag(seconds, n = 1, default = 0)) %>%
  ungroup %>%
  # Overwrite speed
  mutate(speed = distance_diff / seconds_diff) %>%
  arrange(activity_name)
  

# Flag unrealistic (can lead to disjoined further results)
results <-
  results %>%
  mutate(speed = ifelse(speed > 10, NA, speed)) 

# Define function for converting meters per second
# to minutes per kilometer
mps_to_pace <- function(x){
  1000 / x / 60
}
results$pace <- mps_to_pace(results$speed)
results$pace[is.infinite(results$pace)] <- NA
results$pace[results$pace > 100] <- NA
results <- results %>%
  mutate(pace = ifelse(is.na(speed), NA, pace))




# Get week
previous_monday <- function(x) 7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4, origin = "1970-01-01")
results$week_start <- previous_monday(results$date)

# Get marathon pace, etc.
results$pace_group <-
  ifelse(results$pace > 6.5, 'Very slow',
         ifelse(results$pace > 4.5, 'Slow',
                ifelse(results$pace > 4, 'Marathon pace',
                       'Fast')))
results$pace_group <- factor(results$pace_group,
                             levels = rev(sort(unique(results$pace_group))))

# Define function for converting minutes:seconds to decimals
convert_time <- function(x = '2:15'){
  y <- unlist(strsplit(x, ':'))
  y[2] <- paste0(as.numeric(y[2]) / 60)
  y <- as.numeric(y)
  sum(y)
}

# Define function for classifying pace
classify_pace <- function(pace){
  ifelse(pace > convert_time("5:09"), "5:09+ - recovery",
         ifelse(pace > convert_time("4:26"), "4:26-5:09 - endurance",
                ifelse(pace > convert_time("3:59"), "3:59-4:26 - tempo",
                       ifelse(pace > convert_time("3:43"), "3:43-3:59 - threshold",
                              ifelse(pace > convert_time("3:30"), "3:30-3:43 - vo2max", ifelse(pace <= convert_time("3:30"), '<3:30 - anaerobic', NA))))))}
results$zone <- classify_pace(results$pace)


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

marathon_pace <- 14.0625
# 




# Define function for mapping
cool_map <- function(data,
                     color = 'yellow',
                     title = NULL){
  g <- ggplot(data = data %>%
                filter(#pace < 10,
                  !is.na(locality)),
              aes(x = longitude,
                  y = latitude,
                  group = activity_name)) +
    theme(panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(),
          panel.background=element_blank()) + 
    theme(plot.background=element_rect(fill="black"),
          panel.background=element_rect(fill='black'), 
          legend.background= element_rect(fill="black", colour=NA),
          legend.key = element_rect(colour = NA, col = "black",
                                    size = .5, fill = 'black')) +
    theme(axis.line = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          # axis.title = element_blank(), 
          panel.border = element_blank(), 
          panel.spacing = unit(0, 
                               "lines"), 
          legend.justification = c(0, 0), 
          legend.position = c(0, 
                              0)) +
    geom_path(#aes(size = pace),
      # linejoin = "mitre", lineend = "round"
      alpha = 0.4,
      color = color, 
      size = 0.1) #+
  # coord_map()
  if(!is.null(title)){
    g <- g +
      labs(title = title) +
      theme(plot.title = element_text(color = color, size = 8))
  }
  return(g)
}


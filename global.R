library(tidyverse)

# Function for converting : to dec
make_minutes <- function(x){
  x <- as.character(x)
  # Split at the :
  z <- strsplit(x, split = ':')
  z <- unlist(lapply(z, function(a){

    seconds <- as.numeric(a[length(a)])
    minutes <- as.numeric(a[length(a) - 1])
    if(length(a) == 3){
      hours <- as.numeric(a[1])
    } else {
      hours <- 0
    }
    # Combine
      (hours * 60) +
      minutes +
      (seconds / 60)
  }))
  return(z)
}

# Function for converting decimals to times
make_time <- function(x){
  
  # Helper for adding zero
  add_zero <- function(x){
    x <- as.character(x)
    ifelse(nchar(x) == 1,
           paste0('0', x),
           x)
  }
  
  # Split at .
  x <- as.character(x)
  x <- strsplit(x, split = '.', fixed = TRUE)
  # Get hours
  hours <- add_zero(unlist(lapply(x, function(z){
    as.numeric(z[1]) %/% 60
  })))
  # Get minutes
  minutes <- add_zero(unlist(lapply(x, function(z){
    as.numeric(z[1]) %% 60
  })))
  # Get seconds
  seconds <- add_zero(unlist(lapply(x, function(z){
    a <- suppressWarnings(as.numeric(paste0('0.', z[2])) * 60)
    a <- ifelse(is.na(a), 0, a)
    a <- round(a, digits = 0)
    a
  })))
  # Get decimals
  decimals <- add_zero(unlist(lapply(x, function(z){
    a <- suppressWarnings(as.numeric(paste0('0.', z[2])) * 60)
    a <- ifelse(is.na(a), 0, a)
    a <- round(a, digits = 2)
    a <- strsplit(as.character(a), split = '.', fixed = TRUE)
    a <- unlist(lapply(a, function(x){
      x[2]
    }))
    a <- ifelse(is.na(a), '00', a)
    a
  })))
  # Combine
  out <- paste0(hours,
                ':',
                minutes,
                ':',
                seconds,
                '.',
                decimals)
  
  # # Standardize decimals
  # out <-
  #   ifelse(nchar(out) == 8,
  #          paste0(out, '.00'),
  #          ifelse(nchar(out)))
  
  return(out)
}

# Make data
make_data <- function(km_pace = 4.25,
                      km = TRUE,
                      mi = TRUE,
                      simple = FALSE){
  
  conversion_factor <- 1.609344
  
  # Make a kilometer dataframe
  df <- data_frame(km = sort(unique(c(seq(0, 42.2, 0.2),
                                      seq(0, 42, 1),
                                      seq(0, 42, 0.25)))))
  df$time <- df$km * km_pace
  df$mile <- df$km / conversion_factor
  
  # Make a mile df
  mdf <- df %>%
    dplyr::select(-mile) %>%
    mutate(time = time * conversion_factor) %>%
    rename(mile = km) %>%
    mutate(km = mile * conversion_factor)
  
  # Arrange columns
  mdf <- mdf[,names(df)]
  
  # Combine
  if(km & mi){
    final <- bind_rows(df, 
                       mdf) 
  } else if(km){
    final <- df
  } else if(mi){
    final <- mdf
  } else {
    stop('At least one of km and mi must be true')
  }
  
  final <- final %>%
    arrange(km) %>%
    mutate(meters = km * 1000) %>%
    dplyr::select(meters, km, mile, time)
  
  
  # Round distances
  for (j in 1:3){
    final[,j] <- round(final[,j], digits = 2)
  }
  
  
  # Remove dups
  final <- 
    final %>%
    filter(!duplicated(km))
  
  # Keep only those distances less than marathon
  final <-
    final %>%
    filter(km <= 42.2)
  
  # Get minutes
  final$minutes <- final$time
  final$time <- make_time(final$time)
  
  # Get feet
  final$feet <- final$mile * 5280
  
  # Order
  final <- final %>%
    dplyr::select(km,
                  meters,
                  mile, 
                  feet,
                  time,
                  minutes)
  
  if(mi & km & simple){
    # Simplify
    final <- final %>%
      filter(round(km) == km | 
               km == 42.2 | 
               round(mile) == mile | 
               mile == 26.22)
  }
  
  # Remove columns if needed
  if(!mi){
    final$mile <- NULL
    final$feet <- NULL
    if(simple){
      final <- final %>%
        filter(round(km) == km | 
                 km == 42.2)
    }
  }
  if(!km){
    final$km <- NULL
    final$meters <- NULL
    if(simple){
      final <- 
        final %>%
        filter(round(mile) == mile |
                 mi == 26.22)
    }
  }
  
  return(final)
}



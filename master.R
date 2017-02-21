library(tidyverse)

conversion_factor <- 1.609344
km_pace <- 4.25

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

# 
final <- bind_rows(df, 
                   mdf) %>%
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

# Get minutes
final$minutes <- final$time
final$time <- make_time(final$time)




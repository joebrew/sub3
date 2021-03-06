# Get and clean up data
source('garmin_prepare.R')

# HEAT MAP
# Create heat map
# heat_data <- 
#   results %>%
#   mutate(latitude = round(latitude, digits = 4),
#          longitude = round(longitude, digits = 4)) %>%
#   group_by(longitude, 
#            latitude) %>%
#   tally %>%
#   mutate(val = log(n) + 1)

# library(leaflet)
# leaflet() %>%
#   addTiles() %>%
#   addMarkers(data = heat_data,
#                lng = ~longitude,
#                lat = ~latitude)

# Analaysis by simple pace groups ###################################

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

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(x$pace_group)))
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
       y = 'Kilometers',
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
       y = 'Percent of distance run',
       title = 'Pace distribution',
       subtitle = 'By time spent running')

# Analysis by MacMillan pace zone #######################################

x <- results %>%
  filter(!is.na(zone)) %>%
  group_by(week_start, zone) %>%
  summarise(hours = n() / 60 / 60,
            km = sum(distance_diff) / 1000) %>%
  ungroup %>%
  group_by(week_start) %>%
  mutate(percent_time = hours / sum(hours) * 100,
         percent_distance = km / sum(km) * 100) %>%
  ungroup
x$zone <- factor(x$zone,
                 levels = rev(sort(unique(x$zone))))

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(x$zone)))
# cols <- rev(cols)
ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = hours,
           fill = zone)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By time spent running')
ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = km,
           fill = zone)) +
  geom_bar(stat = 'identity',
           position = 'dodge') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By distance')

ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = percent_distance,
           fill = zone)) +
  geom_area(stat = 'identity') +
  labs(x = 'Week',
       y = 'Percent',
       title = 'Pace distribution',
       subtitle = 'Amount of time running at different paces') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Percent of time running',
       title = 'Pace distribution',
       subtitle = 'By distance')

ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = percent_distance,
           fill = zone)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Week',
       y = 'Percent',
       title = 'Pace distribution',
       subtitle = 'Amount of time running at different paces') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Percent',
       title = 'Pace distribution',
       subtitle = 'By distance') +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = hours,
           fill = zone)) +
  geom_bar(stat = 'identity',
           alpha = 0.7) +
  labs(x = 'Week',
       y = 'Percent',
       title = 'Pace distribution',
       subtitle = 'Amount of time running at different paces') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Hours',
       title = 'Pace distribution',
       subtitle = 'By time spent running') +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = x %>% filter(!is.na(zone)),
       aes(x = week_start,
           y = km,
           fill = zone)) +
  geom_bar(stat = 'identity',
           position = 'stack') +
  scale_fill_manual(name = 'Pace',
                    values = cols) +
  labs(x = 'Week',
       y = 'Kilometers',
       title = 'Pace distribution',
       subtitle = 'By distance') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.direction = "horizontal", legend.position = "bottom")

# Pace curves ########################################
pc <- 
  results %>%
  arrange(activity_name, desc(speed)) %>%
  mutate(dummy = 1) %>%
  group_by(activity_name) %>%
  mutate(y = cumsum(dummy)) %>%
  mutate(p = y / max(y) * 100)

activity_names <- sort(unique(pc$activity_name))
ggplot(data = pc,
       aes(x = p,
           y = speed,
           group = activity_name)) +
  geom_line(alpha = 0.3) 

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

# How much fast running per day #################################
# Get how much was at marathon pace for each day
by_day <- results %>%
  group_by(date, marathon_pace) %>%
  summarise(distance = sum(distance_diff)) %>%
  ungroup

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

# Get by week
by_week <- 
  results %>%
  mutate(week = format(date, '%U')) %>%
  group_by(week, marathon_pace) %>%
  summarise(distance = sum(distance_diff),
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

######### MAPS


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


# For now, keep only manhica
# removing calanga, bilene, komati, etc.
map_data <- results
# results %>%
# filter(round(longitude) == 33,
#        # (date < '2017-04-12' | date > '2017-04-13'),
#        date != '2017-03-27',
#        date != '2017-05-03',
#        date != '2017-04-13')
# map_data <- results
top_n <- map_data %>%
  filter(!is.na(locality)) %>%
  group_by(locality) %>%
  tally %>%
  ungroup %>%
  arrange(desc(n))
n <- length(unique(top_n$locality))
top_n <- top_n$locality[1:n]
map_list <- list()
colors <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(n)
colors <- sample(colors, length(colors))
for (i in 1:length(top_n)){
  map_list[[i]] <- cool_map(data = map_data %>% filter(locality == top_n[i]),
                            color = colors[i],
                            title = top_n[i]) +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          plot.margin=unit(c(0,0,0,0), "lines")) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
}
map_list[[1]] 

multiplot_joe <- function (..., plotlist = NULL, cols = 1, layout = NULL) 
{
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) 
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                     ncol = cols, nrow = ceiling(numPlots/cols))
  if (numPlots == 1) {
    print(plots[[1]])
  }
  else {
    grid.newpage()
    grid.rect(gp=gpar(fill="black",
                      col = 'black',
                      alpha = 0,
                      lwd = 0))
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot_joe(plotlist = map_list, cols = 3)
ggsave('~/Desktop/out.png')
ggplot(data = map_data %>%
         filter(#pace < 10,
           !is.na(locality),
           locality %in% top_n),
       # c('Gainesville', 'Manhiça',
       #                          'Barcelona', 'Santa Coloma de Queralt')),
       aes(x = longitude,
           y = latitude,
           group = activity_name)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "lines")) +
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
        axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.spacing = unit(0, 
                             "lines"), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 
                            0)) +
  geom_path(#aes(size = pace),
    # linejoin = "mitre", lineend = "round"
    alpha = 0.2,
    aes(color = locality),
    # color = 'yellow', 
    size = 0.1) +
  # coord_map() +
  facet_wrap(~locality,
             scales = 'free') +
  theme(legend.position="none") +
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color = 'darkgrey', size = 5),
        plot.title = element_text(size = 7))

ggsave('~/Desktop/plot.pdf')
  # ggsave('~/Desktop/plot.png')


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



cool_map(data = map_data %>%
           filter(locality == 'Gainesville'))
ggsave(filename = '~/Desktop/mapa.png')

world <- borders("world") 
ggplot() + world +
  geom_path(data = map_data,
            aes(x = longitude,
                y = latitude,
                group = activity_name),
            alpha = 0.9,
            color = 'red', 
            size = 1)

melon_run <- results %>%
  # filter(date == '2017-07-04') %>%
  filter(activity_name == '2017-07-04_Gainesville Running_Running') %>%
  filter(seconds > 5,
         seconds <= 1055)

ggplot(data = melon_run,
       aes(x = seconds,
           y = pace)) +
  geom_jitter(size = 0.6) +
  geom_smooth()


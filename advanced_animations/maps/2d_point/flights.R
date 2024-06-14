library(tidyverse)
library(dplyr)
library(gganimate)
library(ggplot2)
library(sf)

read_hourly_data <- function(hour) {
  hour_log <- read_csv(sprintf("flight_hours/states_2022-06-27-%02d.csv/states_2022-06-27-%02d.csv", hour, hour),
                       show_col_types = FALSE) 
  hour_log$time <- as_datetime(hour_log$time)
  hour_log$time <- floor_date(hour_log$time, "hour")
  hour_log <-  hour_log |> 
    filter(!onground) |> 
    filter(!is.na(lat)) |> 
    filter(!is.na(lon)) |> 
    filter(lon < -65 & lon > -135) |> 
    filter(lat > 23 & lat < 50) |> 
    st_as_sf(coords = c('lon','lat')) |> 
    st_set_crs(4326) |> 
    group_by(icao24) |> #time
    arrange(desc(icao24)) |> 
    slice_head(n=1)
}

hourly_flights <- read_hourly_data(0)
system.time(for(x in 1:23){
  hourly_flights <- rbind(hourly_flights, read_hourly_data(x))
  print(paste("Read and appended:",x))
})

hourly_flights_AAL <- hourly_flights |> 
  filter(startsWith(callsign, "AAL"))

usa <- st_as_sf(maps::map("state", fill=TRUE))

animation <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=hourly_flights_AAL, size=1, alpha=0.6, aes(group = icao24))+
  theme_void()+
  labs(title="American Airlines Flights Over A 24-hour Period", 
       subtitle="{case_when(
       as.integer((as.numeric(format(frame_time, \"%H\")))%%24) == 0 ~ \"12:00 AM\",
       as.integer((as.numeric(format(frame_time, \"%H\")))%%24) < 12 ~ paste0(as.integer((as.numeric(format(frame_time, \"%H\")))%%24),\":00 AM\"),
       as.integer((as.numeric(format(frame_time, \"%H\")))%%24) == 12 ~ \"12:00 PM\",
       TRUE ~ paste0(as.integer((as.numeric(format(frame_time, \"%H\")))%%24)%%12,\":00 PM\")
       )} EST",
       caption="Nikhil Chinchalkar for Princeton University | Open Sky Network | 2024",
       color="Significance",
       size="Magnitude")+
  xlab('')+
  ylab('')+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5, face = "bold"))+
  transition_time(time)+
  enter_fade(alpha = 0)+
  exit_fade(alpha = 0)

animate(animation, fps=10, duration=24, end_pause = 50, height = 7,
        width = 9, units = "in", res = 200)

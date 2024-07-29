# animated-2d-point-maps-alt.R

<figure><img src="../../.gitbook/assets/final (23).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(tidyverse)
library(dplyr)
library(gganimate)
library(ggplot2)
library(sf)

read_hourly_data <- function(hour) {
  hour_log <- read_csv(sprintf("flight_hours/states_2022-06-27-%02d.csv/states_2022-06-27-%02d.csv", hour, hour),
                       show_col_types = FALSE) 
  hour_log$time <- as_datetime(hour_log$time)
  hour_log$time <- floor_date(hour_log$time, "5 minutes")
  hour_log <-  hour_log |> 
    filter(!onground) |> 
    filter(!is.na(lat)) |> 
    filter(!is.na(lon)) |> 
    filter(!is.na(velocity)) |> 
    group_by(icao24, time) |> 
    arrange(desc(icao24)) |>
    slice_head(n=1)
}

five_minute_flight <- read_hourly_data(0)

five_minute_flight_AAL <- five_minute_flight |> 
  filter(startsWith(callsign, "AAL"))

five_minute_flight_AAL_line <- five_minute_flight_AAL |> 
  mutate(lon2 = lon - velocity*cos((90-heading) * pi/180)/100) |> 
  mutate(lat2 = lat - velocity*sin((90-heading) * pi/180)/100) |> 
  mutate(geom = sprintf("LINESTRING(%f %f, %f %f)", lon, lat, lon2, lat2)) |> 
  st_as_sf(wkt = "geom") |> 
  st_set_crs(4326)

five_minute_flight_AAL_point <- five_minute_flight_AAL |> 
  st_as_sf(coords=c("lon","lat")) |> 
  st_set_crs(4326)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE))

animation <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=five_minute_flight_AAL_point, size=1, alpha=0.6, aes(group = icao24))+
  geom_sf(data=five_minute_flight_AAL_line, size=1, alpha=0.6, aes(group = icao24))+
  theme_void()+
  labs(title="American Airlines Flights Over A 24-hour Period", 
       subtitle="{format(frame_time, \"%I:%M %p\")} EST",
       caption="Nikhil Chinchalkar for Princeton University | Open Sky Network | 2024")+
  ylim(25,50)+
  xlim(-135,-65)+
  xlab('')+
  ylab('')+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 16, hjust =0.5, face = "bold"))+
  transition_time(time)+
  enter_fade(alpha = 0)+
  exit_fade(alpha = 0)

animate(animation, fps=15, duration=20, end_pause = 75, height = 7,
        width = 9, units = "in", res = 200)

```

library(tidyverse)
library(gganimate)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf")

data <- read_csv("earthquakes.csv")

sf_data <- data |> 
  st_as_sf(coords = c('longitude','latitude')) |> 
  st_set_crs(4326) |> 
  mutate(date = as.Date(date_time,"%d-%m-%Y")) |> 
  mutate(year = as.numeric(format(date,"%Y")))

#animation <- 
  
sf_data |> 
  ggplot() +
  geom_sf(data=world)+
  #geom_sf(data=sf_data, aes(size=magnitude, color=sig))+
  theme_minimal()+
  labs(title="Earthquakes Since 1995", 
       subtitle="Year: {next_state}",
       caption="Nikhil Chinchalkar for Princeton University | Seismic Research Dataset | 2024",
       color="Significance",
       size="Magnitude")+
  xlab('')+
  ylab('')+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 10, hjust =0.5, face = "bold"))+
  stat_density2d(data=data, aes(x=longitude, y=latitude), alpha=0.1, geom="polygon")
  
#+transition_states(year)

animate(animation, fps=5, duration=5.8, height = 7,
        width = 9, units = "in", res = 200)

dir.create("frames")

animate(
  animation,
  renderer = file_renderer(dir = "frames", prefix = "frame", overwrite = TRUE),
  fps=5, duration=5.8, height = 7,
  width = 9, units = "in", res = 200)

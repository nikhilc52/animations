library(tidyverse)
library(gganimate)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)
library(gifski)

world <- ne_countries(scale = "medium", returnclass = "sf")

temperatures <- read_csv("temperatures.csv")
temperatures$Country[which(temperatures$Country == "United States")] <- "United States of America"
temperatures$Country[which(temperatures$Country == "Tanzania")] <- "United Republic of Tanzania"
temperatures$Country[which(temperatures$Country == "Congo (Democratic Republic Of The)")] <- "Democratic Republic of the Congo"
temperatures$Country[which(temperatures$Country == "Congo")] <- "Republic of the Congo"
temperatures$Country[which(temperatures$Country == "Côte D'Ivoire")] <- "Ivory Coast"

world_temperatures <- left_join(world, temperatures, by=c("admin"="Country"))

world_temperatures$dt <- as.Date(world_temperatures$dt)

world_plot <- world_temperatures |> 
  filter(dt >= "1984-01-01") |> 
  filter(dt < "2013-09-01")

world_plot <- world_plot |> 
  mutate(year = as.integer(format(dt, '%Y')))

world_plot <- world_plot[,names(world_plot) %in% c("admin", "AverageTemperature", "year")]

world_plot <- world_plot |> 
  group_by(admin, year) |> 
  summarize(
    AverageTemperatureYear=mean(AverageTemperature),
    geometry=first(geometry)
  )

world_plot <- world_plot |> 
  group_by(admin) |> 
  #mutate(differenceInTemp = AverageTemperatureYear - lag(AverageTemperatureYear)) |>
  #ungroup() |>
  group_by(admin) |> 
  mutate(cumulativeDifferenceInTemp = AverageTemperatureYear - first(AverageTemperatureYear)) |>
  ungroup() |>
  filter(year > 1984)

animation <- ggplot(world_plot)+
  geom_sf(aes(fill=cumulativeDifferenceInTemp))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal()+
  labs(title="Cumulative Change in Average Land Temperature Since 1985", 
       subtitle="Date: {frame_time}",
       caption="Nikhil Chinchalkar for Princeton University | Berkley Earth | 2024",
       fill="Cumulative Change in Average Land Temperature (C)")+
  xlab('')+
  ylab('')+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 10, hjust =0.5, face = "bold"),
        legend.position = "bottom")+
  transition_time(year, range=c(1985L,2013L))

world_plot |> 
  filter(admin == "Canada") |> 
  ggplot()+
  geom_line(aes(x=year,y=cumulativeDifferenceInTemp))

animate(animation, fps=5, duration=30, height = 7, end_pause=25,
        width = 9, units = "in", res = 200)

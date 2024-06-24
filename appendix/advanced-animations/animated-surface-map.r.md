---
description: Code used to make the graph found within "Animated Surface Maps"
---

# animated-surface-map.R

<figure><img src="../../.gitbook/assets/final (16).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(tidyverse)
library(gganimate)
library(ggplot2)
library(sf)
library(rnaturalearth)

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
  mutate(year = as.integer(format(dt, '%Y'))) |>
  filter(year > 1984) |> 
  select(admin, AverageTemperature, year)

average_20th <- temperatures |> 
  mutate(year = as.integer(format(dt, '%Y'))) |> 
  filter(year > 1900) |> 
  filter(year < 2001) |> 
  group_by(Country) |> 
  summarize(
    average_20th=mean(AverageTemperature)
  )

world_plot <- left_join(world_plot, average_20th, by=c("admin"="Country"))

world_plot <- world_plot |>
  filter(!is.na(AverageTemperature)) |> 
  group_by(admin, year) |> 
  summarize(
    average_20th = first(average_20th),
    AverageTemperatureYear=mean(AverageTemperature),
    geometry=first(geometry),
    difference = AverageTemperatureYear-average_20th
  )

animation <- ggplot(world_plot)+
  geom_sf(aes(fill=difference))+
  transition_time(year, range=c(1985L,2013L))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal()+
  labs(title="Difference in Surface Temperatures from 20th Century Average", 
       subtitle="Date: {frame_time}",
       fill="Difference from 20th Century Average (C)")+
  xlab('')+
  ylab('')+
  theme(plot.title = ggtext::element_markdown(size = 21, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 10, hjust =0.5, face = "bold"),
        legend.position = "bottom")+
  transition_time(year, range=c(1985L,2013L))

animate(animation, fps=5, duration=30, height = 7, end_pause=25,
        width = 9, units = "in", res = 200)

```

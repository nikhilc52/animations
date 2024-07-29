---
description: Code used to make the graph found within "Animated Maps"
---

# animated-maps.R

<figure><img src="../../.gitbook/assets/final (7) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(ggplot2)
library(maps)
library(sf)
library(dplyr)
library(gganimate)
library(tidygeocoder)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE))

txhousing_data <- txhousing

cities <- data.frame(unique(txhousing_data$city))
names(cities)[names(cities) == 'unique.txhousing_data.city.'] <- 'city'

cities <- geo_osm(paste0(cities$city, ", Texas"))

txhousing_data$city <- paste0(txhousing_data$city,", Texas")
txhousing_data <- left_join(txhousing_data, cities, by=join_by('city'=='address'))

sf_txhousing_data <- txhousing_data |> 
  filter(!is.na(long)) |> 
  filter(!is.na(lat)) |> 
  st_as_sf(coords = c('long','lat')) |> 
  st_set_crs(4326)

animation <- ggplot()+
  geom_sf(data=usa)+
  geom_sf(data=sf_txhousing_data, aes(size=listings), shape=1, color="gray")+
  geom_sf(data=sf_txhousing_data, aes(size=sales, color=median))+
  viridis::scale_color_viridis(option="B", label=scales::comma)+
  scale_size_continuous(label=scales::comma, range = c(1,15))+
  theme_void()+
  labs(title="Texas Housing Data from 2000-2015",
       subtitle="The outer circle is how many houses were listed.<br>The inner circle is how many houses were sold and for what price.
       <br><br>Year: {as.integer(frame_time)}<br>",
       caption="Nikhil Chinchalkar for Princeton University | TXHousing | 2024",
       color="Median Sale Price", 
       size="Number of Listings/Sales")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  coord_sf(xlim = c(-107, -90), ylim = c(25, 37))+
  transition_time(date)

animate(animation, fps=10, duration=15, end_pause=30, height = 8,
        width = 9, units = "in", res = 200)

```

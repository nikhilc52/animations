---
description: Code used to make the graph found within "Animated Heat Maps"
---

# animated-heat-map.R

<figure><img src="../../.gitbook/assets/final (13).gif" alt=""><figcaption></figcaption></figure>

```r
library(tidyverse)
library(dplyr)
library(gganimate)
library(ggplot2)

shots <- read_csv("nba_shots.csv")

shots_plot <- shots |> 
  filter(LOC_Y<=40)

shots_plot$LOC_X <- as.integer(shots_plot$LOC_X)
shots_plot$LOC_Y <- as.integer(shots_plot$LOC_Y)
shots_plot$SEASON_1 <- as.integer(shots_plot$SEASON_1)

shots_plot <- shots_plot |> 
  group_by(LOC_X, LOC_Y, SEASON_1) |> 
  summarize(
    Count = n()
  )

animation <- plot_court() +
  geom_tile(data=shots_plot, aes(x=LOC_X, y=LOC_Y, fill=Count, group=interaction(LOC_Y, LOC_X)), alpha=0.8)+
  scale_fill_gradient2(low = "white", mid="white",high = "red", trans="log2", midpoint = 6)+
  coord_fixed(ratio=1)+
  theme_void()+
  labs(subtitle = "Year: {frame_time}",
       title = "NBA Shots Heatmap: Switch from Mid-range to 3-pointers",
       caption="Nikhil Chinchalkar for Princeton University | Dom Samangy | 2024",
       fill="# of Shots")+
  theme(plot.title = ggtext::element_markdown(size=16, hjust=0.5, face="bold"),
        plot.subtitle = ggtext::element_markdown(size=14, hjust=0.5, face="bold"))+
  transition_time(SEASON_1, range=c(2004L, 2024L))

animate(animation, fps = 10, duration = 26, end_pause = 50, height = 7,
        width = 7, units = "in", res = 200)

####plotting the court####
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

plot_court = function() {
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  court_points <- court_points
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = "black", sizes = 2
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = "black"),
      plot.background = element_rect(fill = 'gray15', color = 'gray15'),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = "white", color = "white"),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}
```

---
description: Code used to make the graph found within "Animated Scatter Plots"
---

# animated-scatter-plot.R

<figure><img src="../../.gitbook/assets/final (12).gif" alt=""><figcaption></figcaption></figure>

```
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyverse)

athletes <- read_csv("athletes.csv")

height_weight <- athletes |> 
  filter(!is.na(Height)) |> 
  filter(!is.na(Weight)) |> 
  filter(Sport == "Swimming") |> 
  group_by(Year, Event, Sex) |> 
  summarize(
    Height = mean(Height),
    Weight = mean(Weight),
    Athlete_Count = n(),
    Sport = first(Sport)
  ) |> 
  filter(Year >= 1960)

animation <- height_weight |> 
  ggplot()+
  geom_point(aes(x=Height, y=Weight, color=Sex, group=Event))+
  labs(title="Swimmers Have Gotten Bigger and Stronger Over Time", 
       subtitle="Each Dot Represents the Average Height/Weight for A Given Olympic Swimming Event
       <br>Year: {next_state}",
       caption="Nikhil Chinchalkar for Princeton University | Sports Reference | 2024")+
  scale_color_manual(breaks=c("F","M"), labels=c("Female","Male"),
                     values=c("#8700f9", "#00c4aa"))+
  theme_minimal()+
  xlab("Height (cm)")+
  ylab("Weight (kg)")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_states(Year, wrap=FALSE, transition_length = 1, state_length = 1)+
  ease_aes("cubic-in-out")+
  shadow_trail(alpha=0.1)
  
animate(animation, fps=10, duration=18, end_pause=30, height = 7,
        width = 11, units = "in", res = 200)
```

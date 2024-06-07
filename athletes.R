library(ggplot2)
library(gganimate)
library(ggimage)
library(ggtext)
library(dplyr)

athletes <- read_csv("athletes.csv")

height_weight <- athletes |> 
  filter(!is.na(Height)) |> 
  filter(!is.na(Weight)) |> 
  filter(Sport == "Swimming") |> 
  mutate(Full_Name = paste(City, Year)) |> 
  group_by(Year, Event, Sex) |> 
  summarize(
    Height = mean(Height),
    Weight = mean(Weight),
    Athlete_Count = n(),
    Full_Name = first(Full_Name), 
    Sport = first(Sport)
  ) |> 
  filter(Year >= 1960)

animation <- height_weight |> 
  ggplot()+
  geom_point(aes(x=Height, y=Weight, color=Sex, group=Event))+
  labs(title="Bigger and Stronger: Swimmers' Heights and Weights Over Time", 
       subtitle="Each Dot Represents the Average Height/Weight for A Given Swimming Event
       <br>Year: {next_state}",
       caption="Nikhil Chinchalkar for Princeton University | Sports Reference | 2024")+
  scale_color_manual(breaks=c("F","M"), labels=c("Female","Male"),
                     values=c("#8700f9", "#00c4aa"))+
  theme_minimal()+
  xlab("Height (cm)")+
  ylab("Weight (kg)")+
  theme(plot.title = element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_states(Year, wrap=FALSE, transition_length = 1, state_length = 1)+
  ease_aes("cubic-in-out")+
  shadow_trail(alpha=0.1)
  
animate(animation, fps=10, nframes=180, end_pause=30, height = 7,
        width = 11, units = "in", res = 200)

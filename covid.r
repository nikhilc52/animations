library(tidyverse)
library(readxl)
library(gganimate)

world <- read_excel("covid.xlsx")

world$date <- as.Date(world$date)

world <- world |> 
  filter(!is.na(total_cases)) |> 
  filter(date < "2023-01-01")

world['label1'] = "First U.S. Death"
world['label2'] = "CDC Mask Recommendation"
world['label3'] = "Vaccines Released in the U.S."
world['label4'] = "FDA Authorizes Booster Shots"
world['label5'] = "WHO Declares Omnicron Variant of Concern"

world <- world |> 
  mutate(label1alpha = ifelse(date >= as.Date('2020-02-06'), 1, 0.1)) |> 
  mutate(label2alpha = ifelse(date >= as.Date('2020-04-03'), 1, 0.1)) |> 
  mutate(label3alpha = ifelse(date >= as.Date('2020-12-14'), 1, 0.1)) |> 
  mutate(label4alpha = ifelse(date >= as.Date('2021-11-19'), 1, 0.1)) |> 
  mutate(label5alpha = ifelse(date >= as.Date('2021-11-26'), 1, 0.1))


animation <- world |> 
  ggplot(aes(x=date, y=new_deaths_smoothed))+
  geom_line()+
  theme_minimal()+
  labs(title="New COVID-19 Worldwide Deaths", 
       subtitle="Date: {frame_along}",
       caption="Nikhil Chinchalkar for Princeton University | OurWorldInData | 2024")+
  xlab('Date')+
  ylab('New Deaths Per Week')+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 10, hjust =0.5, face = "bold"),
        axis.text.x=element_text(size=10))+
  geom_text(aes(label=label1, alpha=label1alpha), x=as.Date('2020-02-06'), y=10000)+
  geom_text(aes(label=label2, alpha=label2alpha), x=as.Date('2020-04-03'), y=15000)+
  geom_text(aes(label=label3, alpha=label3alpha), x=as.Date('2020-12-14'), y=2500)+
  geom_text(aes(label=label4, alpha=label4alpha), x=as.Date('2021-11-19'), y=3000)+
  geom_text(aes(label=label5, alpha=label5alpha), x=as.Date('2021-11-26'), y=12500)+
  geom_segment(aes(xend=as.Date('2020-02-06'), yend=world$new_deaths_smoothed[world$date==as.Date("2020-02-06")], 
                   x=as.Date('2020-02-06'),y=10000, alpha=label1alpha), linetype="dashed")+
  geom_segment(aes(xend=as.Date('2020-04-03'), yend=world$new_deaths_smoothed[world$date==as.Date("2020-04-03")], 
                   x=as.Date('2020-04-03'),y=15000, alpha=label2alpha), linetype="dashed")+
  geom_segment(aes(xend=as.Date('2020-12-14'), yend=world$new_deaths_smoothed[world$date==as.Date("2020-12-14")], 
                   x=as.Date('2020-12-14'),y=0, alpha=label3alpha), linetype="dashed")+
  geom_segment(aes(xend=as.Date('2021-11-19'), yend=world$new_deaths_smoothed[world$date==as.Date("2021-11-19")], 
                   x=as.Date('2021-11-19'),y=0, alpha=label4alpha), linetype="dashed")+
  geom_segment(aes(xend=as.Date('2021-11-26'), yend=world$new_deaths_smoothed[world$date==as.Date("2021-11-26")], 
                   x=as.Date('2021-11-26'),y=12500, alpha=label5alpha), linetype="dashed")+
  scale_alpha(guide='none')+
  transition_reveal(date)

animate(animation, fps = 15, duration = 30, end_pause=75, height = 7,
        width = 9, units = "in", res = 200)

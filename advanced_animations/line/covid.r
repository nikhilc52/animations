library(gganimate)
library(ggplot2)
library(dplyr)
library(hash)

covid <- readxl::read_excel("covid.xlsx")

covid$date <- as.Date(covid$date)

world <- covid |> 
  filter(location=="World") |> 
  filter(!is.na(total_cases)) |> 
  filter(date < "2023-01-01") |> 
  select(date, new_deaths_smoothed)

annotations <- data.frame(keys=list(), values=list())

annotations <- data.frame(keys='First U.S. Death', values='2020-02-06')
annotations <- add_row(annotations, keys='CDC Mask Recommendation', values='2020-04-03')
annotations <- add_row(annotations, keys='Vaccines Released in the U.S.', values='2020-12-14')
annotations <- add_row(annotations, keys='FDA Authorizes Booster Shots', values='2021-11-19')
annotations <- add_row(annotations, keys='WHO Declares Omnicron Variant of Concern', values='2021-11-26')

bullet_effect <- function(dates_to_slow, strength){
  world$show_time = case_when(world$date %in% dates_to_slow ~ strength, TRUE~1)
  world$reveal_time = cumsum(world$show_time)
  return(world)
}

world <- bullet_effect(annotations$values, 100)

ylimit <- 15000

find_y_value <- function(data_value, y_lim, index){
  if(index %% 2 == 1){
    y_value = ifelse(data_value + y_lim/5 < y_lim, data_value + y_lim/5, data_value - y_lim/5)
  }
  else{
    y_value = ifelse(data_value - y_lim/5 > 1000, data_value - y_lim/5, data_value + y_lim/5)
  }
  return(y_value)
}

plot_text_annotation <- function(){
  text_list <- geom_label(y=0,x=0,label="")
  for(i in 1:length(annotations$values)){
    y_value <- find_y_value(world$new_deaths_smoothed[world$date==as.Date(annotations$values[i])], ylimit, i)
    
    text_list <- c(text_list,geom_label(label=as.character(annotations$keys[i]),
                                        color=scales::alpha('black',ifelse(world$date>=as.Date(annotations$values[i]), 1, 0)),
                                        x=as.Date(annotations$values[i]),
                                        y=y_value))
  }
  return(text_list)
}

plot_segment_annotation <- function(){
  segment_list <- geom_segment(y=0,x=0,xend=0,yend=0)
  for(i in 1:length(annotations$values)){
    y_value <- find_y_value(world$new_deaths_smoothed[world$date==as.Date(annotations$values[i])], ylimit, i)
    segment_list <- c(segment_list, geom_segment(xend=as.Date(annotations$values[i]),
                                                 yend=world$new_deaths_smoothed[world$date==as.Date(annotations$values[i])],
                                                 x=as.Date(annotations$values[i]),y=y_value,
                                                 alpha=ifelse(world$date>=as.Date(annotations$values[i]), 1, 0),
                                                 linetype="dashed"))
  }
  return(segment_list)
}

animation <- world |> 
  ggplot(aes(x=date, y=new_deaths_smoothed))+
  theme_minimal()+
  labs(title="New COVID-19 Worldwide Deaths", 
       subtitle="{world$date[min(which(world$reveal_time >= floor(frame_along)))]}",
       caption="Nikhil Chinchalkar for Princeton University | OurWorldInData | 2024")+
  xlab('Date')+
  ylab('New Deaths Per Week')+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 10, hjust =0.5, face = "bold"),
        axis.text.x=element_text(size=10))+
  ylim(0,ylimit)+
  plot_segment_annotation()+
  geom_line()+
  plot_text_annotation()+
  scale_alpha(guide='none')+
  transition_reveal(reveal_time)

animate(animation, fps = 15, duration = 30, end_pause=75, height = 7,
        width = 9, units = "in", res = 200)


---
description: Code used to make the graph found within "Animated Word Clouds"
---

# animated-word-cloud.R

<figure><img src="../../.gitbook/assets/final (3) (1) (1).gif" alt=""><figcaption></figcaption></figure>

```r
library(tidyverse)
library(dplyr)
library(gganimate)
library(ggwordcloud)
library(stopwords)
library(tokenizers)
library(ggplot2)

headlines <- read_csv("headlines.csv")

nyt_headlines <- headlines |> 
  filter(Publication == "New York Times")

nyt_headlines$Date <- as.Date(as.character(nyt_headlines$Date), format="%Y%m%d")

nyt_headlines <- nyt_headlines |> 
  filter(!is.na(Headline)) |> 
  mutate(Year_Month = as.Date(paste0(format(Date, "%Y"), format(Date, "%m"), "01"), format="%Y%m%d")) |> 
  filter(Year_Month < "2023-01-01")

nyt_headlines <- nyt_headlines |> 
  group_by(Year_Month) |> 
  summarize(
    Headlines = paste(Headline, collapse = " "),
    Words = unlist(tokenize_words(Headlines, stopwords = stopwords("en")))
  )

nyt_headlines <- nyt_headlines[-2]

word_count <- nyt_headlines |> 
  group_by(Words, Year_Month) |> 
  summarize(
    count = n()
  ) 

word_count_five <- word_count |> 
  filter(!(tolower(Words) %in% c("new", "york", "times", "nyt"))) |> 
  filter(!(tolower(Words) %in% c("min", "read", "review", "editorial", "now",
                                 "comments", "getty", "images", "theater",
                                 "movie", "slide", "show", "television", "books",
                                 "op","ed"))) |> 
  filter(!grepl("[^A-Za-z0-9 ]", Words)) |> 
  filter(!(Words == "")) |>
  filter(nchar(Words) > 1) |>  
  group_by(Year_Month) |> 
  arrange(desc(count)) |> 
  slice_head(n = 5)

word_count_one <- word_count |> 
  filter(!(tolower(Words) %in% c("new", "york", "times", "nyt"))) |> 
  filter(!(tolower(Words) %in% c("min", "read", "review", "editorial", "now",
                                 "comments", "getty", "images", "theater",
                                 "movie", "slide", "show", "television", "books",
                                 "op","ed"))) |> 
  filter(!grepl("[^A-Za-z0-9 ]", Words)) |> 
  filter(!(Words == "")) |>
  filter(nchar(Words) > 1) |>  
  group_by(Year_Month) |> 
  arrange(desc(count)) |> 
  slice_head(n = 1)

set.seed(52)

word_cloud <- word_count_five |> 
  ggplot(aes(label=Words, size=count))+
  geom_text_wordcloud()+
  theme_minimal()+
  scale_size_area(max_size = 20)+
  transition_time(Year_Month)+
  labs(subtitle = "{paste(month.name[as.numeric(format(as.Date(frame_time),\"%m\"))],
       format(as.Date(frame_time),\"%Y\"))}",
       title = "5 Most-Used Words In NYT Headlines Each Month (2007-2022)",
       caption="Nikhil Chinchalkar for Princeton University | New York Times | 2024")+
  theme(plot.title = ggtext::element_markdown(size=16, hjust=0.5, face="bold"),
        plot.subtitle = ggtext::element_markdown(size=20, hjust=0.5), face="bold")

word_count_one$Year_Month <- as.Date(word_count_one$Year_Month)

year_log <- function(date, index){
  print(date)
  plot <- ggplot(word_count_one)+
    table_axis(date)+
    table_titles(date)+
    geom_text(x=year(date),y=12-month(date), label=word_count_one$Words[which(word_count_one$Year_Month == floor_date(date, "month"))], fontface="plain")+
    xlim(2005,2023)+
    ylim(-1,13)+
    theme_void()
  ggsave(plot=plot, file=paste0("image_sequence/",formatC(index, width = 3, format = "d", flag = "0"),".png"), width=15, height=7, units="in",dpi=150)
}

table_axis <- function(date){
  label <- geom_text(y=0,x=0,label="")
  if (year(date) == 2007){
    label <- geom_text(y=12-month(date),x=2006,label=months(date), fontface="bold", color="gray")
  }
  if(month(date) == 1){
    if(year(date) == 2007){
      label <- c(label,geom_text(y=12, x=year(date), label=year(date), fontface="bold", color="gray"))
    }
    else{
      label <- geom_text(y=12, x=year(date), label=year(date), fontface="bold", color="gray")
    }
  }
  return(label)
}

table_titles <- function(date){
  if(date == as.Date("2007-01-01")){
    return(geom_text(y=13, x=2014.5, label="Most-Used Word In NYT Headlines Each Month", fontface="bold", color="black", size=8))
  }
}


combined_year_log <- function(start_date){
  end_date <- as.Date("2022-12-01")
  curr_date <- start_date
  index <- 0
  while(curr_date <= end_date){
    year_log(curr_date, index)
    index <- index + 1
    curr_date <- curr_date %m+% months(1)
  }
}

system.time(combined_year_log(as.Date("2007-01-01")))

png_files <- sort(list.files("image_sequence", pattern = ".*png$", full.names = TRUE))
gifski::gifski(png_files, gif_file = "animation.gif", width = 1500*1.5, height = 700*1.5, delay = 1)
mgif_word_log <- magick::image_read("animation.gif")

mgif_word_cloud <- magick::image_read(animate(word_cloud, fps = 4, duration = 48, height = 7,
        width = 7, units = "in", res = 150))

new_gif <- magick::image_append(c(mgif_word_cloud[1], mgif_word_log[1]))
for(i in 2:192){
  combined <- magick::image_append(c(mgif_word_cloud[i], mgif_word_log[i]))
  new_gif <- c(new_gif, combined)
}
for(x in 1:30){
  combined <- magick::image_append(c(mgif_word_cloud[192], mgif_word_log[192]))
  new_gif <- c(new_gif, combined)
}

magick::image_write(new_gif, path = "final.gif", format = "gif")

new_gif <- magick::image_read("final.gif")
new_gif <- magick::image_animate(new_gif, fps = 4)
magick::image_write(new_gif, path = "final.gif", format = "gif")

```

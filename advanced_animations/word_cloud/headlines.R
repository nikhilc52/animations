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

word_count <- word_count |> 
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

set.seed(52)

word_cloud <- word_count |> 
  ggplot(aes(label=Words, size=count))+
  geom_text_wordcloud()+
  theme_minimal()+
  scale_size_area(max_size = 20)+
  transition_time(Year_Month)+
  labs(subtitle = "{paste(month.name[as.numeric(format(as.Date(frame_time),\"%m\"))],
       format(as.Date(frame_time),\"%Y\"))}",
       title = "5 Most-Used Words In NYT Headlines Each Month (2007-2022)")+
  theme(plot.title = element_text(size=16, hjust=0.5),
        plot.subtitle = element_text(size=20, hjust=0.5))

animate(word_cloud, fps = 4, duration = 68, end_pause = 20, height = 7,
        width = 7, units = "in", res = 200)

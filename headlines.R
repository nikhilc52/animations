library(tidyverse)
library(dplyr)
library(gganimate)
library(ggwordcloud)
library(stopwords)
library(tokenizers)

headlines <- read_csv("headlines.csv")

nyt_headlines <- headlines |> 
  filter(Publication == "New York Times")

nyt_headlines$Date <- as.Date(as.character(nyt_headlines$Date), format="%Y%m%d")

nyt_headlines <- nyt_headlines |> 
  filter(!is.na(Headline)) |> 
  mutate(Year = format(Date, "%Y"))

nyt_headlines <- nyt_headlines |> 
  group_by(Year) |> 
  summarize(
    Headlines = paste(Headline, collapse = " "),
    Words = unlist(tokenize_words(Headlines, stopwords = stopwords("en")))
  )

nyt_headlines <- nyt_headlines[-2]

word_count <- nyt_headlines |> 
  group_by(Words, Year) |> 
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
  group_by(Year) |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

word_count$Year <- as.numeric(word_count$Year)

word_cloud <- word_count |> 
  ggplot(aes(label=Words, size=count))+
  geom_text_wordcloud()+
  theme_minimal()+
  scale_size_area(max_size = 30)+
  transition_time(Year)+
  labs(subtitle = "Year: {frame_time}",
       title = "Top Words Of NYT Headlines")+
  theme(plot.title = element_text(size=16, hjust=0.5),
        plot.subtitle = element_text(size=14, hjust=0.5))

animate(word_cloud, fps = 1, duration = 19, end_pause = 3, height = 7,
        width = 7, units = "in", res = 200)

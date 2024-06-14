---
description: Code used to make the graph found within "Animated Word Clouds"
---

# animated-word-cloud.R

<figure><img src="../../.gitbook/assets/final (4).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(ggplot2)
library(dplyr)
library(ggwordcloud)
library(gganimate)

txhousing_data <- txhousing

txhousing_data <- txhousing_data |> 
  group_by(date) |> 
  arrange(desc(volume)) |> 
  slice_head(n=5)

set.seed(52)

animation <- ggplot(txhousing_data)+
  geom_text_wordcloud(aes(label=city, size=volume, group=city))+
  scale_size_area(max_size = 30)+
  transition_time(date)+
  labs(title="Texas Cities With The Most Money Spent in Real Estate",
       subtitle="Since 2000, the top five real estate spenders have remained the same, <br>with Houston and Dallas being significantly ahead of the rest.<br><br>{as.integer(frame_time)}",
       caption="Nikhil Chinchalkar for Princeton University | TXHousing | 2024")+
  theme_minimal()+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(date)

animate(animation, fps=3, duration=35, end_pause=9, height = 8,
        width = 8, units = "in", res = 200)

```

---
description: Code used to make the graph found within "Animated Scatter Plots"
---

# animated-scatter-plot.R

<figure><img src="../../.gitbook/assets/final (6).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(ggplot2)
library(dplyr)
library(gganimate)

txhousing_data <- txhousing

animation <- ggplot(txhousing_data)+
  geom_point(aes(x=sales, y=median, group=city))+
  scale_x_continuous(label=scales::comma, transform = "log10")+
  scale_y_continuous(label=scales::comma)+
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House prices and sales for each dot/city have increased since 2000, <br>especially post-2008.<br><br>Year: {as.integer(frame_time)}",
       caption="Nikhil Chinchalkar for Princeton University | TXHousing | 2024",
       x="Sales",
       y="Median Sale Price")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(date)

animate(animation, fps=30, duration=60, end_pause=150, height = 7,
        width = 9, units = "in", res = 200)
```

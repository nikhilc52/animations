---
description: Code used to make the graph found within "Animated Line Graphs"
---

# animated-line-graph.R

<figure><img src="../.gitbook/assets/final (2) (1) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(ggplot2)
library(dplyr)
library(gganimate)

txhousing_data <- txhousing

txhousing_data <- txhousing_data |> 
  group_by(date) |> 
  filter(!is.na(sales)) |> 
  filter(!is.na(listings)) |> 
  summarise(
    sales = sum(sales),
    listings = sum(listings)
  )

animation <- ggplot(txhousing_data)+
  geom_line(aes(x=date, y=sales, color="Sales"))+
  geom_line(aes(x=date, y=listings, color="Listings"))+
  scale_color_manual(breaks = c("Sales", "Listings"), values=c("#1f948b","#482071"))+
  scale_y_continuous(label=scales::comma)+
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House sales are cyclical and somewhat consistent, listings are more volatile.",
       caption="Nikhil Chinchalkar for Princeton University | TXHousing | 2024",
       color="",
       x="Year",
       y="Number of Sales/Listings")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_reveal(date)

animate(animation, fps=10, duration=15, end_pause=9, height = 7,
        width = 9, units = "in", res = 200)

```

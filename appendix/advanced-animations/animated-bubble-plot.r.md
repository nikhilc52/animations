---
description: Code used to make the graph found within "Animated Bubble Plots"
---

# animated-bubble-plot.R

<figure><img src="../../.gitbook/assets/final (3) (1) (1) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(ggplot2)
library(gganimate)
library(viridis)
library(gapminder)
library(plotly)

gapminder_data <- gapminder

animation <- ggplot(gapminder_data, aes(frame=year))+
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, fill=continent, group=country, ids=country), 
             shape=21, alpha=0.7)+
  scale_size_continuous(label=scales::comma, range = c(1,20))+
  scale_x_continuous(label=scales::comma, trans='log2')+
  theme_minimal()+
  scale_fill_viridis(option='A', discrete=TRUE)+
  scale_color_viridis(option='A', discrete=TRUE)+
  labs(title="GDP Per Capita and Life Expectancy Over 50+ Years",
       subtitle="Year: {frame_time}",
         caption="Nikhil Chinchalkar for Princeton University | Gapminder | 2024",
       size="Population",
       fill="Continent",
       x="GDP Per Capita",
       y="Life Expectancy")+
  guides(color="none")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(year)

ggplotly(animation)

animate(animation, fps=10, duration=30, end_pause=50, height = 7, width = 10, 
        units = "in", res = 200)
```

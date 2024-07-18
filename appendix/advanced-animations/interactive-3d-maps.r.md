---
description: Code used to make the graph found within "Interactive 3D Maps"
---

# interactive-3d-maps.R

{% embed url="https://nikhilc52.github.io/animation_links/" %}

```r
library(plotly)
library(tidyverse)
library(dplyr)
library(stars)

earthquakes <- read_csv("earthquakes.csv")

degrees2radians <- function(degree) degree * pi / 180 

earthquakes <- earthquakes |> 
  mutate(x=1.01 * cos(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(latitude)))

manual_colorscale <- list(c(0,0.2,0.4,0.6,0.8,1),
                          c("white","#ffa590","#ff8164", "#ff6242","#fb4b1e","#c61a09"))


x_size <- 1000
y_size <- 500

raw_tif <- read_stars("surface.png",RasterIO = list(nBufXSize=x_size,nBufYSize=y_size))

rgb <- as.data.frame(raw_tif) |> 
  tidyr::pivot_wider(names_from = band, values_from = surface.png)

colnames(rgb)[3] <- "red"
colnames(rgb)[4] <- "green"
colnames(rgb)[5] <- "blue"

rgb$color <- rgb(rgb$red/255,rgb$green/255,rgb$blue/255)
rgb$color_int <- 256 * 256 * rgb$red + rgb$green * 256 + rgb$blue 

rgb_earth <- matrix(data=rgb$color_int, nrow=y_size, ncol = x_size, byrow=TRUE)

lat <- seq(-90, 90, length.out = y_size)
lon <- seq(-180, 180, length.out = x_size)
lat <- matrix(rep(lat, x_size), nrow = y_size)
lon <- matrix(rep(lon, each = y_size), nrow = y_size)

earth_colorscale <- distinct(data.frame(rgb$color_int, rgb$color))
earth_colorscale <- earth_colorscale |> arrange(rgb.color_int)

while(nrow(earth_colorscale) > 255){
  toDelete <- seq(0, nrow(earth_colorscale), 2)
  earth_colorscale <- earth_colorscale[toDelete, ]
  rownames(earth_colorscale) = NULL
}

earth_colorscale$breaks <- seq(1:nrow(earth_colorscale))/nrow(earth_colorscale)
earth_colorscale$breaks[1] = 0
earth_colorscale <- earth_colorscale[,c(3,2)]
names(earth_colorscale)[names(earth_colorscale) == 'rgb.color'] <- 'colors'

empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  showspikes = FALSE,
  spikesides = FALSE,
  title = ""
)

image <- RCurl::base64Encode(readBin("size_scale.png", "raw", file.info("size_scale.png")[1, "size"]), "txt")

globe <- plot_ly(width=800,height=800) |> 
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)),
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), size = I(1),
    hoverinfo = "none"
  ) |>
  add_trace(
    x=earthquakes$x,
    y=earthquakes$y,
    z=earthquakes$z,
    mode = "markers", type = "scatter3d",
    marker = list(color = earthquakes$magnitude, size = earthquakes$sig/100, colorscale = manual_colorscale, showscale=TRUE,
                  colorbar=list(title=list(text="Magnitude",side="top"), thickness=10, len=0.35, orientation='h', y=0.1,
                                tickfont=list(family="Arial"), nticks=6)),
    text = paste0("Description: ", earthquakes$title, "<br>", "Time: ", earthquakes$date_time, "<br>" ,
                  "Magnitude: ", earthquakes$magnitude, "<br>", "Significance: ", earthquakes$sig),
    hoverinfo = "text"
  ) |>
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = cos(degrees2radians(lat)) * sin(degrees2radians(lon)),
    z = -sin(degrees2radians(lat)),
    surfacecolor=rgb_earth,
    colorscale=earth_colorscale,
    showscale = FALSE, 
    hoverinfo = "none",
    lightposition = list(
      x=2,
      y=2,
      z=2
    ),
    contours = list(
      x = list(highlight = FALSE), 
      y = list(highlight = FALSE), 
      z = list(highlight = FALSE)
    )
  ) |>
  layout(
    title=list(text="<br><b>Earthquakes Since 1995</b>",font=list(family="Arial", size=30)),
    showlegend = FALSE,
    annotations=list(text="Nikhil Chinchalkar For Princeton University | Seismic Research Dataset | 2024",
                     showarrow=FALSE, font=list(family="Arial", size=14), y=0),
    images=list(list(source=paste('data:image/png;base64', image, sep=','),
                     xref="paper",
                     yref="paper",
                     x=0.5,
                     y=0.1,
                     sizex=.25,
                     sizey=.25,
                     xanchor="center",
                     yanchor="center")),
    scene = list(
      xaxis = empty_axis,
      yaxis = empty_axis,
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1)
    )
  ) |> 
  config(displayModeBar=FALSE)

globe
```

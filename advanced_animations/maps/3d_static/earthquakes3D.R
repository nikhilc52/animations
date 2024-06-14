library(plotly)
library(tidyverse)
library(dplyr)
library(htmltools)
library(stars)
library(ggplot2)

earthquakes <- read_csv("earthquakes.csv")

degrees2radians <- function(degree) degree * pi / 180 

earthquakes <- earthquakes |> 
  mutate(x=1.01 * cos(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(latitude)))

x_size <- 2048
y_size <- 1024
raw_tif <- read_stars("land_shallow_topo_2048.tif",
                   RasterIO = list(nBufXSize=x_size, nBufYSize=y_size))

df_tif <- as.data.frame(raw_tif)
df_tif <- df_tif |>
  mutate(x = x-180) |>
  mutate(y = y-90)

red <- df_tif |> 
  filter(band == 1) |> 
  mutate(red = land_shallow_topo_2048.tif)
red <- red[-c(3,4)]

green <- df_tif |> 
  filter(band == 2) |> 
  mutate(green = land_shallow_topo_2048.tif)
green <- green[-c(3,4)]

blue <- df_tif |> 
  filter(band == 3) |> 
  mutate(blue = land_shallow_topo_2048.tif)
blue <- blue[-c(3,4)]

rgb <- left_join(left_join(red, green),blue)
rgb$color <- rgb(rgb$red/255,rgb$green/255,rgb$blue/255)
rgb$bw <- rgb$red*0.2126+rgb$green*0.7152+rgb$blue*0.0722
rgb$color_int <- bitwShiftL(rgb$red, 16) + bitwShiftL(rgb$green, 8) + rgb$blue 

rgb_earth <- matrix(data=rgb$color_int, nrow=y_size, ncol = x_size, byrow=TRUE)

colorscale <- distinct(data.frame(rgb$color_int, rgb$color))
colorscale <- colorscale |> arrange(rgb.color_int)

while(nrow(colorscale) > 255){
  toDelete <- seq(0, nrow(colorscale), 2)
  colorscale <- colorscale[toDelete, ]
  rownames(colorscale) = NULL
}

colorscale$breaks <- seq(1:nrow(colorscale))/nrow(colorscale)
colorscale$breaks[1] = 0
colorscale <- colorscale[,c(3,2)]
names(colorscale)[names(colorscale) == 'rgb.color'] <- 'colors'

empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  showspikes = FALSE,
  spikesides = FALSE,
  title = ""
)

nlat <- y_size #match y
nlon <- x_size #match x
lat <- seq(-90, 90, length.out = nlat)
lon <- seq(-180, 180, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)

globe <- plot_ly(height = 600) |> 
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)),
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), size = I(1),
    hoverinfo = "none"
  ) |>
  add_trace(
    data=earthquakes,
    x=earthquakes$x,
    y=earthquakes$y,
    z=earthquakes$z,
    mode = "markers", type = "scatter3d",
    color = ~earthquakes$sig, size = 5,
    text = paste("Description: ", earthquakes$title, "<br>", "Time: ", earthquakes$date_time),
    hoverinfo = "text"
  ) |>
  add_surface(
    data=rgb,
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = cos(degrees2radians(lat)) * sin(degrees2radians(lon)),
    z = -sin(degrees2radians(lat)),
    surfacecolor=rgb_earth,
    colorscale=colorscale,
    showscale = FALSE, 
    hoverinfo = "none",
    lightposition = list(
      x=0.41,
      y=-0.71,
      z=0.57
    ),
    contours = list(
      x = list(highlight = FALSE), 
      y = list(highlight = FALSE), 
      z = list(highlight = FALSE)
    )
  ) |>
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = empty_axis,
      yaxis = empty_axis,
      zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1)
    )
  )

browsable(globe)

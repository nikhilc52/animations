library(plotly)
library(dplyr)
library(htmltools)

earthquakes <- read_csv("earthquakes.csv")

degrees2radians <- function(degree) degree * pi / 180 

earthquakes <- earthquakes |> 
  mutate(x=1.01 * cos(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(latitude)))

nlat <- 200
nlon <- 100
lat <- seq(-180, 180, length.out = nlat)
lon <- seq(-90, 90, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)

empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  showspikes = FALSE,
  spikesides = FALSE,
  title = ""
)

globe <- plot_ly(height = 600) %>%
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
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
    showscale = FALSE, hoverinfo = "skip"
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
  
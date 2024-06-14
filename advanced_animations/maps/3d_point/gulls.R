library(plotly)
library(dplyr)
library(htmltools)

gulls <- read.csv("gulls.csv")

gulls <- gulls |> 
  group_by(day, bird_name) |> 
  summarize(
    device_info_serial=first(device_info_serial),
    latitude=first(latitude),
    longitude=first(longitude),
    date_time=first(date_time),
    direction=first(direction),
    speed_2d=first(speed_2d),
    altitude=first(altitude)
  )

gulls$day <- as.character(gulls$day)

degrees2radians <- function(degree) degree * pi / 180 

gulls <- gulls |> 
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

globe <- gulls |>  
  plot_ly(height = 600) |> 
  add_sf(
    data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    z = ~ 1.001 * sin(degrees2radians(y)),
    color = I("black"), size = I(1),
    hoverinfo = "none"
  ) |> 
  add_trace(
    data=gulls,
    x=~gulls$x,
    y=~gulls$y,
    z=~gulls$z,
    mode = "markers", type = "scatter3d",
    size = 15,
    color= ~device_info_serial,
    text = paste("Name: ", gulls$bird_name, "<br>",
                 "Altitude: ", gulls$altitude, "<br>",
                 "Direction: ", gulls$direction, "<br>",
                 "Speed: ", gulls$speed_2d, "<br>",
                 "Day/Time: ", gulls$date_time, "<br>"),
    frame = ~day,
    hoverinfo = "text"
  ) |> 
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = sin(degrees2radians(lat)),
    surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
    showscale = FALSE, hoverinfo = "skip",
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
    ),
    title="Gulls Migrating South for Winter"
  )

browsable(globe)

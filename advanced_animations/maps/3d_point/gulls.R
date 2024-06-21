library(plotly)
library(dplyr)
library(htmltools)
library(stars)

degrees2radians <- function(degree) degree * pi / 180 

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

gulls <- gulls |> 
  mutate(x=1.01 * cos(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(longitude)) * cos(degrees2radians(latitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(latitude)))

x_size <- 500
y_size <- 250
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
rgb$color_int <- bitwShiftL(rgb$red, 16) + bitwShiftL(rgb$green, 8) + rgb$blue 

rgb_earth <- matrix(data=rgb$color_int, nrow=y_size, ncol = x_size, byrow=TRUE)

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

nlat <- y_size
nlon <- x_size
lat <- seq(-90, 90, length.out = nlat)
lon <- seq(-180, 180, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)


start_date <- as.Date("2013-08-15")
end_date <- as.Date("2014-04-30")
curr_date <- start_date
image_index <- 1
system.time(
while(curr_date <= end_date){
  print(curr_date)
  curr_date <- curr_date+1
  curr_gulls <- gulls |> 
    filter(day == curr_date)
  camera_x <- mean(curr_gulls$x)
  camera_y <- mean(curr_gulls$y)
  camera_z <- mean(curr_gulls$z)
  globe <- curr_gulls |>  
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
      x=~curr_gulls$x,
      y=~curr_gulls$y,
      z=~curr_gulls$z,
      mode = "markers", type = "scatter3d",
      marker = list(color = curr_gulls$device_info_serial, size = 7),
      text = paste("Name: ", curr_gulls$bird_name, "<br>",
                   "Altitude: ", curr_gulls$altitude, "<br>",
                   "Direction: ", curr_gulls$direction, "<br>",
                   "Speed: ", curr_gulls$speed_2d, "m/s <br>",
                   "Day/Time: ", curr_gulls$date_time, "<br>"),
      hoverinfo = "text"
    ) |> 
    add_surface(
      x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
      y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
      z = -sin(degrees2radians(lat)),
      surfacecolor = rgb_earth,
      colorscale=earth_colorscale,
      showscale = FALSE, hoverinfo = "none",
      lightposition = list(
        x=1,
        y=0,
        z=0.6
      ),
      contours = list(
        x = list(highlight = FALSE), 
        y = list(highlight = FALSE), 
        z = list(highlight = FALSE)
      )
    ) |> 
    layout(
      title=list(text="<br><b>Gull Migration</b>",font=list(family="Arial", size=24)),
      showlegend = FALSE,
      annotations=list(text=paste0("Day: ", curr_date ,"<br>Nikhil Chinchalkar For Princeton University | LifeWatch INBO | 2024"),
                       showarrow=FALSE, font=list(family="Arial", size=14), y=0),
      scene = list(
        xaxis = empty_axis,
        yaxis = empty_axis,
        zaxis = empty_axis,
        aspectratio = list(x = 1, y = 1, z = 1),
        camera = list(eye=list(x=camera_x*2,y=camera_y,z=camera_z))
      )
    )
  orca(globe, paste0("image_sequence/",image_index,".png"))
  image_index <- image_index + 1
}
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
    marker = list(color = gulls$device_info_serial, size = 7),
    text = paste("Name: ", gulls$bird_name, "<br>",
                 "Altitude: ", gulls$altitude, "<br>",
                 "Direction: ", gulls$direction, "<br>",
                 "Speed: ", gulls$speed_2d, "m/s <br>",
                 "Day/Time: ", gulls$date_time, "<br>"),
    frame = ~day,
    hoverinfo = "text"
  ) |> 
  add_surface(
    x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    z = -sin(degrees2radians(lat)),
    surfacecolor = rgb_earth,
    colorscale=earth_colorscale,
    showscale = FALSE, hoverinfo = "none",
    lightposition = list(
      x=0.5,
      y=0.8,
      z=0.4
    ),
    contours = list(
      x = list(highlight = FALSE), 
      y = list(highlight = FALSE), 
      z = list(highlight = FALSE)
    )
  ) |> 
  layout(
    title=list(text="<br><b>Gull Migration</b>",font=list(family="Arial", size=24)),
    showlegend = FALSE,
    annotations=list(text="Nikhil Chinchalkar For Princeton University | LifeWatch INBO | 2024",
                     showarrow=FALSE, font=list(family="Arial", size=14), y=0),
    scene = list(
      #xaxis = empty_axis,
      #yaxis = empty_axis,
      #zaxis = empty_axis,
      aspectratio = list(x = 1, y = 1, z = 1),
      camera = list(eye=list(x=2,y=0,z=0.6))
    )
  )

globe

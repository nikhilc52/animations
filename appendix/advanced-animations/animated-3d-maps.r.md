---
description: Code used to make the graph found within "Animated 3D Maps"
---

# animated-3d-maps.R

<figure><img src="../../.gitbook/assets/final_optimized.gif" alt="" width="563"><figcaption></figcaption></figure>

```r
library(plotly)
library(dplyr)
library(stars)
library(tidyr)
library(lubridate)

degrees2radians <- function(degree) degree * pi / 180 

shearwaters <- read.csv("occurrence.csv")

shearwaters <- shearwaters[,names(shearwaters) %in% c("organismID", "verbatimEventDate", "decimalLatitude","decimalLongitude")]

shearwaters <- shearwaters |> 
  separate_rows(organismID, sep=";") |> 
  mutate(date = as.POSIXct(verbatimEventDate))

shearwaters <- shearwaters |> 
  mutate(x=1.01 * cos(degrees2radians(decimalLongitude)) * cos(degrees2radians(decimalLatitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(decimalLongitude)) * cos(degrees2radians(decimalLatitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(decimalLatitude)))

time_range <- seq(
  from = as.Date(min(shearwaters$date)),
  to = as.Date(max(shearwaters$date)),
  by = "day"
)

animals <- unique(shearwaters$organismID)
full_times <- expand.grid(Animal = animals, Time = time_range)

shearwaters_full <- full_times |>
  left_join(shearwaters, by = c("Animal" = "organismID"))|> 
  group_by(Animal, Time) |> 
  filter(abs(difftime(Time, date, units = "secs")) == min(abs(difftime(Time, date, units = "secs")))) |>
  filter(abs(difftime(Time, date, units = "days")) < 1) |> 
  ungroup() |>
  select(Animal, Time, date, decimalLatitude, decimalLongitude, x, y, z) |> 
  group_by(Time) |> 
  filter(n() > 3) |> 
  filter(Time >= as.Date("2010-01-01"))

shearwaters_full$Time <- as.character(shearwaters_full$Time)

##########
x_size <- 1000
y_size <- 500

empty_axis <- list(
  showgrid = FALSE, 
  zeroline = FALSE,
  showticklabels = FALSE,
  showspikes = FALSE,
  spikesides = FALSE,
  title = ""
)

lat <- seq(-90, 90, length.out = y_size)
lon <- seq(-180, 180, length.out = x_size)
lat <- matrix(rep(lat, x_size), nrow = y_size)
lon <- matrix(rep(lon, each = y_size), nrow = y_size)

average_positions <- shearwaters_full |> 
  group_by(Time) |> 
  summarize(
    average_x = mean(x),
    average_y = mean(y),
    average_z = mean(z)
  )

spline_fit_x <- smooth.spline(average_positions$average_x, spar = .8)
spline_fit_y <- smooth.spline(average_positions$average_y, spar = .8)
spline_fit_z <- smooth.spline(average_positions$average_z, spar = .8)

camera_positions <- data.frame(x = predict(spline_fit_x, x=seq(1,nrow(average_positions), by=nrow(average_positions)/(30*15)))$y)
camera_positions <- camera_positions |> 
  mutate(y = predict(spline_fit_y, x=seq(1,nrow(average_positions), by=nrow(average_positions)/(30*15)))$y) |> 
  mutate(z = predict(spline_fit_z, x=seq(1,nrow(average_positions), by=nrow(average_positions)/(30*15)))$y)

###########
generate_surface <- function(file){
  raw_tif <- read_stars(paste0("surfaces/", file, ".png"), RasterIO = list(nBufXSize=x_size, nBufYSize=y_size))
  
  rgb <- as.data.frame(raw_tif) |> 
    tidyr::pivot_wider(names_from = band, values_from = paste0("X",file,".png"))
  
  colnames(rgb)[3] <- "red"
  colnames(rgb)[4] <- "green"
  colnames(rgb)[5] <- "blue"
  
  rgb$color <- rgb(rgb$red/255,rgb$green/255,rgb$blue/255)
  rgb$color_int <- bitwShiftL(rgb$red, 16) + bitwShiftL(rgb$green, 8) + rgb$blue 
  
  rgb_earth <<- matrix(data=rgb$color_int, nrow=y_size, ncol = x_size, byrow=TRUE)
  
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
  earth_colorscale <<- earth_colorscale
}

generate_globe <- function(curr_shearwaters, camera_index, dates_index){
  globe <<- curr_shearwaters |>  
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
      x=~curr_shearwaters$x,
      y=~curr_shearwaters$y,
      z=~curr_shearwaters$z,
      mode = "markers", type = "scatter3d",
      marker = list(size = 7),
      hoverinfo = "none"
    ) |> 
    add_surface(
      x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
      y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
      z = -sin(degrees2radians(lat)),
      surfacecolor = rgb_earth,
      colorscale=earth_colorscale,
      showscale = FALSE, hoverinfo = "none",
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
      showlegend = FALSE,
      annotations=list(list(text=paste0("Day: ", dates[dates_index], "<br>Nikhil Chinchalkar For Princeton University | Migration and foraging ecology of Greater Shearwater | 2024"),
                            showarrow=FALSE, font=list(family="Arial", size=28), y=0, bgcolor="white", opacity=0.85), 
                       list(text="<br><b>Greater Shearwater Migration</b>",font=list(family="Arial", size=48), bgcolor="white", opacity=0.85, y=.9)),
      scene = list(
        xaxis = empty_axis,
        yaxis = empty_axis,
        zaxis = empty_axis,
        aspectratio = list(x = 1, y = 1, z = 1),
        camera = list(eye=list(x=camera_positions$x[camera_index]*2,
                               y=camera_positions$y[camera_index],
                               z=camera_positions$z[camera_index]))
      )
    )
}

#install.packages("processx")
dates <- unique(shearwaters_full$Time)
system.time(
  for(i in 1:nrow(camera_positions)){
    dates_index <- as.integer((i+3) * length(dates)/nrow(camera_positions))
    print(paste("Date:",dates[dates_index]))
    print(paste("Frame #:", i))
    
    file = formatC(yday(as.Date(dates[dates_index])), width = 3, format = "d", flag = "0")
    generate_surface(file)
    
    curr_shearwaters <- shearwaters_full |> 
      filter(Time == dates[dates_index])
    
    generate_globe(curr_shearwaters, i, dates_index)
    
    orca(globe, paste0("image_sequence/",formatC(i, width = 3, format = "d", flag = "0"),".png"), width = 7*200, height = 7*200)
  }
)

png_files <- sort(list.files("image_sequence", pattern = "*.png", full.names = TRUE))
for(i in 1:120){
  png_files <- append(png_files, png_files[length(png_files)])
}

gifski::gifski(png_files, gif_file = "final.gif", width = 7*200, height = 7*200, delay = 30/length(png_files))

```

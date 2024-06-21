library(plotly)
library(dplyr)
library(stars)
library(tidyr)
library(lubridate)

degrees2radians <- function(degree) degree * pi / 180 

shearwaters <- read.csv("occurrence.csv")

shearwaters <- shearwaters[,names(shearwaters) %in% c("organismID", "verbatimEventDate", "decimalLatitude","decimalLongitude")]

shearwaters <- shearwaters |> 
  mutate(x=1.01 * cos(degrees2radians(decimalLongitude)) * cos(degrees2radians(decimalLatitude))) |> 
  mutate(y=1.01 * sin(degrees2radians(decimalLongitude)) * cos(degrees2radians(decimalLatitude))) |> 
  mutate(z=1.01 * sin(degrees2radians(decimalLatitude)))

shearwaters <- shearwaters |> 
  separate_rows(organismID, sep=";") |> 
  mutate(date = as.POSIXct(verbatimEventDate))

time_range <- seq(
  from = as.Date(min(shearwaters$date)),
  to = as.Date(max(shearwaters$date)),
  by = "day"
)

animals <- unique(shearwaters$organismID)
full_times <- expand.grid(Animal = animals, Time = time_range)

shearwaters_full <- full_times |>
  left_join(shearwaters, by = c("Animal" = "organismID"))  |> 
  group_by(Animal, Time) |> 
  filter(abs(difftime(Time, date, units = "secs")) == min(abs(difftime(Time, date, units = "secs")))) |>
  filter(abs(difftime(Time, date, units = "days")) < 1) |> 
  ungroup() |>
  arrange(Time, Animal) |> 
  select(Animal, Time, date, decimalLatitude, decimalLongitude, x, y, z) |> 
  group_by(Time) |> 
  filter(n() > 5) |> 
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

nlat <- y_size
nlon <- x_size
lat <- seq(-90, 90, length.out = nlat)
lon <- seq(-180, 180, length.out = nlon)
lat <- matrix(rep(lat, nlon), nrow = nlat)
lon <- matrix(rep(lon, each = nlat), nrow = nlat)

average_positions <- shearwaters_full |> 
  group_by(Time) |> 
  summarize(
    average_x = mean(x),
    average_y = mean(y),
    average_z = mean(z)
  )


spline_fit <- smooth.spline(average_positions$average_x, spar = .8)
camera_x_positions <- predict(spline_fit)$y
spline_fit <- smooth.spline(average_positions$average_y, spar = .8)
camera_y_positions <- predict(spline_fit)$y
spline_fit <- smooth.spline(average_positions$average_z, spar = .8)
camera_z_positions <- predict(spline_fit)$y

###########
dates <- unique(shearwaters_full$Time)
image_index <- 1
system.time(
  for(i in 1:length(dates)){
    file = formatC(as.Date(dates[i])-as.Date("2010-01-01")+1, width = 3, format = "d", flag = "0")
    raw_tif <- read_stars(paste0("surfaces/", file, ".png"), RasterIO = list(nBufXSize=x_size, nBufYSize=y_size))
    
    df_tif <- as.data.frame(raw_tif)
    df_tif <- df_tif |>
      mutate(x = x-180) |>
      mutate(y = y-90)
    
    red <- df_tif |> 
      filter(band == 1)
    names(red)[names(red) == paste0("X",file,".png")] <- 'red'
    red <- red[-3]
    
    green <- df_tif |> 
      filter(band == 2)
    names(green)[names(green) == paste0("X",file,".png")] <- 'green'
    green <- green[-3]
    
    blue <- df_tif |> 
      filter(band == 3)
    names(blue)[names(blue) == paste0("X",file,".png")] <- 'blue'
    blue <- blue[-3]
    
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
    
    print(paste("Date:",dates[i]))
    print(paste("Frame #:", image_index))
    
    curr_shearwaters <- shearwaters_full |> 
      filter(Time == dates[i])
    
    globe <- curr_shearwaters |>  
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
        data=shearwaters_full,
        x=~curr_shearwaters$x,
        y=~curr_shearwaters$y,
        z=~curr_shearwaters$z,
        mode = "markers", type = "scatter3d",
        marker = list(size = 7),
        text = paste("ID: ", curr_shearwaters$Animal, "<br>"),
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
          x=2,
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
        showlegend = FALSE,
        annotations=list(list(text=paste0("Day: ", dates[i] ,"<br>Nikhil Chinchalkar For Princeton University | LifeWatch INBO | 2024"),
                         showarrow=FALSE, font=list(family="Arial", size=28), y=0, bgcolor="white", opacity=0.85), 
                         list(text="<br><b>Greater Shearwater Migration</b>",font=list(family="Arial", size=48), bgcolor="white", opacity=0.85, y=.9)),
        scene = list(
          xaxis = empty_axis,
          yaxis = empty_axis,
          zaxis = empty_axis,
          aspectratio = list(x = 1, y = 1, z = 1),
          camera = list(eye=list(x=camera_x_positions[i]*2,y=camera_y_positions[i],z=camera_z_positions[i]))
        )
      )
    orca(globe, paste0("image_sequence/",formatC(image_index, width = 2, format = "d", flag = "0"),".png"), width = 7*200, height = 7*200)
    image_index <- image_index + 1
  }
)

png_files <- sort(list.files("image_sequence", pattern = ".*png$", full.names = TRUE))
for(i in 1:10){
  png_files <- append(png_files, png_files[length(png_files)])
}
gifski::gifski(png_files, gif_file = "final.gif", width = 7*200, height = 7*200, delay = .4)




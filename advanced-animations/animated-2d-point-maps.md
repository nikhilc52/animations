---
cover: ../.gitbook/assets/final (6).gif
coverY: 0
---

# Animated 2D Point Maps

Looking at flight data can provide insights into the busiest times for air traffic as well as highlighting the times where the sky is empty.&#x20;

In this module, we'll be making this animation:

<figure><img src="../.gitbook/assets/final (18).gif" alt="" width="563"><figcaption></figcaption></figure>

The data is available [here](https://opensky-network.org/datasets/states/2022-06-27/). Download all the folders into one folder, then click on the .tar file to generate the csv for each hour.

***

Let's load in the libraries:

```r
library(tidyverse) #tidying the data
library(dplyr) #wrangling the data
library(gganimate) #animating the plot
library(ggplot2) #generating the plot
library(sf) #working with map objects
```

Since the data is a bit scattered, our first step is organizing it into a single data frame. We can do this with a simple function.

```r
read_hourly_data <- function(hour) {
  hour_log <- read_csv(sprintf("flight_hours/states_2022-06-27-%02d.csv/states_2022-06-27-%02d.csv", hour, hour),
                       show_col_types = FALSE) 
  hour_log$time <- as_datetime(hour_log$time)
  hour_log$time <- floor_date(hour_log$time, "hour")
  hour_log <-  hour_log |> 
    filter(!onground) |> 
    filter(!is.na(lat)) |> 
    filter(!is.na(lon)) |> 
    filter(!is.na(velocity)) |> 
    group_by(icao24) |> 
    arrange(desc(icao24)) |>
    slice_head(n=1)
}
```

Since the data is divided into hours, and each file has a label that corresponds to the hour of data its carrying, we can write a simple sprintf call to input an integer representing an hour and obtain all the data for that hour.&#x20;

While the syntax might look a bit daunting, all the first line in the function is doing is reading the data at a location given by two hour numbers that are always 2 digits (with leading zeros if needed).&#x20;

Now that our data is loaded in for the hour, we can move on to conversions, to get things done all at once. We'll start by converting the time given for all the data in each hour to a readable time for R, using `as_datetime`.&#x20;

Next, we'll take the floor of every time present within the dataset. The original data has a record of every flight every 10 seconds, which is very powerful data, but a bit too much for our plotting methods within R. So, we're decreasing things 60-fold, by only choosing one data point for every hour.&#x20;

Once the time column is representing data to an hour precision, we then filter so that all the characteristics we care about are present. Next, we group by `icao24`, or each plane's 'serial number', arrange the column in descending order so it's easier to view, and choose the first (arbitrary) logged location for the plane, so that for every hour, we have exactly one location for all planes that were flying at some point during that time.

All together, this function reads in an hour, and gives a formatted data frame with a single location for all the planes logged during that hour, which will decrease our plotting time drastically, without sacrificing too much detail.

With our function in place, we can run it over a 24-hour iteration, to bind together a large dataset with hourly locations for all planes during the 24-hour window.

```r
hourly_flights <- read_hourly_data(0)
for(x in 1:23){
  hourly_flights <- rbind(hourly_flights, read_hourly_data(x))
  print(paste("Read and appended:",x))
}
```

We're printing here to track our progress, since this function will take about three minutes to run, so it's important we know exactly how far along we are on that process (and if there's an error). Once that's done, we'll get a data frame that looks like this:

<figure><img src="../.gitbook/assets/image (14) (1) (1).png" alt=""><figcaption></figcaption></figure>

Even though we shortened our focus down to just one log every hour, the dataset is still very large: nearly 300,000 rows. To focus down even more, we can focus on just American Airlines flights, which have a call sign that starts with "AAL":

```r
hourly_flights_AAL <- hourly_flights |> 
  filter(startsWith(callsign, "AAL"))
```

<figure><img src="../.gitbook/assets/image (15) (1) (1).png" alt=""><figcaption></figcaption></figure>

There's two parts to each plane we're showing on our plot: one is the location of the aircraft itself, and the other is a tail leading out from the aircraft in the opposite direction it is heading, to show direction.

The former of these two parts is easy to implement:

```r
hourly_flights_AAL_point <- hourly_flights_AAL |> 
  st_as_sf(coords=c("lon","lat")) |> 
  st_set_crs(4326)
```

We just convert the data frame's latitude and longitude points into a sf object using `st_as_sf`, and set the Coordinate Reference System to the standard 4326.

The latter part of each aircraft is a bit more complicated to create:

```r
hourly_flights_AAL_line <- hourly_flights_AAL |> 
  mutate(lon2 = lon - velocity*cos((90-heading) * pi/180)/100) |> 
  mutate(lat2 = lat - velocity*sin((90-heading) * pi/180)/100) |> 
  mutate(geom = sprintf("LINESTRING(%f %f, %f %f)", lon, lat, lon2, lat2)) |> 
  st_as_sf(wkt = "geom") |> 
  st_set_crs(4326)
```

Essentially, we're just adding a longitude and latitude pair to act as an end point for the line we're making to represent the trail. The longitude and latitude are created using some trigonometry:

<figure><img src="../.gitbook/assets/rough1 (1).svg" alt=""><figcaption></figcaption></figure>

Since the heading is measure as the distance from the vertical line (clock-wise), we manipulate it as described in the image above to align it within our triangle. From here, it's simple to see how our formula is generated. Since cosine and sine are in radians, we multiply the `90-heading` by `pi/180` to convert it. Then, we're dividing the result by 100 to scale it down, otherwise the tails would be too long, and subtracting it from the original lon and lat points to put our tail behind the point.

With these newly created columns, we can convert our two points into a linestring using the `sprintf` and `st_as_sf` functions. The `sprintf` call makes a linestring character string with each of our floating-point coordinates (referenced by a `%f`). Then, we indicate to R that we want to convert the data held within the "geom" column to an sf object. As always, we then set our Coordinate Reference System to the standard 4326.

With all our data in place, we'll now get the data for the plot of the U.S. underneath the planes, with a standard call to the maps package, then converting that call to a plottable `sf` object.

```r
usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) #requires 'maps' package is installed
```

<figure><img src="../.gitbook/assets/image (16) (1).png" alt=""><figcaption></figcaption></figure>

We're now ready to plot everything. Since this plot takes some time to load, I'll just jump to the final iteration of our plot.

```r
animation <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=hourly_flights_AAL_point, size=1, alpha=0.6, aes(group = icao24))+
  geom_sf(data=hourly_flights_AAL_line, size=1, alpha=0.6, aes(group = icao24))+
  theme_void()+
  labs(title="American Airlines Flights Over A 24-hour Period", 
       subtitle="{format(frame_time, \"%I:%M %p\")} EST")+
  ylim(25,50)+
  xlim(-135,-65)+
  xlab('')+
  ylab('')+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 16, hjust =0.5, face = "bold"))+
  transition_time(time)+
  enter_fade(alpha = 0)+
  exit_fade(alpha = 0)
```

The `geom_sf` calls are all fairly standard: remember that we're including the group attribute to indicate we want each plane to animate over its own locations. The alpha isn't equal to one so that we can see the underlying map.&#x20;

We'll use `theme_void` to get rid of the background axes and format our `frame_time` variable to the XX:XX AM/PM format, using `%I:%M %p`. Next, we'll limit our plot to only display the continental U.S., with limits at the vertical and horizontal end points of the country. Since we're not displaying the axes, we're also not going to label the X and Y axes. After we format the title and subtitle, we call `transition_time` to indicate we're cycling through different times continuously. Finally, we're calling `enter_fade` and `exit_fade` with alphas of 0 to make planes appear and disappear in a more smooth fashion (planes will fade in and out).

We can now animate our animation.&#x20;

```r
animate(animation, fps=15, duration=48, end_pause = 75, height = 7,
        width = 9, units = "in", res = 200)
```

After a few minutes, we'll get our finished animation.

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/advanced-animations/animated-2d-point-maps.r.md)
{% endhint %}

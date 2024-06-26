---
description: >-
  Maps present the opportunity for immensely valuable data visualizations.
  Animations can help to elevate these visuals.
cover: ../.gitbook/assets/final (5) (1).gif
coverY: 0
---

# Animated Maps

For all of the basic animations, we'll use the `txhousing` dataset within the `ggplot2` library, for simplicity. This dataset contains "information about the housing market in Texas provided by the [TAMU real estate center](https://trerc.tamu.edu/)".&#x20;

Our goal is to reproduce this plot:

<figure><img src="../.gitbook/assets/final (1) (1) (1) (1) (1) (1) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

***

Like always, we start by importing libraries. This time, there's two new ones.

```r
library(ggplot2) #plotting and getting the txhousing dataset
library(dplyr) #sorting the data to fit our graph
library(gganimate) #animating the ggplot
library(sf) #sf = shapefile, specifically used to plot maps
library(tidygeocoder) #used to get latitude and longitude points from locations
```

We'll start by getting the backdrop for our plot.

```r
usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) #requires 'maps' package is installed
```

This line is farily complicated. The `maps::map()` call uses the maps package to draw a geographical map. The `state` parameter means that we are drawing the continental U.S. with states as polygons. `Fill=TRUE` indicates that we'd like the polygons to (eventually) have colors. `Plot=FALSE` just means we won't plot the map right now.

`maps::map()` returns a map object, which must be turned into an `sf` if we want to plot it within `ggplot`. To do this we call `st_as_sf()` to turn the map (`st` : spacial type) into an `sf` (simple features). This object is then stored in the variable `usa`.

<figure><img src="../.gitbook/assets/image (4) (1) (1) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Going back to the basics, we read txhousing into a new data frame for viewing.

```r
txhousing_data <- txhousing
```

<figure><img src="../.gitbook/assets/Screenshot 2024-06-12 at 3.11.53 PM.png" alt=""><figcaption></figcaption></figure>

To plot the data on a map, we'll need each city to correspond to a point with an appropriate latitude and longitude. Let's prepare to do this my making a new data frame exclusively for cities, that we can then merge back with `txhousing_data`. In our new dataset, to save time geocoding, each city should only be featured once:

```r
cities <- data.frame(unique(txhousing_data$city)) #df of unique cities
names(cities)[names(cities) == 'unique.txhousing_data.city.'] <- 'city' #rename column name for simplicitydataframe
```

<figure><img src="../.gitbook/assets/image (1) (1) (1) (1) (1) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Essentially, we're taking all the unique city names and making them a data frame, then renaming the column name to save some typing later on.

We're now going to take each of the cities, and find both the latitude and longitude using the `geo_osm` function in the `tidygeocoder` package. The `geo_osm` function returns a tibble with the name, latitude, and longitude, so taking just one attribute and putting it into the corresponding `cities` column is simple. I've concatenated ", Texas" to the end of the cities to make it easier for the function to identify certain ambiguous names (i.e. "Paris", which is a city in both Texas and France).

```r
cities$lat <- geo_osm(paste0(cities$city, ", Texas"))$lat #take each city's lat and put it into a lat column
cities$long <- geo_osm(paste0(cities$city, ", Texas"))$long #take each city's long and put it into a long column
```

<figure><img src="../.gitbook/assets/Screenshot 2024-06-13 at 4.31.24 PM.png" alt=""><figcaption></figcaption></figure>

Let's join this data frame back with the original.

```r
txhousing_data <- left_join(txhousing_data, cities)
```

<figure><img src="../.gitbook/assets/image (3) (1) (1) (1) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Now that we have the latitude and longitude points for each city in our original data frame, we can convert these points to geometry in R.

```r
sf_txhousing_data <- txhousing_data |> 
  filter(!is.na(long)) |> #NAs are not allowed to be converted
  filter(!is.na(lat)) |> #NAs are not allowed to be converted
  st_as_sf(coords = c('long','lat')) |> #convert the 'lat' and 'long' coordinates to points
  st_set_crs(4326) #set a standard coordinate system
```

For each coordinate pair, we use the `st_as_sf` function to convert the "regular" lat and long numbers into actual geometric points. Then, to make sure alignment is proper, we set a Coordinate Reference System using st\_set\_crs, since there are a number of ways to align latitude and longitude along a 2D plane. For most situations, 4326 (World Geodetic System) is your go-to.

<figure><img src="../.gitbook/assets/image (4) (1) (1) (1) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Now that we've got all our data in order, we can start to plot. We're now using `geom_sf()` to plot "simple features" using `ggplot`. We have a few specifications: the `color` of the inner circle should be the median sale price, and both the inner and outer circle should have `size`s that reflect the number of listings/sales for that city. Obviously, there are always at least as many listings as there are sales, so listings will be the outer circle and sales the inner.

Note that the shape parameter in the first `geom_sf` just means the circle will be unfilled (the outline will be gray).

```r
ggplot()+
  geom_sf(data=usa)+
  geom_sf(data=sf_txhousing_data, aes(size=listings), shape=1, color="gray")+
  geom_sf(data=sf_txhousing_data, aes(size=sales, color=median))
```

<figure><img src="../.gitbook/assets/rough1 (2) (1).png" alt="" width="563"><figcaption></figcaption></figure>

We're almost done. Let's clean up this chart a little by fixing the scales and adding titles.

```
ggplot()+
  geom_sf(data=usa)+
  geom_sf(data=sf_txhousing_data, aes(size=listings), shape=1, color="gray")+
  geom_sf(data=sf_txhousing_data, aes(size=sales, color=median))+
  viridis::scale_color_viridis(option="B", label=scales::comma)+ #requires the 'viridis', 'scales' packages are installed
  scale_size_continuous(label=scales::comma, range = c(1,15))+
  theme_void()+
  labs(title="Texas Housing Data from 2000-2015",
       subtitle="The outer circle is how many houses were listed.<br>The inner circle is how many houses were sold and for what price.",
       color="Median Sale Price", 
       size="Number of Listings/Sales")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold")) #requires the 'ggtext' package is installed
```

<figure><img src="../.gitbook/assets/rough2 (2).png" alt="" width="563"><figcaption></figcaption></figure>

Having all of America here isn't really necessary as all our data is within Texas, so we can cut off the coordinates of our graph with a self-explanatory `coord_sf` call.

```r
+coord_sf(xlim = c(-107, -90), ylim = c(25, 37))
```

Here, the X (longitude) goes from -107 to -90 (or 107 W to 90 W) and Y (latitude) goes from 25 to 37 (or 25 N to 37 N).&#x20;

Finally, we can add `transition_time(date)` as usual, to tell R that each frame we see are cycling/seeing data points from different points of time, as specified by the `date` (and smoothing the in-between areas). We'll also add the same `{as.integer(frame_time)}` as before, to update the viewer on the current data they're seeing.

```r
ggplot()+
  geom_sf(data=usa)+
  geom_sf(data=sf_txhousing_data, aes(size=listings), shape=1, color="gray")+
  geom_sf(data=sf_txhousing_data, aes(size=sales, color=median))+
  viridis::scale_color_viridis(option="B", label=scales::comma)+
  scale_size_continuous(label=scales::comma, range = c(1,15))+
  theme_void()+
  labs(title="Texas Housing Data from 2000-2015",
       subtitle="The outer circle is how many houses were listed.<br>The inner circle is how many houses were sold and for what price.
       <br><br>Year: {as.integer(frame_time)}<br>", #changed line
       color="Median Sale Price", 
       size="Number of Listings/Sales")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  coord_sf(xlim = c(-107, -90), ylim = c(25, 37))+ #added line
  transition_time(date) #added line
```

We're done! Now we can just give our animation to an object and animate the object using the animate function, with some appropriate parameters.

```r
animation <- ggplot()+
    #continued from above
```

```r
animate(animation, fps=10, duration=15, end_pause=30, height = 8,
        width = 9, units = "in", res = 200)
```

Congrats, you've made it through all the basic animations, and are ready to tackle the advanced ones.

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/basic-animations/animated-maps.r.md)
{% endhint %}

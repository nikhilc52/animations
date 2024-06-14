---
description: >-
  Line graphs can be very useful indicators of data. Animating them draws your
  viewer in and grabs their attention.
cover: ../.gitbook/assets/final (2) (1).gif
coverY: 0
---

# Animated Line Graphs

For all of the basic animations, we'll use the `txhousing` dataset within the `ggplot2` library, for simplicity. This dataset contains "information about the housing market in Texas provided by the [TAMU real estate center](https://trerc.tamu.edu/)".&#x20;

In a few minutes, we'll make this graph:

<figure><img src="../.gitbook/assets/final (1) (1) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

***

To get started, let's load all the libraries we'll need:

```
library(ggplot2) #plotting and getting the txhousing dataset
library(dplyr) #sorting the data to fit our graph
library(gganimate) #animating the ggplot
```

Now that we have the libraries, we can get the dataset. So that we can easily view it, let's put it into a variable.

```r
txhousing_data <- txhousing
```

<figure><img src="../.gitbook/assets/image (3) (1).png" alt=""><figcaption><p>The first five rows of the txhousing_data dataframe.</p></figcaption></figure>

We want to have a single number for sales and listings for each month and year. Luckily, the dataset already comes with a `date` column that specifies both the month and the year, so we can just group based on `date` and sum the sales and listings for every city to get a total.

```r
txhousing_data <- txhousing_data |> 
  group_by(date) |> 
  filter(!is.na(sales)) |> #any NAs make the 'sum' = 0
  filter(!is.na(listings)) |> #any NAs make the 'sum' = 0
  summarise(
    sales = sum(sales),
    listings = sum(listings)
  )
```

Let's look at the data now.

<figure><img src="../.gitbook/assets/image (5).png" alt=""><figcaption><p>The first five rows of the updated txhousing_data dataframe.</p></figcaption></figure>

Now that the data is in the right format, we're ready to plot. We'll start by first generating a static line graph, with the date on the x-axis and sales/listings on the y-axis (in two separate lines).

```r
ggplot(txhousing_data)+
  geom_line(aes(x=date, y=sales, color="Sales"))+
  geom_line(aes(x=date, y=listings, color="Listings"))
```

<figure><img src="../.gitbook/assets/rough1 (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

This doesn't look too pretty, so let's add a theme, some nicer colors, and fix the labels.

```r
ggplot(txhousing_data)+
  geom_line(aes(x=date, y=sales, color="Sales"))+
  geom_line(aes(x=date, y=listings, color="Listings"))+
  scale_color_manual(breaks = c("Sales", "Listings"), values=c("#1f948b","#482071"))+
  scale_y_continuous(label=scales::comma)+ #requires 'scales' has been installed
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House sales are cyclical and somewhat consistent, listings are more volatile.",
       color="",
       x="Year",
       y="Number of Sales/Listings")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"),  #requires the ggtext package is installed
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold")) #requires the ggtext package is installed
```

<figure><img src="../.gitbook/assets/rough2 (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

We're almost there. To convert this into an animation we have to add `transition_reveal(date)` to the end of our code. `transition_reveal` is somewhat self-explainatory:  we want to reveal more and more of the line as animation goes on. Here, `date` is what is actually moving the animation. Essentially, we're telling the computer to reveal more and more of each line as the `date` increases.

<pre class="language-r"><code class="lang-r">ggplot(txhousing_data)+
  geom_line(aes(x=date, y=sales, color="Sales"))+
  geom_line(aes(x=date, y=listings, color="Listings"))+
  scale_color_manual(breaks = c("Sales", "Listings"), values=c("#1f948b","#482071"))+
  scale_y_continuous(label=scales::comma)+
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House sales are cyclical and somewhat consistent, listings are more volatile.",
<strong>       color="",
</strong>       x="Year",
       y="Number of Sales/Listings")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"),  #requires the 'ggtext' package is installed
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+ #requires the 'ggtext' package is installed  
  transition_reveal(date) #new line added
</code></pre>

<figure><img src="../.gitbook/assets/rough3 (1).gif" alt=""><figcaption></figcaption></figure>

The animation is now in place, but the text is cut off and the resolution isn't ideal. To fix this we put our animation into an object using the pipe operation (`<-`)  and then call `animate()` on that object, with some self-explainatory parameters.

```r
animation <- ggplot(txhousing_data)+ #added the pipe into 'animation'
  geom_line(aes(x=date, y=sales, color="Sales"))+
  geom_line(aes(x=date, y=listings, color="Listings"))+
  #continues from above code snippet
```

```r
animate(animation, fps=10, duration=15, end_pause=9, width = 9, height = 8,
         units = "in", res = 200)
```

We're animating our animation, with 10 frames per second for 15 seconds. The animation's last 9 frames will pause before it wraps back around (`end_pause`). The width and height are 9 and 8 inches each and the resolution (`res`) or how many pixels will be in each inch, is 200 (i.e. the final animation will be 1800x1600 pixels).&#x20;

When you run the animation() function, it should automatically pop up in the "Viewer" tab in R. From there, you can open it in a new window and save it as a GIF from there. However, if you want to easily save it to your local directory within R, simple change some syntax:

```r
animation <- animate(animation, fps=10, duration=15, end_pause=9, height = 8,
        width = 9, units = "in", res = 200)
```

```r
anim_save('animation.gif', animation)
```

Essentially, we're giving the animation its own specifications for animating, so that it can export seamlessly.

That's it! You've now successfully made an animated line chart.&#x20;

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/animated-line-graph.r.md)
{% endhint %}

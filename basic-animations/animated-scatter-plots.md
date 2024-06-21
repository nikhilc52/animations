---
description: >-
  Scatter plots are useful for conveying information about a number of points.
  By animating, we can add a new dimension to the graph.
cover: ../.gitbook/assets/final (3) (1) (1).gif
coverY: 0
---

# Animated Scatter Plots

For all of the basic animations, we'll use the `txhousing` dataset within the `ggplot2` library, for simplicity. This dataset contains "information about the housing market in Texas provided by the [TAMU real estate center](https://trerc.tamu.edu/)".&#x20;

At the end of this module, we'll have made this graph:

<figure><img src="../.gitbook/assets/final (6).gif" alt="" width="563"><figcaption></figcaption></figure>

***

As we did in the previous section, we'll load all the libraries.

```r
library(ggplot2) #plotting and getting the txhousing dataset
library(dplyr) #sorting the data to fit our graph
library(gganimate) #animating the ggplot
```

Again, we'll put the dataset into a data frame so that it's easier to see changes.

```r
txhousing_data <- txhousing
```

For this plot, we need to have a data point (dot) for each city for all time frames. This point needs to have both the median sale price and the sales. Lucky for us, the dataset is already in this format:

<figure><img src="../.gitbook/assets/image (2) (1) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

We can go straight to plotting. The syntax is very similar to the line graph, but note that we now use `geom_point` instead of `geom_line`. We're also going to add a new parameter: group. All this does is tell R that we'd like each point to represent one city

```r
ggplot(txhousing_data)+
  geom_point(aes(x=sales, y=median))
```

<figure><img src="../.gitbook/assets/rough1 (1) (1) (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

That's a lot of data, but for now, let's just focus on the general patterns. There's somewhat of a logarithmic curve to the data, so we'll apply a logarithmic scale to the x-axis. Let's also get rid of the scientific notation on the y-axis by adding commas.

```r
ggplot(txhousing_data)+
  geom_point(aes(x=sales, y=median))+
  scale_x_continuous(label=scales::comma, transform = "log10")+ #requires 'scales' package is installed
  scale_y_continuous(label=scales::comma) #requires 'scales' package is installed
```

<figure><img src="../.gitbook/assets/rough2 (1) (1) (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

Once we've done that, we can make the graph look a bit nicer before we get into the animation stages. `<br>` is added for spacing.

```r
ggplot(txhousing_data)+
  geom_point(aes(x=sales, y=median))+
  scale_x_continuous(label=scales::comma, transform = "log10")+
  scale_y_continuous(label=scales::comma)+
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House prices and sales for each dot/city have increased since 2000, <br>especially post-2008.",
       x="Sales",
       y="Median Sale Price")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))
```

<figure><img src="../.gitbook/assets/rough3 (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

We're now ready to animate. The graph might look like a mess right now, but remember that each point corresponds to a city and a month. Our goal for this animation is to show all the points for a given month, cycling through the months.&#x20;

This can easily be accomplished with `transition_time`, which does exactly this: cycle through a time parameter, showing the data at each point in time (smoothing through transition periods). Our time parameter in this case is date, so we call `transition_time(date)`.

There's one more thing we have to do before we plot, which is adding a `group` parameter to the `geom_point` call. This step is very important (and often forgotten) - we're essentially telling R that we'd like to have the points grouped based on an aesthetic. In this case, we want each point to represent a city, so we use `group=city`:

Lastly, it would be nice if the date was displayed somewhere on the graph. To do so, let's modify the subtitle parameter with the keyword `{frame_time}` which gives the current value of the variable we're animating over (`date` in this case). For simplicity, let's just round down `date` (which has a decimal value for months) with the `as.integer` function, to show each year rather than each year/month.&#x20;

```r
ggplot(txhousing_data)+
  geom_point(aes(x=sales, y=median, group=city))+ #added 'group=city'
  scale_x_continuous(label=scales::comma, transform = "log10")+
  scale_y_continuous(label=scales::comma)+
  theme_minimal()+
  labs(title="Texas Housing Market from 2000-2015",
       subtitle="House prices and sales for each dot/city have increased since 2000, <br>especially post-2008. <br><br>Year: {as.integer(frame_time)}", #changed line
       caption="Nikhil",
       x="Sales",
       y="Median Sale Price")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(date) #added line
```

Our animation is ready to go. To finalize its dimensions, frames, and resolution, let's put it into an object and animate it.

```r
animation <- ggplot(txhousing_data)+
    #continues from above code snippet
```

```r
animate(animation, fps=30, duration=60, end_pause=150, height = 7,
        width = 9, units = "in", res = 200)
```

Note that the increase in FPS is just to make the animation a bit smoother. Anything more than 10 fps should be good to see a smooth animation.&#x20;

Congrats! You've just made an animated scatter plot.

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/basic-animations/animated-scatter-plot.r.md)
{% endhint %}

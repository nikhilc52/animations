---
description: >-
  Word clouds can help viewers easily identify data points. Animating can help
  to show trends over time.
cover: ../.gitbook/assets/final (4) (1) (1).gif
coverY: 0
---

# Animated Word Clouds

For all of the basic animations, we'll use the `txhousing` dataset within the `ggplot2` library, for simplicity. This dataset contains "information about the housing market in Texas provided by the [TAMU real estate center](https://trerc.tamu.edu/)".&#x20;

We're trying to make this graph:

<figure><img src="../.gitbook/assets/final (4) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

***

Let's start off like always, loading the libraries we'll be using to make the word cloud. Note that we've now added `ggwordcloud` to our imports.

```r
library(ggplot2) #plotting and getting the txhousing dataset
library(dplyr) #sorting the data to fit our graph
library(gganimate) #animating the ggplot
library(ggwordcloud) #fitting the data into a word cloud
```

With our libraries in place, we get the dataset in the same way we've been doing.

```r
txhousing_data <- txhousing
```

<figure><img src="../.gitbook/assets/image (3) (1) (1) (1).png" alt=""><figcaption><p>The first five rows of the txhousing_data dataframe.</p></figcaption></figure>

Now that we have our data, we need to format it to make a suitable word cloud. We'll take the `volume` column, which is the number of house sales multiplied by their price (i.e. how much money was spent), and use it as an indicator for size.&#x20;

Our data is already formatted in this manner, but there's too many data points/cities. If we were to plot a word cloud with the data set as is, it would look like this:

<figure><img src="../.gitbook/assets/rough1 (1) (1).gif" alt=""><figcaption></figcaption></figure>

There's just too much going on for the viewer to properly understand - we need to cut down how many cities we're showing per frame. Let's only show the top five cities by housing market volume, for every date. We can format our dataset to this specification by using the `slice_head` function:

```r
txhousing_data <- txhousing_data |> 
  group_by(date) |> #group by date: for every date, find the top 5
  arrange(desc(volume)) |> #arrange in manner such that the top 5 are at the top/head
  slice_head(n=5) #slice/take the top 5 for every date
```

Our data now looks like this:

<figure><img src="../.gitbook/assets/image (4) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Perfect! We have the top five for every month. Let's get started on our word cloud.

Since word clouds are built by shuffling the words around the biggest one, it might be useful to set a seed for our plot. This way, we always follow the same randomly generated shuffle, which can be useful for debugging.

```r
set.seed(52)
```

We're now using `geom_text_wordcloud` to make our plot, with the `label` being the city name and the `size` being the volume of houses (for a given month). We scale up the `max_size` of the city with the largest volume to 30 (`scale_size_area`), which is a somewhat arbitrary size, but I've found it works well to show large, readible words, without having points that are too big to fit inside the plot.

Finally, we'll add the `transition_time(date)` to tell R to only show the words for a given date, cycling through dates.

```r
ggplot(txhousing_data)+
  geom_text_wordcloud(aes(label=city, size=volume))+ #make the word plot
  scale_size_area(max_size = 30)+ #scale the words up 
  transition_time(date) #transition over time
```

<figure><img src="../.gitbook/assets/rough2 (1).gif" alt=""><figcaption></figcaption></figure>

This looks like a good start, but we're missing some key information. Let's add a descriptive title and the current year that is being displayed.

The syntax to do this is the exact same as what we've been doing earlier.

```r
ggplot(txhousing_data)+
  geom_text_wordcloud(aes(label=city, size=volume, group=city))+
  scale_size_area(max_size = 30)+
  transition_time(date)+
  labs(title="Texas Cities With The Most Money Spent in Real Estate",
       subtitle="Since 2000, the top five real estate spenders have remained the same, <br>with Houston and Dallas being significantly ahead of the rest.<br><br>{as.integer(frame_time)}")+
  theme_minimal()+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+ #requires the 'ggtext' package is installed
  transition_time(date)
```

As we finalize the plot with frames, its important to note that words will jump around in each frame. So, it's important that we keep the FPS low to accommodate for this, since, unfortunately, there is no way to stop words from switching from frame to frame. By keeping our FPS low, we minimize the number of switches per second, improving readibility. With a lower FPS we still have to show all the frames/data we have, so we have to increase the duration of the animation.

```r
animation <- ggplot(txhousing_data)+
    #continued from above
```

```r
animate(animation, fps=3, duration=35, end_pause=9, height = 8,
        width = 8, units = "in", res = 200)
```

That's it! You've successfully made an animated word cloud.

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/basic-animations/animated-word-cloud.r.md)
{% endhint %}

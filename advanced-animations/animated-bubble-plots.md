# Animated Bubble Plots

Perhaps the most famous animated graphic is one by Hans Rosling, which was popularized by his 2006 [TED Talk](https://www.ted.com/talks/hans\_rosling\_the\_best\_stats\_you\_ve\_ever\_seen?language=en\&subtitle=en), which now stands at 15 million views. You can view the graph [here](https://www.gapminder.org/tools/#$chart-type=bubbles\&url=v2).

While making something as smooth and interactive as Rosling's is beyond the scope of this book, we can try our best to mimic his chart exclusively within R.

The final product will look something like this:

<figure><img src="../.gitbook/assets/final (1) (1) (1).gif" alt="" width="563"><figcaption></figcaption></figure>

***

Let's load in our libraries.

```r
library(ggplot2) #making the plot
library(gganimate) #animating the plot
library(viridis) #prettier scale colors
library(gapminder) #loading the data
```

The dataset that Rosling uses can actually be found within an R library: `gapminder`. To load in the data we'll use to make this chart, simply use the following line. Though technically not required, this line helps us understand what our data looks like.

```r
gapminder_data <- gapminder
```

<figure><img src="../.gitbook/assets/image (2) (1) (1) (1).png" alt=""><figcaption></figcaption></figure>

Our data is already cleaned for us, so we can get started plotting. First, let's make a simple static graph. Looking ahead, we know that we are going to want each point to represent a country, so we can assign `group=country` to our plot (even though it technically doesn't do anything right now).

There's also some basic changes to the `color`s and `alpha` values.

```r
ggplot(gapminder_data)+
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, fill=continent, group=country), 
             shape=21, alpha=0.7)
```

<figure><img src="../.gitbook/assets/rough1 (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

This looks a little messy, but remember that each frame is only going to show the points available for that time, so the graph will get cleaned up. Speaking of cleaning up, we can make our graph look a little nicer before we get to animating. Let's fix some of the axes, scales, the title, and the legend.

```r
ggplot(gapminder_data)+
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, fill=continent, group=country), 
             shape=21, alpha=0.7)+
  scale_size_continuous(label=scales::comma, range = c(1,20))+
  scale_x_continuous(label=scales::comma, trans='log2')+
  theme_minimal()+
  scale_fill_viridis(option='A', discrete=TRUE)+
  scale_color_viridis(option='A', discrete=TRUE)+
  labs(title="GDP Per Capita and Life Expectancy Over 50+ Years",
       size="Population",
       fill="Continent",
       x="GDP Per Capita",
       y="Life Expectancy")+
  guides(color="none")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))
```

<figure><img src="../.gitbook/assets/rough2 (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

This looks much better and we're now ready to animate. We'll use `transition_time` to cycle through the data, meaning that each frame will show only the points available for that point in time. Since time is continous, R will automatically smoothly transition points between stages of data.&#x20;

We'll also add a subtitle with `{frame_time}` to illustrate the year currently being displayed.

```r
ggplot(gapminder_data)+
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, fill=continent, group=country), 
             shape=21, alpha=0.7)+
  scale_size_continuous(label=scales::comma, range = c(1,20))+
  scale_x_continuous(label=scales::comma, trans='log2')+
  theme_minimal()+
  scale_fill_viridis(option='A', discrete=TRUE)+
  scale_color_viridis(option='A', discrete=TRUE)+
  labs(title="GDP Per Capita and Life Expectancy Over 50+ Years",
       subtitle="Year: {frame_time}", #added line
       size="Population",
       fill="Continent",
       x="GDP Per Capita",
       y="Life Expectancy")+
  guides(color="none")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(year) #added line
```

We can now export this graph using the same code as before:

```r
animation <- ggplot(gapminder_data)+
    #continued from above
```

```r
animate(animation, fps=10, duration=30, end_pause=50, height = 7, width = 10, 
        units = "in", res = 200)
```

This graph is ready to be published, but there's a couple more changes that we can try to make. In its current state, none of the points are labeled, so it's a bit difficult to try and make out certain countries within the graph. We can fix this by adding a single line to our plot.

Let's say we wanted to label the United States.

```
+geom_text(aes(x=gdpPercap, y=lifeExp, label=ifelse(country == "United States", "United States", "")))+
```

Essentially, we're putting a text at the X and Y value of every country, but only having that text actually filled if the country is the United States.

<figure><img src="../.gitbook/assets/rough3 (1).gif" alt="" width="563"><figcaption></figcaption></figure>

Another extension we can do is making the plot interactive. We can do this through the `plotly` library, which can read a `ggplot` and "convert" it to an interactive.

```r
library(plotly)
```

There's a few line changes we need to make before we can export the interative animation, since plotly has different syntax. First, we'll need to add `aes(frame=year`) to the `ggplot` call. This is essentially plotly's version of `transition_time(year)`. Next, we'll add `ids=country` to our `geom_point` call. As you can probably guess, this is plotly's version of `group=country`.

```r
animation <- ggplot(gapminder_data, aes(frame=year))+ #changed line
  geom_point(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, fill=continent, group=country, ids=country), #changed line 
             shape=21, alpha=0.7)+
  scale_size_continuous(label=scales::comma, range = c(1,20))+
  scale_x_continuous(label=scales::comma, trans='log2')+
  theme_minimal()+
  scale_fill_viridis(option='A', discrete=TRUE)+
  scale_color_viridis(option='A', discrete=TRUE)+
  labs(title="GDP Per Capita and Life Expectancy Over 50+ Years",
       subtitle="Year: {frame_time}",
       size="Population",
       fill="Continent",
       x="GDP Per Capita",
       y="Life Expectancy")+
  guides(color="none")+
  theme(plot.title = ggtext::element_markdown(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = ggtext::element_markdown(size = 15, hjust =0.5, face = "bold"))+
  transition_time(year)
```

Now we can just call:

```r
ggplotly(animation)
```

And we get a nice interactive, with hoverable data points, exportable as an HTML.

<figure><img src="../.gitbook/assets/image (3) (1) (1).png" alt="" width="563"><figcaption></figcaption></figure>

{% hint style="info" %}
[Click here to view the raw file used to make this animation.](../appendix/advanced-animations/animated-bubble-plot.r.md)
{% endhint %}

# FAQ

Below are a few resources and code snippets that might help to alleviate errors.

***

## Syntax

If you want to know more about the syntax of the gganimate package, consult this [cheat sheet](https://rstudio.github.io/cheatsheets/gganimate.pdf), which contains detailed information about various parameters and what they mean.

<figure><img src=".gitbook/assets/image.png" alt="" width="563"><figcaption></figcaption></figure>

***

## Gifski

If you're having trouble saving your gif (or if its saving to a "tmp" folder), make sure that the "gifski" package is installed and loaded.

```r
install.packages('gifski')
library(gifski)
```

If that still doesn't work, try adding renderer=gifski\_renderer() to your animate function:

```r
animate(animation, renderer=gifski_renderer())
```

***

## Data Sources

If any of the data source links appear "broken", please visit the [GitHub](https://github.com/nikhilc52/animations/tree/main) to obtain a copy of the data. For some large data sources, you can download them from [Google Drive](https://drive.google.com/drive/folders/10IBKDmcAcOFXzGtZROTiYPza4cjzRrRb?usp=sharing).

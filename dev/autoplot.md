R Notebook
================

``` r
library(ffsimulator)
library(ggplot2)
conn <- mfl_connect(2021,22627)

sims <- ff_simulate(conn)

autoplot(sims)
```

![](autoplot_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
autoplot(sims,"rank")
```

![](autoplot_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
autoplot(sims,"points")
```

    ## Picking joint bandwidth of 4.81

![](autoplot_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

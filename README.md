
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Behavioral effects of dog activity

## Load Packages

``` r
library(tidyverse)
library(lubridate)
library(readr)
```

## Data input

``` r
data_set <- read_rds("data/my_data_set.rds")
glimpse(data_set)
#> Rows: 3,152
#> Columns: 8
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <chr> "183", "186", "183", "183", "183", "184", "186", "18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ day               <dbl> 5, 18, 4, 7, 11, 17, 7, 13, 14, 17, 18, 20, 21, 23, ~
#> $ month             <dbl> 11, 10, 12, 12, 12, 10, 12, 12, 12, 10, 10, 10, 10, ~
#> $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014~
```

## identifying points with `m/e` cameras and setting the `date_time`

``` r
data_set <- data_set |> 
  mutate(
   m_e= str_detect(station, "\\(m/e\\)"),
   station = str_remove(station, "\\(m/e\\)"),
   station = ifelse(station == "NA", NA, as.numeric(station)),
   date_time = ymd_hms(paste(date, time))
   )
glimpse(data_set)
#> Rows: 3,152
#> Columns: 10
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <dbl> 183, 186, 183, 183, 183, 184, 186, 186, 186, 186, 18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ day               <dbl> 5, 18, 4, 7, 11, 17, 7, 13, 14, 17, 18, 20, 21, 23, ~
#> $ month             <dbl> 11, 10, 12, 12, 12, 10, 12, 12, 12, 10, 10, 10, 10, ~
#> $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014~
#> $ m_e               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
#> $ date_time         <dttm> 2014-11-05 00:02:29, 2014-10-18 00:12:24, 2014-12-0~
```

# Quick view of ‘capture’ frequency

``` r
data_set |> 
  ggplot(aes(x=date,y=time,color=conservation_unit)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~conservation_unit, scale="free", nrow = 5)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

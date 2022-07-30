
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Behavioral effects of dog activity

## Load Packages

``` r
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
```

## Data input

``` r
data_set <- read_rds("data/my_data_set_2.rds")
data_set <- data_set |> 
  mutate(
    species = ifelse(species == "Dasypsu", "Dasypus", species),
    conservation_unit = ifelse(str_detect(conservation_unit,"Furnas"),"Furnas",conservation_unit)
  )
glimpse(data_set)
#> Rows: 3,417
#> Columns: 9
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <chr> "183", "186", "183", "183", "183", "184", "186", "18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
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
#> Rows: 3,417
#> Columns: 11
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <dbl> 183, 186, 183, 183, 183, 184, 186, 186, 186, 186, 18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
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

# Identify the pray and predator

``` r
pray <- c("Dasypus", "Cabassous", "Euphractus")
data_set <- data_set |> 
  mutate(
    type = ifelse(species %in% pray, "pray", "predator")
  )
glimpse(data_set)
#> Rows: 3,417
#> Columns: 12
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <dbl> 183, 186, 183, 183, 183, 184, 186, 186, 186, 186, 18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
#> $ day               <dbl> 5, 18, 4, 7, 11, 17, 7, 13, 14, 17, 18, 20, 21, 23, ~
#> $ month             <dbl> 11, 10, 12, 12, 12, 10, 12, 12, 12, 10, 10, 10, 10, ~
#> $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014~
#> $ m_e               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
#> $ date_time         <dttm> 2014-11-05 00:02:29, 2014-10-18 00:12:24, 2014-12-0~
#> $ type              <chr> "pray", "pray", "pray", "pray", "pray", "pray", "pra~
```

# Calculating the time

``` r
data_set_aux <- data_set |> 
  arrange(conservation_unit, mestre, station, date_time)

data_set_aux$diff_time <- NA
for(i in 1:(nrow(data_set_aux)-1)){
  unit_con <- data_set_aux$conservation_unit[i]
  mestre <- data_set_aux$mestre[i]
  station <- data_set_aux$station[i]
  if((unit_con == data_set_aux$conservation_unit[i+1] &
     mestre == data_set_aux$mestre[i+1] &
      station == data_set_aux$station[i+1]) | is.na(data_set_aux$station[i+1]) ){
    if(data_set_aux$type[i] == "predator" & 
       data_set_aux$type[i+1] == "pray"){
      data_set_aux$diff_time[i] =  as.numeric(
        difftime(
          data_set_aux$date_time[i+1], data_set_aux$date_time[i],units = "hours"))
       }
  }else{
        data_set_aux$diff_time[i] <- NA
      }
}
writexl::write_xlsx(data_set_aux,"data/banco_de_dados.xlsx")
```

## The same approch for the new data

## Data input

``` r
data_set <- read_rds("data/my_data_set_novo.rds")
data_set <- data_set |> 
  mutate(
    species = ifelse(species == "Dasypsu", "Dasypus", species),
    conservation_unit = ifelse(str_detect(conservation_unit,"Furnas"),"Furnas",conservation_unit)
  )
glimpse(data_set)
#> Rows: 3,951
#> Columns: 9
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <chr> "183", "186", "183", "183", "183", "184", "186", "18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
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
#> Rows: 3,951
#> Columns: 11
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <dbl> 183, 186, 183, 183, 183, 184, 186, 186, 186, 186, 18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
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

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# Identify the pray and predator

``` r
pray <- c("Dasypus", "Cabassous", "Euphractus")
predators <- c("Cfamiliaris", "Lpardalis", "Cbrachyurus", "Pconcolor")
data_set <- data_set |> 
  mutate(
    type = ifelse(species %in% pray, "pray", "predator")
  )
```

``` r
data_set <- data_set |> 
  filter(species != "Mtridactyla")   
glimpse(data_set)
#> Rows: 1,897
#> Columns: 12
#> $ conservation_unit <chr> "Bebedouro", "Bebedouro", "Bebedouro", "Bebedouro", ~
#> $ species           <chr> "Dasypus", "Dasypus", "Dasypus", "Dasypus", "Dasypus~
#> $ station           <dbl> 183, 186, 183, 183, 183, 184, 186, 186, 186, 186, 18~
#> $ date              <date> 2014-11-05, 2014-10-18, 2014-12-04, 2014-12-07, 201~
#> $ time              <time> 00:02:29, 00:12:24, 04:00:20, 03:33:39, 21:10:36, 2~
#> $ mestre            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
#> $ day               <dbl> 5, 18, 4, 7, 11, 17, 7, 13, 14, 17, 18, 20, 21, 23, ~
#> $ month             <dbl> 11, 10, 12, 12, 12, 10, 12, 12, 12, 10, 10, 10, 10, ~
#> $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014~
#> $ m_e               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
#> $ date_time         <dttm> 2014-11-05 00:02:29, 2014-10-18 00:12:24, 2014-12-0~
#> $ type              <chr> "pray", "pray", "pray", "pray", "pray", "pray", "pra~
```

# Calculating the time

``` r
data_set_aux <- data_set |> 
  arrange(conservation_unit, mestre, station, date_time)

data_set_aux$diff_time <- NA
for(i in 1:(nrow(data_set_aux)-1)){
  unit_con <- data_set_aux$conservation_unit[i]
  mestre <- data_set_aux$mestre[i]
  station <- data_set_aux$station[i]
  if((unit_con == data_set_aux$conservation_unit[i+1] &
     mestre == data_set_aux$mestre[i+1] &
      station == data_set_aux$station[i+1]) | is.na(data_set_aux$station[i+1]) ){
    if(data_set_aux$type[i] == "predator" & 
       data_set_aux$type[i+1] == "pray"){
      data_set_aux$diff_time[i] =  as.numeric(
        difftime(
          data_set_aux$date_time[i+1], data_set_aux$date_time[i],units = "hours"))
       }
  }else{
        data_set_aux$diff_time[i] <- NA
      }
}
writexl::write_xlsx(data_set_aux,"data/banco_de_dados_novo.xlsx")
```

# Calculating the time without multiple predator effect.

``` r
data_set_aux <- data_set |>
  arrange(conservation_unit, mestre, station, date_time)
units <- data_set_aux |> pull(conservation_unit) |>  unique()
cont=0
for(i in seq_along(units)){
  df <- data_set_aux |>
    filter(conservation_unit == units[i])
  stations <-  df |>  pull(station) |>  unique()
  for(j in seq_along(stations)){
    dff <- df |>
      filter(station == stations[j])
    n_pray <- sum(dff |> pull(type) == "pray")
    n_predator <- sum(dff |> pull(type) == "predator")
    dff$flag <- NA
    dff$diff_time <- NA
    if((n_pray != 0 & nrow(dff) >1) & n_predator > 0){
      for(k in 1:(nrow(dff)-1)){
        if(dff$type[k]=="pray") cont=0
        if((dff$type[k] == "predator" & dff$type[k+1] =="pray") & cont == 0){
          cont = 0
          dff$flag[k] <- paste("homogeneo", cont)
          dff$diff_time[k] =  as.numeric(
            difftime(dff$date_time[k+1], dff$date_time[k],units = "hours"))
        }
        if(dff$type[k] == "predator" & dff$type[k+1] =="predator"){
          if(dff$species[k] != dff$species[k+1]) {
            cont = cont + 1
            dff$flag[k] <- paste("heterogeneo", cont)}
        }
      }
      cont = 0
    }
    if(i == 1 & j == 1){
      dff_final <- dff
    }else{
      dff_final <- rbind(dff_final, dff)
    }
  }
}
dff_final
#> # A tibble: 1,897 x 14
#>    conservation_un~ species station date       time     mestre   day month  year
#>    <chr>            <chr>     <dbl> <date>     <time>   <lgl>  <dbl> <dbl> <dbl>
#>  1 Bebedouro        Cfamil~     183 2014-10-20 11:14:46 FALSE     20    10  2014
#>  2 Bebedouro        Cfamil~     183 2014-11-03 10:56:26 FALSE      3    11  2014
#>  3 Bebedouro        Dasypus     183 2014-11-05 00:02:29 FALSE      5    11  2014
#>  4 Bebedouro        Cfamil~     183 2014-12-03 12:02:55 FALSE      3    12  2014
#>  5 Bebedouro        Dasypus     183 2014-12-04 04:00:20 FALSE      4    12  2014
#>  6 Bebedouro        Dasypus     183 2014-12-07 03:33:39 FALSE      7    12  2014
#>  7 Bebedouro        Dasypus     183 2014-12-11 21:10:36 FALSE     11    12  2014
#>  8 Bebedouro        Dasypus     184 2014-10-17 21:48:32 FALSE     17    10  2014
#>  9 Bebedouro        Dasypus     186 2014-10-17 22:26:24 FALSE     17    10  2014
#> 10 Bebedouro        Dasypus     186 2014-10-18 00:12:24 FALSE     18    10  2014
#> # ... with 1,887 more rows, and 5 more variables: m_e <lgl>, date_time <dttm>,
#> #   type <chr>, flag <chr>, diff_time <dbl>
writexl::write_xlsx(dff_final,"data/dff_final.xlsx")
```

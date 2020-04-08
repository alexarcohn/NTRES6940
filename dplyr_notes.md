dplyr notes
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.2     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Exploration of the Coronavirus dataset
--------------------------------------

First, we'll load and examine the data

``` r
coronavirus <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv', col_types = cols(Province.State = col_character()))
coronavirus
```

    ## # A tibble: 59,675 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           Afghanistan       33    65 2020-01-22     0 confirmed
    ##  2 <NA>           Afghanistan       33    65 2020-01-23     0 confirmed
    ##  3 <NA>           Afghanistan       33    65 2020-01-24     0 confirmed
    ##  4 <NA>           Afghanistan       33    65 2020-01-25     0 confirmed
    ##  5 <NA>           Afghanistan       33    65 2020-01-26     0 confirmed
    ##  6 <NA>           Afghanistan       33    65 2020-01-27     0 confirmed
    ##  7 <NA>           Afghanistan       33    65 2020-01-28     0 confirmed
    ##  8 <NA>           Afghanistan       33    65 2020-01-29     0 confirmed
    ##  9 <NA>           Afghanistan       33    65 2020-01-30     0 confirmed
    ## 10 <NA>           Afghanistan       33    65 2020-01-31     0 confirmed
    ## # … with 59,665 more rows

``` r
# View(coronavirus)
head(coronavirus)
```

    ## # A tibble: 6 x 7
    ##   Province.State Country.Region   Lat  Long date       cases type     
    ##   <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ## 1 <NA>           Afghanistan       33    65 2020-01-22     0 confirmed
    ## 2 <NA>           Afghanistan       33    65 2020-01-23     0 confirmed
    ## 3 <NA>           Afghanistan       33    65 2020-01-24     0 confirmed
    ## 4 <NA>           Afghanistan       33    65 2020-01-25     0 confirmed
    ## 5 <NA>           Afghanistan       33    65 2020-01-26     0 confirmed
    ## 6 <NA>           Afghanistan       33    65 2020-01-27     0 confirmed

``` r
tail(coronavirus)
```

    ## # A tibble: 6 x 7
    ##   Province.State Country.Region   Lat  Long date       cases type     
    ##   <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ## 1 Zhejiang       China           29.2  120. 2020-04-02     2 recovered
    ## 2 Zhejiang       China           29.2  120. 2020-04-03     0 recovered
    ## 3 Zhejiang       China           29.2  120. 2020-04-04     1 recovered
    ## 4 Zhejiang       China           29.2  120. 2020-04-05     1 recovered
    ## 5 Zhejiang       China           29.2  120. 2020-04-06     0 recovered
    ## 6 Zhejiang       China           29.2  120. 2020-04-07     0 recovered

``` r
names(coronavirus)
```

    ## [1] "Province.State" "Country.Region" "Lat"            "Long"          
    ## [5] "date"           "cases"          "type"

``` r
dim(coronavirus)
```

    ## [1] 59675     7

``` r
summary(coronavirus)
```

    ##  Province.State     Country.Region          Lat               Long        
    ##  Length:59675       Length:59675       Min.   :-51.796   Min.   :-135.00  
    ##  Class :character   Class :character   1st Qu.:  6.877   1st Qu.: -15.18  
    ##  Mode  :character   Mode  :character   Median : 22.300   Median :  20.94  
    ##                                        Mean   : 20.930   Mean   :  23.93  
    ##                                        3rd Qu.: 40.182   3rd Qu.:  84.25  
    ##                                        Max.   : 71.707   Max.   : 178.06  
    ##       date                cases             type          
    ##  Min.   :2020-01-22   Min.   : -268.0   Length:59675      
    ##  1st Qu.:2020-02-10   1st Qu.:    0.0   Class :character  
    ##  Median :2020-02-29   Median :    0.0   Mode  :character  
    ##  Mean   :2020-02-29   Mean   :   30.3                     
    ##  3rd Qu.:2020-03-19   3rd Qu.:    0.0                     
    ##  Max.   :2020-04-07   Max.   :33264.0

``` r
# install.packages('skimr')
library(skimr)
skim(coronavirus)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | coronavirus |
| Number of rows                                   | 59675       |
| Number of columns                                | 7           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 3           |
| Date                                             | 1           |
| numeric                                          | 3           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

**Variable type: character**

| skim\_variable |  n\_missing|  complete\_rate|  min|  max|  empty|  n\_unique|  whitespace|
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| Province.State |       41888|             0.3|    5|   32|      0|         82|           0|
| Country.Region |           0|             1.0|    2|   32|      0|        184|           0|
| type           |           0|             1.0|    5|    9|      0|          3|           0|

**Variable type: Date**

| skim\_variable |  n\_missing|  complete\_rate| min        | max        | median     |  n\_unique|
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |           0|               1| 2020-01-22 | 2020-04-07 | 2020-02-29 |         77|

**Variable type: numeric**

| skim\_variable |  n\_missing|  complete\_rate|   mean|      sd|      p0|     p25|    p50|    p75|      p100| hist  |
|:---------------|-----------:|---------------:|------:|-------:|-------:|-------:|------:|------:|---------:|:------|
| Lat            |           0|               1|  20.93|   24.63|   -51.8|    6.88|  22.30|  40.18|     71.71| ▁▂▇▇▃ |
| Long           |           0|               1|  23.93|   69.68|  -135.0|  -15.18|  20.94|  84.25|    178.06| ▂▃▇▃▂ |
| cases          |           0|               1|  30.30|  481.70|  -268.0|    0.00|   0.00|   0.00|  33264.00| ▇▁▁▁▁ |

``` r
# coronavirus$cases
head(coronavirus$cases)
```

    ## [1] 0 0 0 0 0 0

Now let's explore the dplyr functions

``` r
filter(coronavirus, cases > 0)
```

    ## # A tibble: 10,973 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           Afghanistan       33    65 2020-02-24     1 confirmed
    ##  2 <NA>           Afghanistan       33    65 2020-03-08     3 confirmed
    ##  3 <NA>           Afghanistan       33    65 2020-03-10     1 confirmed
    ##  4 <NA>           Afghanistan       33    65 2020-03-11     2 confirmed
    ##  5 <NA>           Afghanistan       33    65 2020-03-14     4 confirmed
    ##  6 <NA>           Afghanistan       33    65 2020-03-15     5 confirmed
    ##  7 <NA>           Afghanistan       33    65 2020-03-16     5 confirmed
    ##  8 <NA>           Afghanistan       33    65 2020-03-17     1 confirmed
    ##  9 <NA>           Afghanistan       33    65 2020-03-20     2 confirmed
    ## 10 <NA>           Afghanistan       33    65 2020-03-22    16 confirmed
    ## # … with 10,963 more rows

``` r
filter(coronavirus, Country.Region == "US")
```

    ## # A tibble: 231 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           US              37.1 -95.7 2020-01-22     1 confirmed
    ##  2 <NA>           US              37.1 -95.7 2020-01-23     0 confirmed
    ##  3 <NA>           US              37.1 -95.7 2020-01-24     1 confirmed
    ##  4 <NA>           US              37.1 -95.7 2020-01-25     0 confirmed
    ##  5 <NA>           US              37.1 -95.7 2020-01-26     3 confirmed
    ##  6 <NA>           US              37.1 -95.7 2020-01-27     0 confirmed
    ##  7 <NA>           US              37.1 -95.7 2020-01-28     0 confirmed
    ##  8 <NA>           US              37.1 -95.7 2020-01-29     0 confirmed
    ##  9 <NA>           US              37.1 -95.7 2020-01-30     0 confirmed
    ## 10 <NA>           US              37.1 -95.7 2020-01-31     2 confirmed
    ## # … with 221 more rows

``` r
coronavirus_us <- filter(coronavirus, Country.Region == "US")

filter(coronavirus, Country.Region == "US" | Country.Region == "Canada")
```

    ## # A tibble: 2,618 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           US              37.1 -95.7 2020-01-22     1 confirmed
    ##  2 <NA>           US              37.1 -95.7 2020-01-23     0 confirmed
    ##  3 <NA>           US              37.1 -95.7 2020-01-24     1 confirmed
    ##  4 <NA>           US              37.1 -95.7 2020-01-25     0 confirmed
    ##  5 <NA>           US              37.1 -95.7 2020-01-26     3 confirmed
    ##  6 <NA>           US              37.1 -95.7 2020-01-27     0 confirmed
    ##  7 <NA>           US              37.1 -95.7 2020-01-28     0 confirmed
    ##  8 <NA>           US              37.1 -95.7 2020-01-29     0 confirmed
    ##  9 <NA>           US              37.1 -95.7 2020-01-30     0 confirmed
    ## 10 <NA>           US              37.1 -95.7 2020-01-31     2 confirmed
    ## # … with 2,608 more rows

``` r
US_deaths <- filter(coronavirus, Country.Region == "US", type == "death")
sum(US_deaths$cases)
```

    ## [1] 12722

``` r
select(coronavirus, -Lat, -Long)
```

    ## # A tibble: 59,675 x 5
    ##    Province.State Country.Region date       cases type     
    ##    <chr>          <chr>          <date>     <dbl> <chr>    
    ##  1 <NA>           Afghanistan    2020-01-22     0 confirmed
    ##  2 <NA>           Afghanistan    2020-01-23     0 confirmed
    ##  3 <NA>           Afghanistan    2020-01-24     0 confirmed
    ##  4 <NA>           Afghanistan    2020-01-25     0 confirmed
    ##  5 <NA>           Afghanistan    2020-01-26     0 confirmed
    ##  6 <NA>           Afghanistan    2020-01-27     0 confirmed
    ##  7 <NA>           Afghanistan    2020-01-28     0 confirmed
    ##  8 <NA>           Afghanistan    2020-01-29     0 confirmed
    ##  9 <NA>           Afghanistan    2020-01-30     0 confirmed
    ## 10 <NA>           Afghanistan    2020-01-31     0 confirmed
    ## # … with 59,665 more rows

Now to dplyr exploration part 2

``` r
coronavirus_us <- filter(coronavirus, Country.Region == "US")
coronavirus_us2 <- select(coronavirus_us, - Lat, -Long, -Province.State)

coronavirus %>% filter (Country.Region == "US")
```

    ## # A tibble: 231 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           US              37.1 -95.7 2020-01-22     1 confirmed
    ##  2 <NA>           US              37.1 -95.7 2020-01-23     0 confirmed
    ##  3 <NA>           US              37.1 -95.7 2020-01-24     1 confirmed
    ##  4 <NA>           US              37.1 -95.7 2020-01-25     0 confirmed
    ##  5 <NA>           US              37.1 -95.7 2020-01-26     3 confirmed
    ##  6 <NA>           US              37.1 -95.7 2020-01-27     0 confirmed
    ##  7 <NA>           US              37.1 -95.7 2020-01-28     0 confirmed
    ##  8 <NA>           US              37.1 -95.7 2020-01-29     0 confirmed
    ##  9 <NA>           US              37.1 -95.7 2020-01-30     0 confirmed
    ## 10 <NA>           US              37.1 -95.7 2020-01-31     2 confirmed
    ## # … with 221 more rows

``` r
coronavirus_us %>% select(-Lat, -Long, -Province.State)
```

    ## # A tibble: 231 x 4
    ##    Country.Region date       cases type     
    ##    <chr>          <date>     <dbl> <chr>    
    ##  1 US             2020-01-22     1 confirmed
    ##  2 US             2020-01-23     0 confirmed
    ##  3 US             2020-01-24     1 confirmed
    ##  4 US             2020-01-25     0 confirmed
    ##  5 US             2020-01-26     3 confirmed
    ##  6 US             2020-01-27     0 confirmed
    ##  7 US             2020-01-28     0 confirmed
    ##  8 US             2020-01-29     0 confirmed
    ##  9 US             2020-01-30     0 confirmed
    ## 10 US             2020-01-31     2 confirmed
    ## # … with 221 more rows

``` r
coronavirus %>% head(3)
```

    ## # A tibble: 3 x 7
    ##   Province.State Country.Region   Lat  Long date       cases type     
    ##   <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ## 1 <NA>           Afghanistan       33    65 2020-01-22     0 confirmed
    ## 2 <NA>           Afghanistan       33    65 2020-01-23     0 confirmed
    ## 3 <NA>           Afghanistan       33    65 2020-01-24     0 confirmed

``` r
coronavirus_us <- coronavirus %>% 
  filter(Country.Region == "US") %>% 
  select(-Lat, -Long, -Province.State)

coronavirus_us <- coronavirus[coronavirus$Country.Region == "US", colnames(coronavirus) %in% c("Lat", "Long", "Province.State")]
```

Now let's look at the dataset in wide format

``` r
coronavirus_ttd <- coronavirus %>% 
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

coronavirus_ttd
```

    ## # A tibble: 184 x 4
    ## # Groups:   country [184]
    ##    country        confirmed death recovered
    ##    <chr>              <dbl> <dbl>     <dbl>
    ##  1 US                396223 12722     21763
    ##  2 Spain             141942 14045     43208
    ##  3 Italy             135586 17127     24392
    ##  4 France            110065 10343     19523
    ##  5 Germany           107663  2016     36081
    ##  6 China              82718  3335     77410
    ##  7 Iran               62589  3872     27039
    ##  8 United Kingdom     55949  6171       325
    ##  9 Turkey             34109   725      1582
    ## 10 Switzerland        22253   821      8704
    ## # … with 174 more rows

``` r
coronavirus_ttd %>% 
  mutate(deathrate = round(death / confirmed, 2))
```

    ## # A tibble: 184 x 5
    ## # Groups:   country [184]
    ##    country        confirmed death recovered deathrate
    ##    <chr>              <dbl> <dbl>     <dbl>     <dbl>
    ##  1 US                396223 12722     21763      0.03
    ##  2 Spain             141942 14045     43208      0.1 
    ##  3 Italy             135586 17127     24392      0.13
    ##  4 France            110065 10343     19523      0.09
    ##  5 Germany           107663  2016     36081      0.02
    ##  6 China              82718  3335     77410      0.04
    ##  7 Iran               62589  3872     27039      0.06
    ##  8 United Kingdom     55949  6171       325      0.11
    ##  9 Turkey             34109   725      1582      0.02
    ## 10 Switzerland        22253   821      8704      0.04
    ## # … with 174 more rows

``` r
coronavirus_ttd %>% 
  mutate(undetermined = (confirmed - death - recovered) / confirmed) %>% 
  filter(confirmed > 20000) %>% 
  arrange(undetermined)
```

    ## # A tibble: 11 x 5
    ## # Groups:   country [11]
    ##    country        confirmed death recovered undetermined
    ##    <chr>              <dbl> <dbl>     <dbl>        <dbl>
    ##  1 China              82718  3335     77410       0.0239
    ##  2 Iran               62589  3872     27039       0.506 
    ##  3 Switzerland        22253   821      8704       0.572 
    ##  4 Spain             141942 14045     43208       0.597 
    ##  5 Germany           107663  2016     36081       0.646 
    ##  6 Italy             135586 17127     24392       0.694 
    ##  7 Belgium            22194  2035      4157       0.721 
    ##  8 France            110065 10343     19523       0.729 
    ##  9 United Kingdom     55949  6171       325       0.884 
    ## 10 US                396223 12722     21763       0.913 
    ## 11 Turkey             34109   725      1582       0.932

``` r
coronavirus_ttd %>% 
  filter(death > 3000) %>% 
  arrange(-death)
```

    ## # A tibble: 7 x 4
    ## # Groups:   country [7]
    ##   country        confirmed death recovered
    ##   <chr>              <dbl> <dbl>     <dbl>
    ## 1 Italy             135586 17127     24392
    ## 2 Spain             141942 14045     43208
    ## 3 US                396223 12722     21763
    ## 4 France            110065 10343     19523
    ## 5 United Kingdom     55949  6171       325
    ## 6 Iran               62589  3872     27039
    ## 7 China              82718  3335     77410

``` r
coronavirus %>% 
  filter(type == "death") %>% 
  arrange(-cases)
```

    ## # A tibble: 20,251 x 7
    ##    Province.State Country.Region   Lat   Long date       cases type 
    ##    <chr>          <chr>          <dbl>  <dbl> <date>     <dbl> <chr>
    ##  1 <NA>           US              37.1 -95.7  2020-04-07  1939 death
    ##  2 <NA>           France          46.2   2.21 2020-04-07  1417 death
    ##  3 <NA>           France          46.2   2.21 2020-04-02  1355 death
    ##  4 <NA>           US              37.1 -95.7  2020-04-04  1320 death
    ##  5 <NA>           US              37.1 -95.7  2020-04-05  1212 death
    ##  6 <NA>           US              37.1 -95.7  2020-04-02  1169 death
    ##  7 <NA>           US              37.1 -95.7  2020-04-06  1164 death
    ##  8 <NA>           US              37.1 -95.7  2020-04-03  1161 death
    ##  9 <NA>           France          46.2   2.21 2020-04-03  1120 death
    ## 10 <NA>           France          46.2   2.21 2020-04-04  1053 death
    ## # … with 20,241 more rows

``` r
coronavirus %>%
  filter(Country.Region == "Denmark", cases > 0) %>% 
  arrange(date)
```

    ## # A tibble: 119 x 7
    ##    Province.State Country.Region   Lat  Long date       cases type     
    ##    <chr>          <chr>          <dbl> <dbl> <date>     <dbl> <chr>    
    ##  1 <NA>           Denmark         56.3  9.50 2020-02-27     1 confirmed
    ##  2 <NA>           Denmark         56.3  9.50 2020-02-29     2 confirmed
    ##  3 <NA>           Denmark         56.3  9.50 2020-03-01     1 confirmed
    ##  4 <NA>           Denmark         56.3  9.50 2020-03-03     2 confirmed
    ##  5 <NA>           Denmark         56.3  9.50 2020-03-04     4 confirmed
    ##  6 Faroe Islands  Denmark         61.9 -6.91 2020-03-04     1 confirmed
    ##  7 <NA>           Denmark         56.3  9.50 2020-03-06    13 confirmed
    ##  8 <NA>           Denmark         56.3  9.50 2020-03-06     1 recovered
    ##  9 <NA>           Denmark         56.3  9.50 2020-03-08    12 confirmed
    ## 10 Faroe Islands  Denmark         61.9 -6.91 2020-03-08     1 confirmed
    ## # … with 109 more rows

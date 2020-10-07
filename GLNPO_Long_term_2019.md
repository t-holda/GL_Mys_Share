Great Lakes Mysid Abundance Trends 1997-2019
================

# Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

### Setup data

#### Load package libraries

``` r
library(tidyverse)
library(mgcv)
```

#### Load data and modify as necessary

``` r
GLNPO =
  
  read.csv("GLNPO_Zooplankton_DB_ZoopDensBmssTable_MYSRELI_20200919.csv") %>% 
  select(Visit, Lake, Year, Season, Date, Station, StationDepth, DepthZone, ZoopDens = NUM_M2, MysDens = Mysid_Dens, ZoopBiom = BIOM_mgM2, MysBiom = Mysid_Bms, ZoopDN = Day_Nite_Zoop, MysDN = Day_Nite_Mys, ZoopTimeEDT = TimeZoopSam_EDT, MysTimeEDT = TimeMysSam_EDT, Regress = Regress.) %>% 
  modify_at(c("StationDepth", "ZoopDens", "MysDens", "ZoopBiom", "MysBiom"), as.numeric) %>% 
  modify_at(c("Lake", "Season", "DepthZone", "ZoopDN", "MysDN"), function(field){factor(field,ordered=F)}) %>%
  modify_at(c("Date"), as.Date)


GLNPO =
  GLNPO %>% 
  modify_at(c("ZoopTimeEDT", "MysTimeEDT"), function(time) {paste(GLNPO$Date, time, sep = " ")}) %>%
  modify_at(c("ZoopTimeEDT", "MysTimeEDT"), function(time) {strptime(time, format = "%Y-%m-%d %H:%M", tz = "EST5EDT")}) %>% 
  as_tibble()
```

#### Examine `GLNPO` tibble

    ## # A tibble: 3,965 x 17
    ##    Visit Lake   Year Season Date       Station StationDepth DepthZone ZoopDens
    ##    <chr> <fct> <int> <fct>  <date>     <chr>          <dbl> <fct>        <dbl>
    ##  1 M047~ Mich~  2000 Summer 2000-08-26 MI47             195 Offshore    236.  
    ##  2 M41M~ Mich~  2000 Summer 2000-08-26 MI41M            263 Offshore      4.96
    ##  3 M040~ Mich~  2000 Summer 2000-08-26 MI40             169 Offshore      0   
    ##  4 M032~ Mich~  2000 Summer 2000-08-27 MI32             164 Offshore    111.  
    ##  5 M034~ Mich~  2000 Summer 2000-08-26 MI34             158 Offshore      0   
    ##  6 M27M~ Mich~  2000 Summer 2000-08-27 MI27M            105 Offshore      4.99
    ##  7 M023~ Mich~  2000 Summer 2000-08-27 MI23              91 Offshore      0   
    ##  8 M019~ Mich~  2000 Summer 2000-08-27 MI19              91 Offshore      7   
    ##  9 M18M~ Mich~  2000 Summer 2000-08-27 MI18M            160 Offshore    326.  
    ## 10 M017~ Mich~  2000 Summer 2000-08-28 MI17             100 Offshore     65.4 
    ## # ... with 3,955 more rows, and 8 more variables: MysDens <dbl>,
    ## #   ZoopBiom <dbl>, MysBiom <dbl>, ZoopDN <fct>, MysDN <fct>,
    ## #   ZoopTimeEDT <dttm>, MysTimeEDT <dttm>, Regress <lgl>

<br>

## Begin plotting relationships between nets

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-2.png)<!-- -->

  - Correlations look good, but  
  - Lines do not seem to match 1:1 lines nor include them in confidence
    intervals.

Adjusted r^2 values for the linear fits plotted above:  
Density: 0.61  
Biomass: 0.33

How do regression model assumptions perform, and should the data need to
be transformed?

<br>

#### Transform density data?

Make plots of density regression diagnostics using:  
1\. Linear (untransformed) data  
2\. Log\_e transformed data 3. Square-root transformed data  
4\. Fourth-root transformed data

In addition, make a plot for each of these with data displayed in
transformed units, with 1:1 reference line, and with linear lm fit line.

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-5.png)<!-- -->

Repeat the same thing for biomass data: Make plots of density regression
diagnostics using:  
1\. Linear (untransformed) data  
2\. Log\_e transformed data  
3\. Square-root transformed data  
4\. Fourth-root transformed data

In addition, make a plot for each of these with data displayed in
transformed units, with 1:1 reference line, and with linear lm fit line.

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-5.png)<!-- -->

### Transformation conclusion:

  - The diagnostics look pretty good for most of the transformations on
    the whole.
  - Linear has issues with scale-location (density and biomass),
    leverage points (density and biomass), and normality (density and
    biomass).
  - Natural Log has issues with homoscedasticity (biomass and perhaps
    density) and perhaps normality (density).
  - Square-Root has issues with scale-location (density and perhaps
    biomass), leverage points (just biomass) and normality (biomass and
    perhaps density).
  - Fourth-Root may have a slight issue with homoscedasticity, but no
    more than the others and scale loction looks better (density and
    biomass).

Thus, the Fourth-Root transformation seems to perform the best overall
in terms of the regression diagnostics.

The Fourth-Root transformations also clearly performs the best in terms
of the actual fit. The slopes are very close to 1, and primarily have
intercept offsets.

<br>

## 4th-Root Transformation Fit: Stats and Plots

    ## [1] "Density 4th-Root Fit Statistics:"

    ##               Estimate Std. Error   t value      Pr(>|t|)
    ## (Intercept) -0.3131127 0.09445379 -3.314983  9.864665e-04
    ## MysDens      0.9817618 0.03018113 32.528999 4.764670e-123

    ## [1] "Adjusted r^2:"

    ## [1] 0.6890758

    ## `geom_smooth()` using formula 'y ~ x'

![](GLNPO_Long_term_2019_files/figure-gfm/4th-Root%20Fit%20Stats%20and%20Plots-1.png)<!-- -->

    ## [1] "Biomass 4th-Root Fit Statistics:"

    ##               Estimate Std. Error   t value      Pr(>|t|)
    ## (Intercept) -0.5160127 0.11393639 -4.528954  7.501934e-06
    ## MysBiom      0.8539635 0.02995537 28.507858 5.211604e-105

    ## [1] "Adjusted r^2:"

    ## [1] 0.629859

    ## `geom_smooth()` using formula 'y ~ x'

![](GLNPO_Long_term_2019_files/figure-gfm/4th-Root%20Fit%20Stats%20and%20Plots-2.png)<!-- -->

#### Clearly these data are worth using for *Mysis diluviana* in the Great Lakes.

  - Zoop net data will correctly display direction and relative
    magnitude of any changes over time or between lakes.  
  - Zoop net data will probably not representvalues similar to Mysid net
    data.  
  - For densities from the Zoop net, the values themselves are only
    different from the higher Mysid net values by a constant negative
    offset when 4th-root transformed.

<br>

# Upload Remaining Annual Monitoring Data And Compile Into Single Tibble Object

<br>

# Compare Values Among Lakes

<br>

# Examine Time Series Trends in Each Lake

<br> <br>

-----

# End of Script.

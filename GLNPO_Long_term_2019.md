Great Lakes Mysid Abundance Trends 1997-2019
================

# 1\. Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

### Setup data

#### Load package libraries

``` r
library(tidyverse)
library(mgcv)
library(skimr)
```

#### Load data and modify as necessary

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

First, which variable on ‘Y’ and which on ‘X’ axis? Plot the more
variable data on the ‘Y’ axis.

Mysid net data are likely to be less variable than Zooplankton net data,
because the mysid data is based on sampling of about 8x more area than
the zooplankton net. The mysid net opening is 0.785, and is towed twice.
The zooplankton net opening is 0.196, and is towed just once.

This is borne out by comparing their CV’s in this dataset:

  - Mysid Net Density CV: 1.13
  - Zoop Net Density CV: 2.59
  - Mysid Net Biomass CV: 1.11
  - Zoop Net Biomass CV: 3.68

OK: plot `Zoop...` data on ‘Y’ axis and `Mys...` data on ‘X’ axis.

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-2.png)<!-- -->

*(Adjusted r^2 values for the linear fits plotted above):*

  - *Density: 0.61*
  - *Biomass: 0.33*

<br> Conclusions:

  - Correlations look good. Can use the data.
  - Lines do not seem to match 1:1 lines nor include them in confidence
    intervals. Zooplankton net values cannot be considered the same as
    the values one would obtain with mysid net.

<br>

<br>

#### Transform density data?

Do regression model assumptions perform well, or should the data need to
be transformed?  
Make plots of density regression diagnostics using:

1.  Linear (untransformed) data

2.  Log\_e transformed data

3.  Square-root transformed data

4.  Fourth-root transformed data

In addition, make a plot for each of these with data displayed in
transformed units, with 1:1 reference line, and with linear lm fit line.

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-5.png)<!-- -->

Repeat the same thing for biomass data. Make plots of density regression
diagnostics using:

1.  Linear (untransformed) data

2.  Log\_e transformed data

3.  Square-root transformed data

4.  Fourth-root transformed data

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

#### **Thus:**

  - **The Fourth-Root transformation seems to perform the best overall
    in terms of the regression diagnostics.**
  - The Fourth-Root transformations also clearly performs the best in
    terms of the actual fit. The slopes are very close to 1, and
    primarily have intercept offsets.

<br>

## 4th-Root Transformation Fit: Stats and Plots

    ## [1] "Density 4th-Root Fit Statistics:"

    ##               Estimate Std. Error   t value      Pr(>|t|)
    ## (Intercept) -0.3131127 0.09445379 -3.314983  9.864665e-04
    ## MysDens      0.9817618 0.03018113 32.528999 4.764670e-123

    ## [1] "Adjusted r^2:"

    ## [1] 0.6890758

![](GLNPO_Long_term_2019_files/figure-gfm/4th-Root%20Fit%20Stats%20and%20Plots-1.png)<!-- -->

    ## [1] "Biomass 4th-Root Fit Statistics:"

    ##               Estimate Std. Error   t value      Pr(>|t|)
    ## (Intercept) -0.5160127 0.11393639 -4.528954  7.501934e-06
    ## MysBiom      0.8539635 0.02995537 28.507858 5.211604e-105

    ## [1] "Adjusted r^2:"

    ## [1] 0.629859

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

#### *Some other observations:*

  - *There are some points that show up clearly in the plots of
    transformed data where 0 mysids were caught in the zooplankton net
    while at least 1 mysid was caught in the mysid net.*
      - *These points behave nicely with the rest of the data,
        especially in the 4th-root case.*
      - *We also no longer intend to convert the zooplankton net values
        to predicted ‘mysid’ net values for the subsequent analyses, so
        these 0’s in the zooplankton net become unconcerning for those
        portions.*
  - *The fact that 4th-root transformed variables turn out to be just an
    offset of each other is good for subsequent GAM analyses.*
      - *When we tranform the data later on for the GAM, and include
        collection procedure (`ColPrcdr`) as a non-smoothed additive
        categorical predictor, this should work out well.*
      - *It may make sense at that time to try square-root or 4th-root
        tranformation rather than natural log.*
          - *For the present data (with `Zoop...` on ‘Y’ and `Mys...` on
            ‘X’), we find that 4th-root performs best.*
          - *When we had previously intended to predict mysid net
            catches from zooplankton net catches, we had modeled these
            data with `Mys...` on ‘Y’ and `Zoop...` on ‘X’). In that
            case, natural log transformation worked best. Perhaps
            square-root (which is somewhere in-between in degree of
            transformation) would be best for the GAM model. Just take a
            look at what makes the most sense when it comes to that
            point.*

<br>

# 2\. Upload Remaining Annual Monitoring Data And Compile Into Single Tibble Object

### Load GLNPO Mysid Net Data

Mysid net data exist in GLNPO Zoop Net tibble dataframe `GLNPO`, but in
that tibble they are organized by zooplankton net tow ID, rather than
mysid net tow ID. Some mysid tows are taken even when zoop tows are not.
I.e.:

1.  At summer benthic stations (from which zooplankton are not collected
    but mysids often are), or  
2.  During the occasional station with weather safety concerns that halt
    sampling operations in-between mysid and zooplaknton net tows. *This
    latter case has happened at least once during a CSMI survey, but has
    perhaps never occurred during a GLNPO survey*

Therefore, a seperate mysid-net-tow specific tibble is needed to
represent that dataset.

``` r
GLNPO_Mysid_Nets =
  read.csv("GLNPO_Mysis_DB_Step5_OutputByStation_20200615.csv") %>% 
  as_tibble %>% 
  filter(regexpr("CSMI", Survey)<0 & regexpr("Teach", Survey)<0) %>% 
  mutate(DepthZone = cut(Station_Depth_m, c(0, 30, 70, 400),labels = c("Nearshore", "Mid-depth", "Offshore"))) %>% 
  select(Visit = Visit_ID, Lake, Year = Year_, Date = Date_Visit, Season, Station, StationDepth = Station_Depth_m, DepthZone, Dens = AvgDens, Biom = AvgBmss_mg, DN = DayNite, TimeEDT = Sam_Vis_EDT) %>% 
  modify_at(c("Lake","Season", "DN"), function(field){factor(field,ordered=F)}) %>%
  modify_at(c("Date"), as.Date)

GLNPO_Mysid_Nets =
  GLNPO_Mysid_Nets %>% 
  modify_at("TimeEDT", function(time) {paste(GLNPO_Mysid_Nets$Date, time, sep = " ")}) %>%
  modify_at("TimeEDT", function(time) {strptime(time, format = "%Y-%m-%d %H:%M", tz = "EST5EDT")})
```

#### Examine `GLNPO_Mysid_Nets` tibble

    ## # A tibble: 538 x 12
    ##    Visit Lake   Year Date       Season Station StationDepth DepthZone  Dens
    ##    <chr> <fct> <int> <date>     <fct>  <chr>          <dbl> <fct>     <dbl>
    ##  1 06GB~ Huron  2006 2006-08-06 Summer HU54_H~         80   Offshore  71.3 
    ##  2 E009~ Erie   2019 2019-04-23 Spring ER09            50   Mid-depth  0   
    ##  3 E009~ Erie   2008 2008-08-11 Summer ER09            48.6 Mid-depth  3.5 
    ##  4 E009~ Erie   2009 2009-08-19 Summer ER09            48.6 Mid-depth  0   
    ##  5 E009~ Erie   2010 2010-08-11 Summer ER09            48.6 Mid-depth  0   
    ##  6 E009~ Erie   2015 2015-08-13 Summer ER09            46   Mid-depth 10.8 
    ##  7 E009~ Erie   2018 2018-08-13 Summer ER09            49   Mid-depth 18.5 
    ##  8 E009~ Erie   2019 2019-08-11 Summer ER09            56   Mid-depth 24.2 
    ##  9 E009~ Erie   2014 2014-04-25 Spring ER09            49.1 Mid-depth  0   
    ## 10 E009~ Erie   2018 2018-04-09 Spring ER09            50   Mid-depth  2.55
    ## # ... with 528 more rows, and 3 more variables: Biom <dbl>, DN <fct>,
    ## #   TimeEDT <dttm>

### Load NOAA GLERL Muskegon Mysid Data

``` r
# NOAA GLERL Data

NOAA_Biom = 
  read.csv("NOAA_MYSIS 2007-2019_Toby_Lengths.csv") %>% 
  as_tibble() %>% 
  rename(SL_mm = SL_ml) %>% 
  mutate(Visit = paste(Date, Station, sep = "_"), IndMass_mg = exp(-12.27 + 2.72 * log(SL_mm)) * 1000) %>% 
  group_by(Visit) %>% 
  summarize(AvgIndMass_mg = mean(IndMass_mg, na.rm = T))

NOAA =
  read.csv("NOAA_MYSIS 2007-2019_Toby_Densities.csv") %>% 
  as_tibble() %>% 
  mutate(Visit = paste(Date, Station, sep = "_")) %>% 
  group_by(Visit, year, Date, Station) %>% 
  summarize(Dens = mean(X..m2, na.rm=T)) %>% 
  mutate(Lake = factor("Michigan", levels = levels(GLNPO$Lake)),
         Date = as.Date(Date), StationDepth = as.numeric(substr(Station,2,4)),
         DN = factor("Night",levels = c("Night", "Day")),
         Biom = NA,
         TimeEDT = NA) %>% 
  mutate(Month = as.factor(months.Date(Date)),
         Season_Q = as.factor(recode(quarters.Date(Date), Q2 = "Spring", Q3 = "Summer", Q4 = "Fall", Q1 = "Winter")),
         DepthZone = cut(StationDepth, c(0, 30, 70, 400),labels = c("Nearshore", "Mid-depth", "Offshore"))) %>% 
  select(Visit, Lake, Year = year, Date, Month, Season = Season_Q, Station = Station, StationDepth, DepthZone, Dens, Biom, DN, TimeEDT) %>% 
  full_join(NOAA_Biom %>% select(Visit, AvgIndMass_mg), by = c("Visit")) %>% 
  mutate(Biom = AvgIndMass_mg * Dens) %>% 
  select(-AvgIndMass_mg)
```

#### Examine `NOAA` tibble

    ## # A tibble: 223 x 13
    ## # Groups:   Visit, Year, Date [223]
    ##    Visit Lake   Year Date       Month Season Station StationDepth DepthZone
    ##    <chr> <fct> <int> <date>     <fct> <fct>  <chr>          <dbl> <fct>    
    ##  1 2007~ Mich~  2007 2007-03-20 March Winter M110             110 Offshore 
    ##  2 2007~ Mich~  2007 2007-04-09 April Spring M110             110 Offshore 
    ##  3 2007~ Mich~  2007 2007-04-09 April Spring M45               45 Mid-depth
    ##  4 2007~ Mich~  2007 2007-05-10 May   Spring M110             110 Offshore 
    ##  5 2007~ Mich~  2007 2007-05-10 May   Spring M45               45 Mid-depth
    ##  6 2007~ Mich~  2007 2007-06-25 June  Spring M110             110 Offshore 
    ##  7 2007~ Mich~  2007 2007-06-25 June  Spring M45               45 Mid-depth
    ##  8 2007~ Mich~  2007 2007-07-29 July  Summer M110             110 Offshore 
    ##  9 2007~ Mich~  2007 2007-07-29 July  Summer M45               45 Mid-depth
    ## 10 2007~ Mich~  2007 2007-08-13 Augu~ Summer M110             110 Offshore 
    ## # ... with 213 more rows, and 4 more variables: Dens <dbl>, Biom <dbl>,
    ## #   DN <fct>, TimeEDT <lgl>

### Load USGS GLSC Mysid Net Data From Annual Acoustic Surveys in Lakes Michigan and Huron

``` r
# USGS Michigan and Huron
USGS_MI_HU =
  read.csv("USGS_Michigan_Huron_2005_2019_Mysis_op_and_density_tjh_20200604.csv") %>% 
  as_tibble() %>% 
  mutate(Visit = paste(substr(Lake, 1, 1), as.character(OP_DATE), formatC(round(depth,0), width = 3, flag = "0"), sep = "_")) %>% 
  mutate(OP_DATE = as.Date(OP_DATE)) %>%
  filter(OP_DATE < as.Date("2012-01-01") | OP_DATE > as.Date("2012-09-01") | Lake == "Michigan") %>% # Filter out Huron 2012 CSMI Surveys
  filter(OP_DATE < as.Date("2017-01-01") | OP_DATE > as.Date("2017-09-01") | Lake == "Michigan") %>% # Filter out Huron 2017 CSMI Surveys
  filter((OP_DATE < as.Date("2010-01-01") | (OP_DATE > as.Date("2010-08-01") & OP_DATE < as.Date("2010-09-15")) | OP_DATE > as.Date("2011-01-01")) | Lake == "Huron") %>% # Filter out Michigan 2010 CSMI Surveys
  filter(OP_DATE < as.Date("2015-01-01") | (OP_DATE > as.Date("2015-08-01") & OP_DATE < as.Date("2015-09-15")) | OP_DATE > as.Date("2016-01-01") | Lake == "Huron") %>%  # Filter out Michigan 2010 CSMI Surveys
  arrange(OP_ID)

USGS_MI_HU =
  USGS_MI_HU %>% 
  mutate(OP_TIME = strptime(paste(OP_DATE, OP_TIME, sep = " "), format = "%Y-%m-%d %H:%M", tz = "EST5EDT"))

# Check for proper exclusion of CSMI survey visits
USGS_MI_HU %>% 
  ggplot(mapping = aes(y = Latitude, x = Longitude)) +
  geom_point(color = 1, pch = 4, data = read.csv(
    "USGS_Michigan_Huron_2005_2019_Mysis_op_and_density_tjh_20200604.csv") %>% 
      as_tibble() %>% 
      mutate(OP_DATE = as.Date(OP_DATE)) %>%
      filter(xor(YEAR %in% c(2012, 2017) & Lake == "Huron", YEAR %in% c(2010, 2015) & Lake == "Michigan"))) +
  geom_point(mapping = aes(color = Lake), pch = 15) +
  facet_wrap(~YEAR)
```

    ## Warning: Removed 22 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/USGS%20Mysid%20Data%20Upload-1.png)<!-- -->

``` r
DupVisits = (USGS_MI_HU %>% filter(duplicated(Visit)))$Visit

View(USGS_MI_HU %>% filter(Visit %in% DupVisits))

USGS_MI_HU %>% filter(Visit %in% DupVisits, Lake == "Michigan") %>% 
  ggplot(mapping = aes(y = jitter(Latitude,50), x = jitter(Longitude,50))) +
  geom_point(pch=1)
```

    ## Warning: Removed 18 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/USGS%20Mysid%20Data%20Upload-2.png)<!-- -->

``` r
USGS_MI_HU %>% filter(Visit %in% DupVisits, Lake == "Huron") %>% 
  ggplot(mapping = aes(y = jitter(Latitude,50), x = jitter(Longitude,50))) +
  geom_point(pch=1)
```

![](GLNPO_Long_term_2019_files/figure-gfm/USGS%20Mysid%20Data%20Upload-3.png)<!-- -->

``` r
USGS_MI_HU %>% filter(Visit %in% DupVisits) %>% 
  ggplot(mapping = aes(y = jitter(depth), x = OP_ID)) +
  geom_point(pch = 1) +
  facet_grid(~Lake+YEAR)
```

![](GLNPO_Long_term_2019_files/figure-gfm/USGS%20Mysid%20Data%20Upload-4.png)<!-- -->

``` r
# Determine visits and replicates
USGS_Diffs = 
  USGS_MI_HU %>% 
  select(OP_ID, VESSEL, SERIAL, OP_TIME, REPLICATE, depth, Latitude, Longitude, OP_DATE) %>% 
  modify(function(Vector){Vector - lag(Vector)}) %>% 
  mutate(Visit_SameAsPrevious = replace(rep(F,nrow(USGS_MI_HU)), REPLICATE == 1, T))

units(USGS_Diffs$OP_TIME) = "mins"

USGS_Diffs
```

    ## # A tibble: 465 x 10
    ##    OP_ID VESSEL SERIAL OP_TIME REPLICATE depth Latitude Longitude OP_DATE
    ##    <int>  <int>  <int> <drtn>      <int> <dbl>    <dbl>     <dbl> <drtn> 
    ##  1    NA     NA     NA   NA m~        NA    NA  NA        NA      NA days
    ##  2     4      0      2 1440 m~         0    63  -0.173    -0.743   1 days
    ##  3     2      0      1  102 m~         0   -52   0.005    -0.157   0 days
    ##  4     4      0      2 1380 m~         0    19  -0.368    -0.437   1 days
    ##  5     1      0      1 1513 m~         0    84  -0.0317    0.128   1 days
    ##  6     3      0      1 1383 m~         0    26  -0.212    -0.0650  1 days
    ##  7    10      0      6 2839 m~         0     6  -0.598    -0.565   2 days
    ##  8     5      0      3  376 m~         0   -85  -0.437    -0.292   1 days
    ##  9     2      0      1 1022 m~         0    27  -0.187     0.293   0 days
    ## 10     3      0      2  283 m~         0    14   0         0.398   1 days
    ## # ... with 455 more rows, and 1 more variable: Visit_SameAsPrevious <lgl>

#### Examine `USGS_MI_HU` tibble

<br>

# 3\. Compare Values Among Lakes

<br>

# 4\. Examine Time Series Trends in Each Lake

<br> <br>

-----

# End of Script.

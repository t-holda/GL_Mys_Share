Great Lakes Mysid Abundance Trends 1997-2019
================

#### Load package libraries

``` r
# In order of occurrence

library(tidyverse) # for tidyverse functions and pipe operator

library(knitr) # for kable() function

library(skimr) # for skim() function

library(geosphere) # for maps()

library(mgcv) # for gam() functions

library(MuMIn) # for AICc() function
```

# 1\. Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

### Setup data

#### Load data and modify as necessary

#### Examine `GLNPO` tibble

    ## Warning: Couldn't find skimmers for class: POSIXlt, POSIXt; No user-defined
    ## `sfl` provided. Falling back to `character`.
    
    ## Warning: Couldn't find skimmers for class: POSIXlt, POSIXt; No user-defined
    ## `sfl` provided. Falling back to `character`.

|                                                  |       |
| :----------------------------------------------- | :---- |
| Name                                             | GLNPO |
| Number of rows                                   | 3965  |
| Number of columns                                | 17    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 4     |
| Date                                             | 1     |
| factor                                           | 5     |
| logical                                          | 1     |
| numeric                                          | 6     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate |   min |   max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | ----: | ----: | ----: | --------: | ---------: |
| Visit          |          0 |           1.00 |     4 |    11 |     0 |      3135 |          0 |
| Station        |          0 |           1.00 |     4 |     5 |     0 |        83 |          0 |
| ZoopTimeEDT    |          1 |           1.00 | 11919 | 27808 |     0 |      3208 |          0 |
| MysTimeEDT     |       3406 |           0.14 | 15331 | 17572 |     0 |       466 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 1997-08-02 | 2019-08-25 | 2008-04-20 |       757 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                              |
| :------------- | ---------: | -------------: | :------ | --------: | :--------------------------------------- |
| Lake           |          0 |              1 | FALSE   |         5 | Eri: 1044, Sup: 1008, Hur: 788, Mic: 662 |
| Season         |          0 |              1 | FALSE   |         2 | Sum: 2136, Spr: 1829                     |
| DepthZone      |          0 |              1 | FALSE   |         3 | Off: 2613, Nea: 824, Mid: 528            |
| ZoopDN         |          0 |              1 | FALSE   |         4 | Day: 2319, Nig: 1284, Twi: 357, 0: 5     |
| MysDN          |          0 |              1 | FALSE   |         4 | \#N/: 3405, Nig: 539, Twi: 11, Day: 10   |

**Variable type: logical**

| skim\_variable | n\_missing | complete\_rate | mean | count               |
| :------------- | ---------: | -------------: | ---: | :------------------ |
| Regress        |          0 |              1 | 0.12 | FAL: 3487, TRU: 478 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |   p0 |     p25 |     p50 |     p75 |    p100 | hist  |
| :------------- | ---------: | -------------: | ------: | -----: | ---: | ------: | ------: | ------: | ------: | :---- |
| Year           |          0 |           1.00 | 2007.87 |   6.26 | 1997 | 2003.00 | 2008.00 | 2013.00 | 2019.00 | ▆▆▇▅▆ |
| StationDepth   |          0 |           1.00 |  113.14 |  75.07 |    5 |   49.00 |  104.00 |  160.00 |  308.00 | ▇▆▆▃▁ |
| ZoopDens       |        308 |           0.92 |   39.43 | 102.08 |    0 |    0.00 |    0.00 |   24.27 | 1314.06 | ▇▁▁▁▁ |
| MysDens        |       3405 |           0.14 |  129.05 | 146.46 |    0 |   22.12 |   90.58 |  189.75 | 1135.50 | ▇▁▁▁▁ |
| ZoopBiom       |        358 |           0.91 |   58.78 | 216.17 |    0 |    0.00 |    0.00 |   11.84 | 3457.38 | ▇▁▁▁▁ |
| MysBiom        |       3405 |           0.14 |  278.29 | 308.03 |    0 |   45.63 |  196.56 |  416.70 | 2137.18 | ▇▂▁▁▁ |

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
    ##    Visit    Lake   Year Date       Season Station   StationDepth DepthZone  Dens
    ##    <chr>    <fct> <int> <date>     <fct>  <chr>            <dbl> <fct>     <dbl>
    ##  1 06GB50M5 Huron  2006 2006-08-06 Summer HU54_HU53         80   Offshore  71.3 
    ##  2 E009A19  Erie   2019 2019-04-23 Spring ER09              50   Mid-depth  0   
    ##  3 E009G08  Erie   2008 2008-08-11 Summer ER09              48.6 Mid-depth  3.5 
    ##  4 E009G09  Erie   2009 2009-08-19 Summer ER09              48.6 Mid-depth  0   
    ##  5 E009G10  Erie   2010 2010-08-11 Summer ER09              48.6 Mid-depth  0   
    ##  6 E009G15  Erie   2015 2015-08-13 Summer ER09              46   Mid-depth 10.8 
    ##  7 E009G18  Erie   2018 2018-08-13 Summer ER09              49   Mid-depth 18.5 
    ##  8 E009G19  Erie   2019 2019-08-11 Summer ER09              56   Mid-depth 24.2 
    ##  9 E009M14  Erie   2014 2014-04-25 Spring ER09              49.1 Mid-depth  0   
    ## 10 E009M18  Erie   2018 2018-04-09 Spring ER09              50   Mid-depth  2.55
    ##      Biom DN       TimeEDT            
    ##     <dbl> <fct>    <dttm>             
    ##  1 188.   Night    2006-08-06 03:51:00
    ##  2   0    Night    2019-04-23 01:51:00
    ##  3   1.11 Night    2008-08-11 00:01:00
    ##  4   0    Night    2009-08-19 00:01:00
    ##  5   0    Night    2010-08-11 05:00:00
    ##  6   3.43 Night    2015-08-13 01:42:00
    ##  7   4.10 Night    2018-08-13 00:39:00
    ##  8   3.72 Twilight 2019-08-11 06:01:00
    ##  9   0    Night    2014-04-25 02:12:00
    ## 10  16.6  Night    2018-04-09 22:15:00
    ## # ... with 528 more rows

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
    ##    Visit           Lake      Year Date       Month  Season Station StationDepth
    ##    <chr>           <fct>    <int> <date>     <fct>  <fct>  <chr>          <dbl>
    ##  1 2007-03-20_M110 Michigan  2007 2007-03-20 March  Winter M110             110
    ##  2 2007-04-09_M110 Michigan  2007 2007-04-09 April  Spring M110             110
    ##  3 2007-04-09_M45  Michigan  2007 2007-04-09 April  Spring M45               45
    ##  4 2007-05-10_M110 Michigan  2007 2007-05-10 May    Spring M110             110
    ##  5 2007-05-10_M45  Michigan  2007 2007-05-10 May    Spring M45               45
    ##  6 2007-06-25_M110 Michigan  2007 2007-06-25 June   Spring M110             110
    ##  7 2007-06-25_M45  Michigan  2007 2007-06-25 June   Spring M45               45
    ##  8 2007-07-29_M110 Michigan  2007 2007-07-29 July   Summer M110             110
    ##  9 2007-07-29_M45  Michigan  2007 2007-07-29 July   Summer M45               45
    ## 10 2007-08-13_M110 Michigan  2007 2007-08-13 August Summer M110             110
    ##    DepthZone  Dens  Biom DN    TimeEDT
    ##    <fct>     <dbl> <dbl> <fct> <lgl>  
    ##  1 Offshore  31.3   69.9 Night NA     
    ##  2 Offshore  28.3  106.  Night NA     
    ##  3 Mid-depth 15.3   26.4 Night NA     
    ##  4 Offshore  29.7   41.4 Night NA     
    ##  5 Mid-depth 45.3  104.  Night NA     
    ##  6 Offshore  53.3  136.  Night NA     
    ##  7 Mid-depth 17.7   21.7 Night NA     
    ##  8 Offshore  50.7  224.  Night NA     
    ##  9 Mid-depth  9.33  11.1 Night NA     
    ## 10 Offshore  37.7  137.  Night NA     
    ## # ... with 213 more rows

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
  # arrange(OP_ID)
  arrange(YEAR, VESSEL, OP_DATE, OP_TIME)

USGS_MI_HU =
  USGS_MI_HU %>% 
  mutate(OP_TIME = strptime(paste(OP_DATE, OP_TIME, sep = " "), format = "%Y-%m-%d %H:%M", tz = "EST5EDT"))

# # Check for proper exclusion of CSMI survey visits
# USGS_MI_HU %>% 
#   ggplot(mapping = aes(y = Latitude, x = Longitude)) +
#   geom_point(color = 1, pch = 4, data = read.csv(
#     "USGS_Michigan_Huron_2005_2019_Mysis_op_and_density_tjh_20200604.csv") %>% 
#       as_tibble() %>% 
#       mutate(OP_DATE = as.Date(OP_DATE)) %>%
#       filter(xor(YEAR %in% c(2012, 2017) & Lake == "Huron", YEAR %in% c(2010, 2015) & Lake == "Michigan"))) +
#   geom_point(mapping = aes(color = Lake), pch = 15) +
#   facet_wrap(~YEAR)
```

``` r
USGS_Diffs = 
  USGS_MI_HU %>% 
  select(YEAR, OP_ID, VESSEL, LAKE_USGS, SERIAL, OP_TIME, REPLICATE, depth, OP_DATE) %>% 
  modify(function(Vector){Vector - lag(Vector)}) %>% 
  mutate(Visit_SameAsPrevious = replace(rep(F,nrow(USGS_MI_HU)), REPLICATE == 1, T)) %>% 
  filter(YEAR >= 0) %>% 
  mutate(Distance_km = distVincentyEllipsoid(
    matrix(c( USGS_MI_HU$Longitude, USGS_MI_HU$Latitude
    ), ncol = 2)
  ) / 1000)

units(USGS_Diffs$OP_TIME) = "mins"

USGS_Diffs$RowID = 1:(dim(USGS_Diffs)[1])

USGS_Diffs$Visit_RepNoted = FALSE
USGS_Diffs$Visit_RepNoted[USGS_Diffs$Visit_SameAsPrevious] = TRUE

USGS_Diffs$Visit_Vessel_Same = TRUE
USGS_Diffs$Visit_Vessel_Same[USGS_Diffs$VESSEL != 0] = FALSE

USGS_Diffs$Visit_Lake_Same = TRUE
USGS_Diffs$Visit_Lake_Same[USGS_Diffs$LAKE_USGS != 0] = FALSE

USGS_Diffs$Visit_Year_Same = TRUE
USGS_Diffs$Visit_Year_Same[USGS_Diffs$YEAR > 0] = FALSE

USGS_Diffs$Visit_Time_within_40 = TRUE
USGS_Diffs$Visit_Time_within_40[USGS_Diffs$OP_TIME > 40] = FALSE

USGS_Diffs$Visit_Depth_within_25 = TRUE
USGS_Diffs$Visit_Depth_within_25[abs(USGS_Diffs$depth) > 25] = FALSE

USGS_Diffs$Visit_Distance_within_40 = TRUE
USGS_Diffs$Visit_Distance_within_40[USGS_Diffs$Distance_km > 40] = FALSE

USGS_Diffs <-
  USGS_Diffs %>% 
  select(everything(), Visit_SameAsNext = Visit_SameAsPrevious)

USGS_Diffs <-
  USGS_Diffs %>% 
  mutate(Visit_SameAsNext = FALSE) %>% 
  mutate(Visit_SameAsNext = 
           Visit_RepNoted | (
             Visit_Time_within_40 &
               Visit_Distance_within_40 &
               Visit_Depth_within_25 &
               Visit_Year_Same &
               Visit_Lake_Same &
               Visit_Vessel_Same
           ))

(RepRows <- 
  USGS_Diffs %>% 
  filter(Visit_SameAsNext) %>% 
  select(RowID) %>% 
  .$RowID)

(RepOPs <- 
  USGS_MI_HU[
    RepRows + 1, ] %>% 
  .$OP_ID)

(FirstOPs <- 
    USGS_MI_HU[
      RepRows[
        c(diff(RepRows) > 1, TRUE)
      ], ] %>% 
    .$OP_ID)

USGS_MI_HU2 <-
  USGS_MI_HU %>%
  mutate(SubsequentRepATvisit = ifelse(OP_ID %in% RepOPs, TRUE, FALSE)) %>% 
  mutate(OPifFIRSTorONLYrepATvisit = ifelse(
    OP_ID %in% c(FirstOPs), OP_ID, ifelse(
      SubsequentRepATvisit, 0, OP_ID
    ))) %>% 
  mutate(OPofFIRSTorONLYrepATvisit = cumsum(OPifFIRSTorONLYrepATvisit)) %>% 
  select(Visit_OP = OPofFIRSTorONLYrepATvisit, everything())

USGS_MI_HU3 <-
  USGS_MI_HU2 %>%
  select(-Visit, -SubsequentRepATvisit, -OPifFIRSTorONLYrepATvisit)
  
(USGS <- 
  USGS_MI_HU3 %>% 
  group_by(Visit_OP, YEAR, Lake, VESSEL) %>% 
  summarize(Date = first(OP_DATE), Time = first(OP_TIME), Month = first(MONTH), 
            Lat = mean(Latitude), Lon = mean(Longitude), StationDepth = mean(depth), Dens = mean(density), 
            N_Col = sum(N), N_Meas = sum(LF_N)) %>% 
    ungroup() %>% 
    mutate(Season = ifelse(Lake == "Huron", "Late Summer", "Summer"), DN = "Night", Station = NA, Biom = NA, 
           DepthZone = cut(StationDepth, c(0, 30, 70, 400), labels = c(
             "Nearshore", "Mid-depth", "Offshore"
           ))))

rm(USGS_MI_HU2, USGS_MI_HU3)

str(USGS)
```

#### Examine `USGS` tibble

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | USGS |
| Number of rows                                   | 414  |
| Number of columns                                | 18   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 3    |
| Date                                             | 1    |
| factor                                           | 1    |
| logical                                          | 2    |
| numeric                                          | 10   |
| POSIXct                                          | 1    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| Lake           |          0 |              1 |   5 |   8 |     0 |         2 |          0 |
| Season         |          0 |              1 |   6 |  11 |     0 |         2 |          0 |
| DN             |          0 |              1 |   5 |   5 |     0 |         1 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 2005-08-18 | 2019-10-09 | 2013-08-13 |       304 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                 |
| :------------- | ---------: | -------------: | :------ | --------: | :-------------------------- |
| DepthZone      |          0 |              1 | FALSE   |         3 | Off: 235, Mid: 151, Nea: 28 |

**Variable type: logical**

| skim\_variable | n\_missing | complete\_rate | mean | count |
| :------------- | ---------: | -------------: | ---: | :---- |
| Station        |        414 |              0 |   \- | :     |
| Biom           |        414 |              0 |   \- | :     |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |        mean |          sd |       p0 |        p25 |         p50 |         p75 |        p100 | hist  |
| :------------- | ---------: | -------------: | ----------: | ----------: | -------: | ---------: | ----------: | ----------: | ----------: | :---- |
| Visit\_OP      |          0 |           1.00 | 15960469.85 | 10371132.05 | 58034.00 | 6756216.25 | 15162026.50 | 24590643.25 | 35597729.00 | ▇▆▆▅▅ |
| YEAR           |          0 |           1.00 |     2012.08 |        4.65 |  2005.00 |    2007.00 |     2013.00 |     2016.00 |     2019.00 | ▇▅▅▆▇ |
| VESSEL         |          0 |           1.00 |       84.84 |       15.30 |    11.00 |      88.00 |       88.00 |       88.00 |       88.00 | ▁▁▁▁▇ |
| Month          |          0 |           1.00 |        8.64 |        0.67 |     8.00 |       8.00 |        9.00 |        9.00 |       10.00 | ▇▁▇▁▂ |
| Lat            |         15 |           0.96 |       44.37 |        1.08 |    41.65 |      43.55 |       44.48 |       45.26 |       46.22 | ▂▅▇▇▇ |
| Lon            |         15 |           0.96 |     \-84.44 |        2.52 |  \-87.80 |    \-87.12 |     \-83.48 |     \-82.18 |     \-80.14 | ▇▁▂▆▂ |
| StationDepth   |          0 |           1.00 |       85.29 |       43.91 |    14.00 |      54.00 |       79.15 |      107.90 |      264.90 | ▇▇▃▁▁ |
| Dens           |          0 |           1.00 |       95.05 |      126.85 |     0.00 |      17.83 |       43.31 |      130.41 |      871.34 | ▇▁▁▁▁ |
| N\_Col         |          0 |           1.00 |       84.52 |      122.27 |     0.00 |      16.00 |       37.00 |      109.50 |     1326.00 | ▇▁▁▁▁ |
| N\_Meas        |          0 |           1.00 |       31.51 |       45.03 |     0.00 |       0.00 |       17.00 |       44.00 |      506.00 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
| :------------- | ---------: | -------------: | :------------------ | :------------------ | :------------------ | --------: |
| Time           |          0 |              1 | 2005-08-18 22:00:00 | 2019-10-09 22:13:00 | 2013-08-13 11:56:00 |       413 |

#### Upload DFO data into `DFO` tibble

``` r
# Add NOAA data from Historic Published studies by Pothoven...

DFO <- 
  read_csv("Historic_Published_Mysid_Data_Densities_20200609.csv")

DFO %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(Lake == "Ontario") %>% 
  count(reference)

DFO %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(reference %in% c(
    "Johannsson et al. 2011"
  ))

DFO %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(reference %in% c(
    "Rudstam et al. 2017 (GLFC)"
  ))

# `Rudstam et al. 2017 (GLFC)` has same data for more years, and includes 2 depth zones + `SE` values.
# Two notes: 50-100 m zone includes both samples less than 70 m and samples more than 70 m.
# Expecting to get DFO data by sample or by visit - that will be better.

DFO <- 
  DFO %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(reference %in% c(
    "Rudstam et al. 2017 (GLFC)"
  ))

DFO <- 
  DFO %>% 
  mutate(Visit = NA, Season = "Fall", StationDepth = mean(c(minDepth, maxDepth)), DepthZone = "Offshore", Biom = NA, DN = "Night", TimeEDT = NA) %>% 
  select(Visit, Lake, Year = StartYear, Date, Month = month, Season, Station = StationName, StationDepth, DepthZone, Dens = DensPerm2, Biom, DN, TimeEDT)
```

#### Examine `DFO` tibble

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | DFO  |
| Number of rows                                   | 41   |
| Number of columns                                | 13   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 6    |
| Date                                             | 1    |
| logical                                          | 3    |
| numeric                                          | 3    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| Lake           |          0 |              1 |   7 |   7 |     0 |         1 |          0 |
| Month          |          0 |              1 |   7 |   7 |     0 |         1 |          0 |
| Season         |          0 |              1 |   4 |   4 |     0 |         1 |          0 |
| Station        |         41 |              0 |  \- |  \- |     0 |         0 |          0 |
| DepthZone      |          0 |              1 |   8 |   8 |     0 |         1 |          0 |
| DN             |          0 |              1 |   5 |   5 |     0 |         1 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 2002-11-01 | 2016-11-01 | 2008-11-01 |        14 |

**Variable type: logical**

| skim\_variable | n\_missing | complete\_rate | mean | count |
| :------------- | ---------: | -------------: | ---: | :---- |
| Visit          |         41 |              0 |   \- | :     |
| Biom           |         41 |              0 |   \- | :     |
| TimeEDT        |         41 |              0 |   \- | :     |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |      p0 |     p25 |     p50 |     p75 |    p100 | hist  |
| :------------- | ---------: | -------------: | ------: | -----: | ------: | ------: | ------: | ------: | ------: | :---- |
| Year           |          0 |              1 | 2008.54 |   4.25 | 2002.00 | 2005.00 | 2008.00 | 2012.00 | 2016.00 | ▇▇▇▇▅ |
| StationDepth   |          0 |              1 |  143.29 |   0.00 |  143.29 |  143.29 |  143.29 |  143.29 |  143.29 | ▁▁▇▁▁ |
| Dens           |          0 |              1 |  209.95 | 108.24 |   26.78 |  100.00 |  200.00 |  299.48 |  403.29 | ▇▅▅▇▆ |

<br>

#### Compile data sources into single tibble

    ## Warning: Couldn't find skimmers for class: POSIXlt, POSIXt; No user-defined
    ## `sfl` provided. Falling back to `character`.

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 845        |
| Number of columns                                | 12         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 3          |
| Date                                             | 1          |
| factor                                           | 4          |
| numeric                                          | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate |  min |  max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | ---: | ---: | ----: | --------: | ---------: |
| Visit          |          0 |              1 |    7 |    7 |     0 |       655 |          0 |
| Station        |          0 |              1 |    4 |    5 |     0 |        50 |          0 |
| TimeEDT        |          1 |              1 | 2541 | 5924 |     0 |       660 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 1997-08-15 | 2019-08-25 | 2007-04-04 |       447 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                            |
| :------------- | ---------: | -------------: | :------ | --------: | :------------------------------------- |
| Lake           |          0 |              1 | FALSE   |         4 | Sup: 323, Mic: 215, Hur: 166, Ont: 141 |
| Season         |          0 |              1 | FALSE   |         2 | Sum: 437, Spr: 408                     |
| DepthZone      |          0 |              1 | FALSE   |         1 | Off: 845, Mid: 0, Nea: 0               |
| DN             |          0 |              1 | FALSE   |         1 | Nig: 845, 0: 0, Day: 0, Twi: 0         |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |   p0 |     p25 |     p50 |     p75 |    p100 | hist  |
| :------------- | ---------: | -------------: | ------: | -----: | ---: | ------: | ------: | ------: | ------: | :---- |
| Year           |          0 |           1.00 | 2007.44 |   6.19 | 1997 | 2002.00 | 2007.00 | 2012.00 | 2019.00 | ▆▇▇▅▆ |
| StationDepth   |          0 |           1.00 |  154.84 |  54.93 |   73 |  107.00 |  148.00 |  193.00 |  308.00 | ▆▇▃▂▁ |
| Dens           |         17 |           0.98 |  137.16 | 160.95 |    0 |   39.21 |   90.90 |  175.87 | 1314.06 | ▇▁▁▁▁ |
| Biom           |         61 |           0.93 |  221.12 | 384.67 |    0 |   35.88 |  106.69 |  233.32 | 3457.38 | ▇▁▁▁▁ |

    ##  [1] "Visit"        "Lake"         "Year"         "Season"       "Date"        
    ##  [6] "Station"      "StationDepth" "DepthZone"    "Dens"         "Biom"        
    ## [11] "DN"           "TimeEDT"

    ##  [1] "Visit"        "Lake"         "Year"         "Date"         "Season"      
    ##  [6] "Station"      "StationDepth" "DepthZone"    "Dens"         "Biom"        
    ## [11] "DN"           "TimeEDT"

    ##  [1] "Visit"        "Lake"         "Year"         "Date"         "Season"      
    ##  [6] "Station"      "StationDepth" "DepthZone"    "Dens"         "Biom"        
    ## [11] "DN"           "TimeEDT"

    ##  [1] "Visit"        "Lake"         "Year"         "Date"         "Season"      
    ##  [6] "Station"      "StationDepth" "DepthZone"    "Dens"         "Biom"        
    ## [11] "DN"           "TimeEDT"

    ##  [1] "Visit"        "Lake"         "Year"         "Date"         "Season"      
    ##  [6] "Station"      "StationDepth" "DepthZone"    "Dens"         "Biom"        
    ## [11] "DN"           "TimeEDT"

#### Examine `DFO` tibble

|                                                  |        |
| :----------------------------------------------- | :----- |
| Name                                             | Mysids |
| Number of rows                                   | 1643   |
| Number of columns                                | 14     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 4      |
| Date                                             | 1      |
| factor                                           | 4      |
| numeric                                          | 4      |
| POSIXct                                          | 1      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| Visit          |         41 |           0.98 |   6 |  15 |     0 |      1077 |          0 |
| Station        |        276 |           0.83 |   3 |  11 |     0 |        62 |          0 |
| DepthZone      |          0 |           1.00 |   8 |   8 |     0 |         1 |          0 |
| DN             |          0 |           1.00 |   5 |   5 |     0 |         1 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 1997-08-15 | 2019-11-19 | 2010-08-19 |       730 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                            |
| :------------- | ---------: | -------------: | :------ | --------: | :------------------------------------- |
| Lake           |          0 |              1 | FALSE   |         4 | Mic: 565, Sup: 485, Hur: 344, Ont: 249 |
| Season         |          0 |              1 | FALSE   |         4 | Sum: 824, Spr: 655, Lat: 91, Fal: 73   |
| Group          |          0 |              1 | FALSE   |         5 | GLN: 845, GLN: 413, USG: 235, NOA: 109 |
| Period         |          0 |              1 | TRUE    |         3 | 200: 988, 199: 389, 201: 266           |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |     p0 |     p25 |     p50 |     p75 |    p100 | hist  |
| :------------- | ---------: | -------------: | ------: | -----: | -----: | ------: | ------: | ------: | ------: | :---- |
| Year           |          0 |           1.00 | 2009.87 |   5.90 | 1997.0 | 2006.00 | 2010.00 | 2015.00 | 2019.00 | ▃▃▇▆▇ |
| StationDepth   |          0 |           1.00 |  144.02 |  52.12 |   70.1 |  102.00 |  139.00 |  165.00 |  308.00 | ▇▇▃▂▁ |
| Dens           |         17 |           0.99 |  134.96 | 149.61 |    0.0 |   38.42 |   92.70 |  181.88 | 1314.06 | ▇▁▁▁▁ |
| Biom           |        337 |           0.79 |  242.06 | 342.03 |    0.0 |   47.55 |  142.34 |  302.68 | 3457.38 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
| :------------- | ---------: | -------------: | :------------------ | :------------------ | :------------------ | --------: |
| TimeEDT        |        153 |           0.91 | 1997-08-15 01:15:00 | 2019-10-03 23:41:00 | 2010-08-04 01:31:00 |      1172 |

<br>

# 3\. Compare Values Among Lakes

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | Lake     |
| :---------- | :----- | --------: | --------: | --------: | :------- |
| GLNPO\_Mys  | Spring |        \- |     109.2 |      50.1 | Michigan |
| GLNPO\_Zoop | Spring |     169.2 |      91.3 |      31.4 | Michigan |
| NOAA        | Spring |        \- |      50.4 |      17.7 | Michigan |
| GLNPO\_Mys  | Summer |        \- |     172.7 |      84.5 | Michigan |
| GLNPO\_Zoop | Summer |     429.1 |     169.6 |      75.3 | Michigan |
| NOAA        | Summer |        \- |      97.5 |      29.0 | Michigan |
| USGS        | Summer |     352.5 |     195.5 |      84.7 | Michigan |
| NOAA        | Fall   |        \- |      79.0 |      28.7 | Michigan |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-1.png)<!-- -->

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | Lake    |
| :---------- | :----- | --------: | --------: | --------: | :------ |
| GLNPO\_Mys  | Spring |        \- |     143.3 |     190.7 | Ontario |
| GLNPO\_Zoop | Spring |      91.1 |     162.9 |     122.8 | Ontario |
| GLNPO\_Mys  | Summer |        \- |     349.7 |     286.4 | Ontario |
| GLNPO\_Zoop | Summer |     207.3 |     268.8 |     225.9 | Ontario |
| DFO         | Fall   |     239.6 |     197.7 |        \- | Ontario |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-2.png)<!-- -->

| Group       | Season      | 1997-2005 | 2006-2016 | 2017-2019 | Lake  |
| :---------- | :---------- | --------: | --------: | --------: | :---- |
| GLNPO\_Mys  | Spring      |        \- |      12.1 |       6.4 | Huron |
| GLNPO\_Zoop | Spring      |      49.3 |       9.5 |       8.7 | Huron |
| GLNPO\_Mys  | Summer      |        \- |      43.1 |      27.5 | Huron |
| GLNPO\_Zoop | Summer      |     127.5 |      31.5 |      13.1 | Huron |
| USGS        | Late Summer |      45.1 |      57.6 |      41.3 | Huron |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-3.png)<!-- -->

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | Lake     |
| :---------- | :----- | --------: | --------: | --------: | :------- |
| GLNPO\_Mys  | Spring |        \- |     115.7 |     119.0 | Superior |
| GLNPO\_Zoop | Spring |     108.2 |      81.7 |      54.8 | Superior |
| GLNPO\_Mys  | Summer |        \- |     218.9 |     213.3 | Superior |
| GLNPO\_Zoop | Summer |     112.6 |     162.6 |     136.5 | Superior |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-4.png)<!-- -->

<br>

# 4\. Examine Time Series Trends in Each Lake

### Plot trends with smoother gams for each season, group and lake.

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-4.png)<!-- -->

### Fit GAM models for each Lake

Looks like Natural-Log transformation will be the best.

For most of the lakes, `Season` effects do not change over time
(`Year`). `Group` effects do change a little bit over time (`Year`). I’m
not sure why. The best models account for this. But the simpler models
that only smooth the `Year` term are not substantially worse off in
terms of AICc.

In Lake Superior, both `Season` and `Group` have significant effects
over time (`Year`), while time (`Year`) on its own does not have a
significant effect. For Lake Superior, `Season` is more important that
`Group`, but `Group` remains important according to AICc.

Based on the plot of data for Lake Superior, these findings are not
suprising. The models that smooth over `Season` and `Group` show
temporal patterns much more similar to what each seasonal dataset
demonstrates on its own (pers. obsv. in previous preliminary analyses).
Perhasp for Lake Superior, we should fit a model with a slightly
different statistical structure.

``` r
Michigan_Dens_GAM
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Estimated degrees of freedom:
    ## 7.75  total = 13.75 
    ## 
    ## REML score: 775.4408

``` r
Ontario_Dens_GAM
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Estimated degrees of freedom:
    ## 2.02  total = 6.02 
    ## 
    ## REML score: 389.2474     rank: 13/14

``` r
Huron_Dens_GAM
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Estimated degrees of freedom:
    ## 5.02  total = 9.02 
    ## 
    ## REML score: 576.4369     rank: 13/14

``` r
Superior_Dens_GAM
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Estimated degrees of freedom:
    ## 2.19  total = 5.19 
    ## 
    ## REML score: 658.2985

<br>

#### Plot fitted GAM models for each lake by season

Plots below are mean value predicted +/- 2 SE.

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-4.png)<!-- -->

…And versions with data plotted for supplementary figure:

    ## Warning: Removed 17 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season-1.png)<!-- -->

<br> <br>

-----

# End of Script.

Great Lakes Mysid Abundance Trends 1997-2019
================

#### Load package libraries

``` r
# In order of occurrence

library(tidyverse) # for tidyverse functions and pipe operator

library(knitr) # for kable() function

# library(skimr) # for skim() function

library(geosphere) # for maps()

library(mgcv) # for gam() functions

library(MuMIn) # for AICc() function
```

# 1\. Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

### Setup data

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

    ## # A tibble: 414 x 18
    ##    Visit_OP  YEAR Lake  VESSEL Date       Time                Month   Lat   Lon
    ##       <dbl> <int> <chr>  <int> <date>     <dttm>              <int> <dbl> <dbl>
    ##  1    58034  2005 Mich~     88 2005-08-18 2005-08-18 22:00:00     8  46.0 -85.3
    ##  2   116072  2005 Mich~     88 2005-08-19 2005-08-19 22:00:00     8  45.8 -86.0
    ##  3   174112  2005 Mich~     88 2005-08-19 2005-08-19 23:42:00     8  45.8 -86.2
    ##  4   232156  2005 Mich~     88 2005-08-20 2005-08-20 22:42:00     8  45.4 -86.6
    ##  5   290201  2005 Mich~     88 2005-08-21 2005-08-21 23:55:00     8  45.4 -86.5
    ##  6   348249  2005 Mich~     88 2005-08-22 2005-08-22 22:58:00     8  45.2 -86.5
    ##  7   406307  2005 Mich~     88 2005-08-24 2005-08-24 22:17:00     8  44.6 -87.1
    ##  8   464370  2005 Mich~     88 2005-08-25 2005-08-25 04:33:00     8  44.2 -87.4
    ##  9   522435  2005 Mich~     88 2005-08-25 2005-08-25 21:35:00     8  44.0 -87.1
    ## 10   580503  2005 Mich~     88 2005-08-26 2005-08-26 02:18:00     8  44.0 -86.7
    ## # ... with 404 more rows, and 9 more variables: StationDepth <dbl>, Dens <dbl>,
    ## #   N_Col <int>, N_Meas <int>, Season <chr>, DN <chr>, Station <lgl>,
    ## #   Biom <lgl>, DepthZone <fct>

#### Upload DFO data into `DFO` tibble

``` r
# Add NOAA data from Historic Published studies by Pothoven...

DFO_NOAA <- 
  read_csv("Historic_Published_Mysid_Data_Densities_20200609.csv")

DFO_NOAA %>% 
  filter(Lake == "Ontario" | StartYear >= 1997) %>% 
  count(reference)

DFO_NOAA %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(reference %in% c(
    "Johannsson et al. 2011"
  ))

DFO_NOAA %>% 
  filter(Lake == "Ontario", StartYear >= 1997) %>% 
  filter(reference %in% c(
    "Rudstam et al. 2017 (GLFC)"
  ))

# `Rudstam et al. 2017 (GLFC)` has same data for more years, and includes 2 depth zones + `SE` values.
# Two notes: 50-100 m zone includes both samples less than 70 m and samples more than 70 m.
# Expecting to get DFO_NOAA data by sample or by visit - that will be better.

DFO_NOAA <- 
  DFO_NOAA %>% 
  filter(Lake %in% c("Michigan", "Ontario"), 
         StartYear >= 1995) %>% 
  filter(Lake == "Michigan" | StartYear >= 1997) %>% 
  filter(Lake == "Michigan" | reference == "Rudstam et al. 2017 (GLFC)")

DFO_NOAA <- 
  DFO_NOAA %>% 
  mutate(Visit = NA, 
         Season = ifelse(Lake == "Ontario", "Fall", NA), 
         StationDepth = (minDepth + maxDepth) / 2, 
         Biom = NA, 
         DN = "Night", 
         TimeEDT = NA) %>% 
  mutate(Season = 
           ifelse(Lake == "Michigan", recode(
             quarters.Date(Date), Q2 = "Spring", Q3 = "Summer", Q4 = "Fall", Q1 = "Winter"
             ), Season),
         DepthZone = ifelse(StationDepth >= 70, "Offshore", "MidDepth")) %>% 
  select(Visit, Lake, Year = StartYear, Date, Month = month, Season, Station = StationName, StationDepth, DepthZone, Dens = DensPerm2, Biom, DN, TimeEDT)
```

#### Examine `DFO_NOAA` tibble

    ## # A tibble: 166 x 13
    ##    Visit Lake   Year Date       Month Season Station StationDepth DepthZone
    ##    <lgl> <chr> <dbl> <date>     <chr> <chr>  <chr>          <dbl> <chr>    
    ##  1 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              45 MidDepth 
    ##  2 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              45 MidDepth 
    ##  3 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              45 MidDepth 
    ##  4 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              75 Offshore 
    ##  5 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              75 Offshore 
    ##  6 NA    Mich~  2000 2000-08-15 Aug   Summer <NA>              75 Offshore 
    ##  7 NA    Mich~  2000 2000-06-15 Jun   Spring <NA>              45 MidDepth 
    ##  8 NA    Mich~  2000 2000-06-15 Jun   Spring <NA>              45 MidDepth 
    ##  9 NA    Mich~  2000 2000-06-15 Jun   Spring <NA>              45 MidDepth 
    ## 10 NA    Mich~  2000 2000-06-15 Jun   Spring <NA>              75 Offshore 
    ## # ... with 156 more rows, and 4 more variables: Dens <dbl>, Biom <lgl>,
    ## #   DN <chr>, TimeEDT <lgl>

<br>

#### Compile data sources into single tibble

``` r
GLNPO_Zoop_Nets <- 
  GLNPO %>% 
  select(Visit:DepthZone, Dens = ZoopDens, Biom = ZoopBiom, DN = ZoopDN, TimeEDT = ZoopTimeEDT) %>% 
  filter(DN == "Night")

GLNPO_Zoop_Nets %>%
filter(is.na(Season) == FALSE,
         DepthZone == "Offshore")

names(GLNPO_Zoop_Nets)
names(GLNPO_Mysid_Nets)
names(NOAA %>% select(-Month))
names(USGS %>% select(Visit = Visit_OP, Lake, Year = YEAR, Date, Season, Station, StationDepth, DepthZone, Dens, Biom, DN, TimeEDT = Time))
names(DFO_NOAA %>% select(-Month))


Mysids <- 
  bind_rows(
    GLNPO_Zoop_Nets %>% 
      mutate(Group = "GLNPO_Zoop"),
    GLNPO_Mysid_Nets %>% 
      filter(DN == "Night") %>% 
      mutate(Group = "GLNPO_Mys"),
    NOAA %>% 
      select(-Month) %>% 
      mutate(Group = "NOAA"),
    USGS %>% 
      select(Visit = Visit_OP, Lake, Year = YEAR, Date, Season, Station, StationDepth, DepthZone, Dens, Biom, DN, TimeEDT = Time) %>% 
      modify_at("Visit", as.character) %>% 
      mutate(Visit = paste("USGS", Visit, sep = "_")) %>% 
      mutate(Group = "USGS"),
    DFO_NOAA %>% 
      select(-Month) %>% 
      mutate(Group = ifelse(Lake == "Michigan", "NOAA", "DFO"))
    ) %>% 
  filter(is.na(Season) == FALSE,
         DepthZone == "Offshore",
         Season != "Winter") %>% 
  mutate(Period = as.character(cut(Year, c(1996,2005.5,2016.5,2020), labels = c("1997-2005", "2006-2016", "2017-2019")))) %>% 
  mutate(Period = ifelse(Lake == "Michigan" & Year < 2006, "1995-2005", Period)) %>%
  # filter(Year >= 1997) %>% 
  modify_at("Group", as.factor) %>% 
  modify_at("Lake", as.factor)

Mysids$Season = factor(as.character(Mysids$Season), levels = c("Spring", "Summer", "Late Summer", "Fall"))

Mysids$Period = factor(as.character(Mysids$Period), levels = c("1995-2005", "1997-2005", "2006-2016", "2017-2019"), ordered = T)
```

#### Examine `Mysids` tibble

    ## # A tibble: 1,705 x 14
    ##    Visit Lake   Year Season Date       Station StationDepth DepthZone  Dens
    ##    <chr> <fct> <dbl> <fct>  <date>     <chr>          <dbl> <chr>     <dbl>
    ##  1 M047~ Mich~  2000 Summer 2000-08-26 MI47             195 Offshore  236. 
    ##  2 M032~ Mich~  2000 Summer 2000-08-27 MI32             164 Offshore  111. 
    ##  3 M18M~ Mich~  2000 Summer 2000-08-27 MI18M            160 Offshore  326. 
    ##  4 M017~ Mich~  2000 Summer 2000-08-28 MI17             100 Offshore   65.4
    ##  5 H45M~ Huron  2000 Summer 2000-08-14 HU45M             95 Offshore  147. 
    ##  6 H038~ Huron  2000 Summer 2000-08-13 HU38             137 Offshore  101. 
    ##  7 H037~ Huron  2000 Summer 2000-08-13 HU37              74 Offshore   60.1
    ##  8 H45M~ Huron  2000 Summer 2000-08-14 HU45M             95 Offshore  153. 
    ##  9 H45M~ Huron  2000 Summer 2000-08-14 HU45M             95 Offshore  169. 
    ## 10 H053~ Huron  2000 Summer 2000-08-15 HU53              91 Offshore   10.9
    ## # ... with 1,695 more rows, and 5 more variables: Biom <dbl>, DN <chr>,
    ## #   TimeEDT <dttm>, Group <fct>, Period <ord>

<br>

# 3\. Compare Values Among Lakes

    ## [1] "Lake Michigan:"

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | 1995-2005 | Lake     |
| :---------- | :----- | --------: | --------: | --------: | --------: | :------- |
| GLNPO\_Mys  | Spring |        \- |     109.2 |      50.1 |        \- | Michigan |
| GLNPO\_Zoop | Spring |        \- |      91.3 |      31.4 |     169.2 | Michigan |
| NOAA        | Spring |        \- |      50.4 |      17.7 |     119.2 | Michigan |
| GLNPO\_Mys  | Summer |        \- |     172.7 |      84.5 |        \- | Michigan |
| GLNPO\_Zoop | Summer |        \- |     169.6 |      75.3 |     429.1 | Michigan |
| NOAA        | Summer |        \- |      97.5 |      29.0 |     214.6 | Michigan |
| USGS        | Summer |        \- |     195.5 |      84.7 |     352.5 | Michigan |
| NOAA        | Fall   |        \- |      79.0 |      28.7 |     118.7 | Michigan |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-1.png)<!-- -->

    ## [1] "Lake Ontario:"

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | 1995-2005 | Lake    |
| :---------- | :----- | --------: | --------: | --------: | --------: | :------ |
| GLNPO\_Mys  | Spring |        \- |     143.3 |     190.7 |        \- | Ontario |
| GLNPO\_Zoop | Spring |      91.1 |     162.9 |     122.8 |        \- | Ontario |
| GLNPO\_Mys  | Summer |        \- |     349.7 |     286.4 |        \- | Ontario |
| GLNPO\_Zoop | Summer |     207.3 |     268.8 |     225.9 |        \- | Ontario |
| DFO         | Fall   |     239.6 |     197.7 |        \- |        \- | Ontario |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-2.png)<!-- -->

    ## [1] "Lake Huron:"

| Group       | Season      | 1997-2005 | 2006-2016 | 2017-2019 | 1995-2005 | Lake  |
| :---------- | :---------- | --------: | --------: | --------: | --------: | :---- |
| GLNPO\_Mys  | Spring      |        \- |      12.1 |       6.4 |        \- | Huron |
| GLNPO\_Zoop | Spring      |      49.3 |       9.5 |       8.7 |        \- | Huron |
| GLNPO\_Mys  | Summer      |        \- |      43.1 |      27.5 |        \- | Huron |
| GLNPO\_Zoop | Summer      |     127.5 |      31.5 |      13.1 |        \- | Huron |
| USGS        | Late Summer |      45.1 |      57.6 |      41.3 |        \- | Huron |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-3.png)<!-- -->

    ## [1] "Lake Superior:"

| Group       | Season | 1997-2005 | 2006-2016 | 2017-2019 | 1995-2005 | Lake     |
| :---------- | :----- | --------: | --------: | --------: | --------: | :------- |
| GLNPO\_Mys  | Spring |        \- |     115.7 |     119.0 |        \- | Superior |
| GLNPO\_Zoop | Spring |     108.2 |      81.7 |      54.8 |        \- | Superior |
| GLNPO\_Mys  | Summer |        \- |     218.9 |     213.3 |        \- | Superior |
| GLNPO\_Zoop | Summer |     112.6 |     162.6 |     136.5 |        \- | Superior |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-4.png)<!-- -->

<br>

# 4\. Examine Time Series Trends in Each Lake

### Plot trends with smoother gams for each season, group and lake.

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-4.png)<!-- -->

    ## [1] "All Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-5.png)<!-- -->

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
summary(Michigan_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.37804    0.10573  41.409  < 2e-16 ***
    ## SeasonSummer     0.76050    0.08927   8.519  < 2e-16 ***
    ## SeasonFall       0.40394    0.16271   2.482   0.0133 *  
    ## GroupGLNPO_Zoop -0.30732    0.12100  -2.540   0.0113 *  
    ## GroupNOAA       -0.57031    0.13144  -4.339 1.68e-05 ***
    ## GroupUSGS       -0.13892    0.13267  -1.047   0.2955    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 7.904  8.665 31.43  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.382   Deviance explained = 39.5%
    ## -REML = 848.25  Scale est. = 0.84151   n = 621

``` r
summary(Ontario_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       5.1593     0.1825  28.266  < 2e-16 ***
    ## SeasonSummer      0.7598     0.1625   4.676 4.87e-06 ***
    ## SeasonFall        0.0000     0.0000      NA       NA    
    ## GroupGLNPO_Mys   -0.5503     0.2485  -2.214 0.027765 *  
    ## GroupGLNPO_Zoop  -0.8025     0.2204  -3.641 0.000333 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value
    ## s(Year) 2.016  2.537 0.684    0.44
    ## 
    ## Rank: 13/14
    ## R-sq.(adj) =  0.104   Deviance explained = 12.2%
    ## -REML = 389.25  Scale est. = 1.3369    n = 247

``` r
summary(Huron_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         2.2590     0.1672  13.508  < 2e-16 ***
    ## SeasonSummer        1.5466     0.1701   9.094  < 2e-16 ***
    ## SeasonLate Summer   1.6785     0.2122   7.910 3.94e-14 ***
    ## GroupGLNPO_Zoop    -0.7953     0.1909  -4.167 3.95e-05 ***
    ## GroupUSGS           0.0000     0.0000      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F  p-value    
    ## s(Year) 5.022  6.115 10.45 7.76e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Rank: 13/14
    ## R-sq.(adj) =  0.379   Deviance explained = 39.4%
    ## -REML = 576.44  Scale est. = 1.6973    n = 338

``` r
summary(Superior_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## LogDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.65083    0.09172  50.709  < 2e-16 ***
    ## SeasonSummer     0.51042    0.08622   5.920 6.17e-09 ***
    ## GroupGLNPO_Zoop -0.50162    0.10006  -5.013 7.55e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value
    ## s(Year) 2.193  2.733 1.277   0.333
    ## 
    ## R-sq.(adj) =  0.133   Deviance explained =   14%
    ## -REML =  658.3  Scale est. = 0.87577   n = 482

<br> <br>

### Plot fitted GAM models for each lake by season

**Need to visualize mysid densities:**

  - In 4 deep lakes

  - Over \~ 20 years (1995/1997 - 2019)

  - In 2 - 3 seasons

  - Collected by 2 - 4 methods/groups

<br>

**Plotting Decisions:**

1.  Axes:
    
      - Y axis will be density
      - X axis will be year

2.  Series
    
      - One for each lake and season
          - Use GAM fit +/- 1 or 2 SE.
      - Do not display a sepearat GAM fit for each method/agency (aka
        `Group`)
          - Have to pick one for `predict()` method
          - Pick GLNPO Mysid Net (`GLNPO_Mys`)
              - Present in each lake
              - Targets Mysids (unlike `GLNPO_Zoop`)
              - 2nd most years present (except for `GLNPO_Zoop` which
                has more years)
          - This means method/agency effect will not be shown in the
            figure
              - It will need to reported / shown elsewhere in the ms

3.  Panels
    
      - Could do one per series, and include points, OR
      - Could do one per Lake or per Season, and exclude points
      - Do One per Season
          - Allows simultaneous visualization of:
              - Each lake’s trends over time series
              - Cross-lake comparisons at any point in time series
              - Comparison between lakes of trends over time series
          - Seasonal differences are less interesting
              - Well studied - consistant differences (likely
                multiplicative)
              - Predictable from 2-yr life cycle
          - Showing individual visits coded to Lake by color would be
            too busy on these plots.
          - Save GAM-corrected individual visits with GAM fits to
            multi-faceted plot for supplemental materials

<br>

#### Plots below are mean value predicted GAM value +/- 1 SE.

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-4.png)<!-- -->

#### …And versions with data plotted for supplementary figure(s):

The plot below shows the same above GAM fits superimposed on
model-adjusted data points. The model adjustment in the data points is
that of re-predicting the values assuming the GLNPO Mysid net was used
to collect the sample. Thus, each point is the predicted GAM value for
the lake, season, and year shown and gvien the `GLNPO_Mys` `Group`
effect, plus the residual associated with that point’s fit to the GAM.

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20-%20GAM%20adjusted%20for%20GLNPO_Mys-1.png)<!-- -->

The below plots show the raw data, color coded by collection group. The
GAM model fits are not plotted with these data.

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20with%20raw%20data-1.png)<!-- -->

<br> <br>

-----

# End of Script.

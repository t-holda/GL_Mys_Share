Great Lakes Mysid Abundance Trends 1997-2019
================

# Contents:

[Load Libraries](#load-package-libraries) [Section 1: Evaluate GLNPO
Zooplankton Net
Data](#1-upload-glnpo-data-and-evaluate-usability-of-zooplankton-net-mysid-catches)  
[…1… Conclusions](#thus)  
[Section 2: Upload Remaining Annual Program
Datasets](#2-upload-remaining-annual-monitoring-data-and-compile-into-single-tibble-object)  
[Section 3: Compare Lakes](#3-compare-values-among-lakes)  
[Section 4: Trends Within
Lakes](#4-examine-time-series-trends-in-each-lake)  
[…4… GAM results
plotted](#plots-below-are-mean-value-predicted-gam-value---1-se)  
[Section 5: Life History
Data](#5-examine-averages-and-trends-in-life-history-rates-in-the-lakes)  
[…5… Fecundity and Brooding Female
Lengths](#key-plots-and-analyses-of-brooding-females-data)  
[…5… Size and Growth](#size-structure-plots)  
[…5… Age Structure and Survival](#survival-estimates)  
[Section 6: Food resources](#6-compare-to-food-resources)

<br>

#### Load package libraries

``` r
# In order of occurrence

library(tidyverse) # for tidyverse functions and pipe operator

library(knitr) # for kable() function

# library(skimr) # for skim() function

library(geosphere) # for maps()

library(lme4)

library(mclust) # for Mclust() function

library(mgcv) # for gam() functions

library(MuMIn) # for AICc() function

library(zoo)
```

<br>

# 1\. Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

### Setup data

#### Load data and modify as necessary

#### Examine `GLNPO` tibble

    ## # A tibble: 1,045 x 17
    ## # Groups:   Visit, Lake, Year, Season [1,045]
    ##    Visit Lake   Year Season Station Date       StationDepth DepthZone ZoopDens
    ##    <chr> <fct> <int> <fct>  <chr>   <date>            <dbl> <fct>        <dbl>
    ##  1 E009~ Erie   2000 Spring ER09    2000-04-16           49 Mid-depth   NaN   
    ##  2 E009~ Erie   2000 Summer ER09    2000-08-04           30 Mid-depth     0   
    ##  3 E009~ Erie   2001 Summer ER09    2001-08-05           51 Mid-depth    15.2 
    ##  4 E009~ Erie   2002 Summer ER09    2002-08-08           48 Mid-depth     0   
    ##  5 E009~ Erie   2008 Summer ER09    2008-08-11           49 Mid-depth     6.79
    ##  6 E009~ Erie   2009 Summer ER09    2009-08-19           49 Mid-depth     0   
    ##  7 E009~ Erie   2010 Summer ER09    2010-08-11           49 Mid-depth     0   
    ##  8 E009~ Erie   2014 Summer ER09    2014-08-15           49 Mid-depth     0   
    ##  9 E009~ Erie   2015 Summer ER09    2015-08-13           49 Mid-depth    10.1 
    ## 10 E009~ Erie   2018 Summer ER09    2018-08-13           49 Mid-depth    19.0 
    ## # ... with 1,035 more rows, and 8 more variables: ZoopBiom <dbl>,
    ## #   ZoopTimeEDT <time>, ZoopDN <fct>, Regress <lgl>, MysDens <dbl>,
    ## #   MysBiom <dbl>, MysDN <fct>, MysTimeEDT <time>

<br>

## Begin plotting relationships between nets

First, which variable on ‘Y’ and which on ‘X’ axis? Plot the more
variable data on the ‘Y’ axis.

Mysid net data are likely to be less variable than Zooplankton net data,
because the mysid data is based on sampling of about 8x more area than
the zooplankton net. The mysid net opening is 0.785, and is towed twice.
The zooplankton net opening is 0.196, and is towed just once.

This is borne out by comparing their CV’s in this dataset:

  - Mysid Net Density CV: 1.15
  - Zoop Net Density CV: 1.51
  - Mysid Net Biomass CV: 1.08
  - Zoop Net Biomass CV: 2.09

OK: plot `Zoop...` data on ‘Y’ axis and `Mys...` data on ‘X’ axis.

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-1.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-3.png)<!-- -->

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

    ## Warning: Removed 4 rows containing missing values (geom_bar).

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-4.png)<!-- -->

    ## # A tibble: 1 x 2
    ##   Density_Proportion Biomass_Proportion
    ##                <dbl>              <dbl>
    ## 1              0.854              0.424

    ## # A tibble: 1 x 2
    ##   Density_Proportion_geomean Biomass_Proportion_geomean
    ##                        <dbl>                      <dbl>
    ## 1                      0.754                      0.289

    ## # A tibble: 1 x 4
    ##   Density_Proporti~ Biomass_Proporti~ Density_Proportion_g~ Biomass_Proportion_~
    ##               <dbl>             <dbl>                 <dbl>                <dbl>
    ## 1             0.769             0.370                 0.696                0.276

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

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-1.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-2.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-3.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-4.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20density%20data-5.png)<!-- -->

Repeat the same thing for biomass data. Make plots of density regression
diagnostics using:

1.  Linear (untransformed) data

2.  Log\_e transformed data

3.  Square-root transformed data

4.  Fourth-root transformed data

In addition, make a plot for each of these with data displayed in
transformed units, with 1:1 reference line, and with linear lm fit line.

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-1.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-2.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-3.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-4.png)<!-- -->

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/check%20for%20data%20transformation%20in%20biomass%20data-5.png)<!-- -->

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

    ##               Estimate Std. Error t value     Pr(>|t|)
    ## (Intercept) -0.2903798 0.10329757 -2.8111 5.189637e-03
    ## MysDens      0.9640695 0.03329797 28.9528 1.106590e-98

    ## [1] "Adjusted r^2:"

    ## [1] 0.684451

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/4th-Root%20Fit%20Stats%20and%20Plots-1.png)<!-- -->

    ## [1] "Biomass 4th-Root Fit Statistics:"

    ##               Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) -0.4361253 0.12047018 -3.620193 3.337152e-04
    ## MysBiom      0.8229876 0.03207529 25.657992 2.307141e-85

    ## [1] "Adjusted r^2:"

    ## [1] 0.6300317

    ## Warning: Removed 9 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 9 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/4th-Root%20Fit%20Stats%20and%20Plots-2.png)<!-- -->

#### Clearly these data are worth using for *Mysis diluviana* in the Great Lakes.

  - Zoop net data will correctly display direction and relative
    magnitude of any changes over time or between lakes.  
  - Zoop net data will probably not representvalues similar to Mysid net
    data.  
  - For densities from the Zoop net, the values themselves are only
    different from the higher Mysid net values by a constant negative
    offset when 4th-root transformed.

n = 396

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

NOAA_Lengths <- 
  read.csv("NOAA_MYSIS_1995-2002_2007-2019_Toby_Lengths_20201214.csv") %>% 
  as_tibble()

NOAA_Biom <- 
  NOAA_Lengths %>% 
  rename(SL_mm = SL_ml) %>% 
  mutate(Visit = paste(Date, Station, sep = "_"), IndMass_mg = exp(-12.27 + 2.72 * log(SL_mm)) * 1000) %>% 
  group_by(Visit) %>% 
  summarize(AvgIndMass_mg = mean(IndMass_mg, na.rm = T),
            Count_Length = length(SL_mm) - sum(is.na(SL_mm)),
            Count_rep = length(unique(repl)))

NOAA =
  read.csv("NOAA_MYSIS_1995-2002_2007-2019_Toby_Densities_tjh_20201214.csv") %>% 
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
  left_join(NOAA_Biom, by = c("Visit")) %>% 
  mutate(Dens = ifelse(is.na(Dens), Count_Length / Count_rep / (0.25 * pi), Dens)) %>% 
  mutate(Biom = ifelse(Dens > 0, AvgIndMass_mg * Dens, 0)) %>% 
  select(-AvgIndMass_mg, -Count_Length, -Count_rep) %>% 
  filter(Station %in% c("M110", "M45"))

# NOAA[NOAA$Visit == "2010-09-30_M110", ] <- 
#   NOAA %>% 
#   filter(Visit == "2010-09-30_M110") %>% 
#   mutate(Lake = "Michigan",
#          Year = 2010,
#          Date = as.Date("2010-09-30"),
#          Month = "September",
#          Season = "Summer",
#          Station = "M110",
#          StationDepth = 110,
#          DepthZone= "Offshore",
#          DN = "Night")
# 
# NOAA[NOAA$Visit == "2014-10-15_M110", ] <- 
#   NOAA %>% 
#   filter(Visit == "2014-10-15_M110") %>% 
#   mutate(Lake = "Michigan",
#          Year = 2014,
#          Date = as.Date("2014-10-15"),
#          Month = "October",
#          Season = "Fall",
#          Station = "M110",
#          StationDepth = 110,
#          DepthZone= "Offshore",
#          DN = "Night")
# 
# NOAA[NOAA$Visit == "2014-10-15_M45", ] <- 
#   NOAA %>% 
#   filter(Visit == "2014-10-15_M45") %>% 
#   mutate(Lake = "Michigan",
#          Year = 2014,
#          Date = as.Date("2014-10-15"),
#          Month = "October",
#          Season = "Fall",
#          Station = "M45",
#          StationDepth = 45,
#          DepthZone= "Mid-depth",
#          DN = "Night")
```

#### Examine `NOAA` tibble

    ## # A tibble: 260 x 13
    ## # Groups:   Visit, Year, Date [260]
    ##    Visit Lake   Year Date       Month Season Station StationDepth DepthZone
    ##    <chr> <fct> <int> <date>     <fct> <fct>  <chr>          <dbl> <fct>    
    ##  1 1995~ Mich~  1995 1995-05-02 May   Spring M110             110 Offshore 
    ##  2 1995~ Mich~  1995 1995-05-31 May   Spring M110             110 Offshore 
    ##  3 1995~ Mich~  1995 1995-06-27 June  Spring M110             110 Offshore 
    ##  4 1995~ Mich~  1995 1995-07-31 July  Summer M110             110 Offshore 
    ##  5 1995~ Mich~  1995 1995-09-23 Sept~ Summer M110             110 Offshore 
    ##  6 1996~ Mich~  1996 1996-04-16 April Spring M110             110 Offshore 
    ##  7 1996~ Mich~  1996 1996-05-20 May   Spring M110             110 Offshore 
    ##  8 1996~ Mich~  1996 1996-06-10 June  Spring M110             110 Offshore 
    ##  9 1996~ Mich~  1996 1996-07-16 July  Summer M110             110 Offshore 
    ## 10 1996~ Mich~  1996 1996-08-07 Augu~ Summer M110             110 Offshore 
    ## # ... with 250 more rows, and 4 more variables: Dens <dbl>, Biom <dbl>,
    ## #   DN <fct>, TimeEDT <lgl>

### Load USGS GLSC Mysid Net Data From Annual Acoustic Surveys in Lakes Michigan and Huron

``` r
# USGS Michigan and Huron

USGS_MI_HU_0 =
  
  read.csv("USGS_Michigan_Huron_2005_2019_Mysis_op_and_density_tjh_20200604.csv") %>% 
  
  as_tibble() %>% 
  
  mutate(Visit = paste(substr(Lake, 1, 1), as.character(OP_DATE), formatC(round(depth,0), width = 3, flag = "0"), sep = "_")) %>% 
  
  mutate(OP_DATE = as.Date(OP_DATE)) %>%
  
  # Filter out Huron 2012 CSMI Surveys
  filter(OP_DATE < as.Date("2012-01-01") | OP_DATE > as.Date("2012-09-01") | Lake == "Michigan") %>% 
  
  # Filter out Huron 2017 CSMI Surveys
  filter(OP_DATE < as.Date("2017-01-01") | OP_DATE > as.Date("2017-09-01") | Lake == "Michigan") %>% 
  
  # Filter out Michigan 2010 CSMI Surveys
  filter((OP_DATE < as.Date("2010-01-01") | (OP_DATE > as.Date("2010-08-01") & OP_DATE < as.Date("2010-09-15")) | OP_DATE > as.Date("2011-01-01")) | Lake == "Huron") %>%
  
  # Filter out Michigan 2015 CSMI Surveys
  filter(OP_DATE < as.Date("2015-01-01") | (OP_DATE > as.Date("2015-08-01") & OP_DATE < as.Date("2015-09-15")) | OP_DATE > as.Date("2016-01-01") | Lake == "Huron") %>%
  
  # arrange(OP_ID)
  arrange(YEAR, VESSEL, OP_DATE, OP_TIME)

USGS_MI_HU_0 =
  USGS_MI_HU_0 %>% 
  mutate(OP_TIME = strptime(paste(OP_DATE, OP_TIME, sep = " "), format = "%Y-%m-%d %H:%M", tz = "EST5EDT"))

# # Check for proper exclusion of CSMI survey visits
# USGS_MI_HU_0 %>% 
#   ggplot(mapping = aes(y = Latitude, x = Longitude)) +
#   geom_point(color = 1, pch = 4, data = read.csv(
#     "USGS_Michigan_Huron_2005_2019_Mysis_op_and_density_tjh_20200604.csv") %>% 
#       as_tibble() %>% 
#       mutate(OP_DATE = as.Date(OP_DATE)) %>%
#       filter(xor(YEAR %in% c(2012, 2017) & Lake == "Huron", YEAR %in% c(2010, 2015) & Lake == "Michigan"))) +
#   geom_point(mapping = aes(color = Lake), pch = 15) +
#   facet_wrap(~YEAR)

USGS_MI_HU_Lengths <- 
  read_csv("USGS_Michigan_Huron_2005_2019_Mysis_individuals.csv",
          col_types = cols(
    OP_ID = col_double(),
    LENGTH = col_double(),
    SEX = col_factor(),
    FECUNDITY = col_integer()
          )
  ) %>% 
  as_tibble()

USGS_MI_HU_Lengths <- 
  USGS_MI_HU_Lengths %>% 
  mutate(SEX = ifelse(is.na(SEX), "0", SEX)) %>% 
  mutate(SEX = fct_recode(SEX, "J" = "0", "M" = "1", "F" = "2"),
         MASS_mg = exp(-12.27 + 2.72 * log(LENGTH)) * 1000)

# USGS_MI_HU_Lengths

USGS_MI_HU_Biom <- 
  USGS_MI_HU_Lengths %>% 
  group_by(OP_ID) %>% 
  summarize(Av_Length = mean(LENGTH),
            Av_Mass = mean(MASS_mg))

# USGS_MI_HU_Biom

USGS_MI_HU <- 
  USGS_MI_HU_0 %>% 
  left_join(USGS_MI_HU_Biom, by = "OP_ID") %>% 
  mutate(biomass_mg = density * Av_Mass)

# USGS_MI_HU %>% select(Av_Mass, OP_ID:OP_DATE, -LAKE_USGS, depth:density, biomass_mg)
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
  select(-Visit, -SubsequentRepATvisit, -OPifFIRSTorONLYrepATvisit) %>% 
  mutate(Season = "Summer",
         DepthZone = cut(depth, c(0, 30, 70, 400), labels = c(
             "Nearshore", "Mid-depth", "Offshore"
           )))
  
(USGS <- 
  USGS_MI_HU3 %>% 
  group_by(Visit_OP, YEAR, Lake, VESSEL) %>% 
  summarize(Date = first(OP_DATE), Time = first(OP_TIME), Month = first(MONTH), 
            Lat = mean(Latitude), Lon = mean(Longitude), StationDepth = mean(depth), Dens = mean(density), 
            Av_Mass = mean(Av_Mass, na.rm = T), N_Col = sum(N), N_Meas = sum(LF_N)) %>% 
    ungroup() %>% 
    mutate(Season = "Summer", DN = "Night", Station = NA, Biom = Dens * Av_Mass, 
           DepthZone = cut(StationDepth, c(0, 30, 70, 400), labels = c(
             "Nearshore", "Mid-depth", "Offshore"
           ))))

rm(USGS_MI_HU_0, USGS_MI_HU2)
# rm(USGS_MI_HU_0, USGS_MI_HU2, USGS_MI_HU3)

str(USGS)
```

#### Examine `USGS` tibble

    ## # A tibble: 414 x 19
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
    ## # ... with 404 more rows, and 10 more variables: StationDepth <dbl>,
    ## #   Dens <dbl>, Av_Mass <dbl>, N_Col <int>, N_Meas <int>, Season <chr>,
    ## #   DN <chr>, Station <lgl>, Biom <dbl>, DepthZone <fct>

    ## # A tibble: 19,252 x 5
    ##    OP_ID LENGTH SEX   FECUNDITY MASS_mg
    ##    <dbl>  <dbl> <fct>     <int>   <dbl>
    ##  1 80040   12.3 M            NA    4.32
    ##  2 80040   10.3 M            NA    2.67
    ##  3 80040   11.4 M            NA    3.52
    ##  4 80040   14.4 M            NA    6.64
    ##  5 80040   10.9 M            NA    3.11
    ##  6 80040   11.4 M            NA    3.52
    ##  7 80040   10.8 M            NA    3.03
    ##  8 80040   11.6 M            NA    3.69
    ##  9 80040   11.5 M            NA    3.60
    ## 10 80040   11.2 M            NA    3.35
    ## # ... with 19,242 more rows

#### Upload Published NOAA and DFO data into tibble

``` r
DFO <- 
  read_csv("DFO_all_ontario_mysid_data_1990_to_2017_from_KB_WC_20201210_tjh_20201210.csv")

DFO %>% names

DFO %>% count(Rep)

DFO <- 
  DFO %>% 
  mutate(Visit = paste("DFO", Year, Month, Station_ID, sep = "_")) %>% 
  group_by(Date, Year, Visit, Station_ID) %>%
  summarize(Dens = mean(Mysid_Dens_m2, na.rm = T),
            Biom = mean(Biom_mg_m2, na.rm = T),
            StationDepth = mean(StationDepth, na.rm = T)) %>% 
  mutate(Lake = "Ontario",
         Season = recode(quarters.Date(Date), 
                         Q2 = "Spring", Q3 = "Summer", Q4 = "Fall", Q1 = "Winter"),
         DepthZone = cut(StationDepth, 
                         c(0, 30, 70, 400), 
                         labels = c("Nearshore", "Mid-depth", "Offshore")),
         DN = "Night", 
         TimeEDT = NA) %>% 
  select(Visit, 
         Lake, 
         Year, 
         Date, 
         Season, 
         Station = Station_ID, 
         StationDepth, 
         DepthZone, 
         Dens, 
         Biom, 
         DN, 
         TimeEDT)

DFO
```

#### Compile data sources into single tibble

``` r
GLNPO_Zoop_Nets <-
  GLNPO %>%
  select(Visit:DepthZone, Dens = ZoopDens, Biom = ZoopBiom, DN = ZoopDN, TimeEDT = ZoopTimeEDT) %>% 
  mutate(TimeEDT = as.POSIXlt(paste(Date, TimeEDT, sep = " ")))

GLNPO_Zoop_Nets %>%
filter(is.na(Season) == FALSE,
         DepthZone == "Offshore")
GLNPO_Zoop_Lengths <- read_csv("Zoop_export_betav5.0_Mysis_Lengths_tjh_20200519_01.csv")

names(GLNPO_Zoop_Nets)
names(GLNPO_Mysid_Nets)
names(NOAA %>% select(-Month))
names(USGS %>% select(Visit = Visit_OP, Lake, Year = YEAR, Date, Season, Station, StationDepth, DepthZone, Dens, Biom, DN, TimeEDT = Time))
names(DFO)


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
    DFO %>% 
      mutate(Group = "DFO")
    ) %>% 
  filter(is.na(Season) == FALSE,
         DepthZone == "Offshore" | Lake == "Erie",
         DepthZone != "Nearshore",
         Season != "Winter") %>% 
  mutate(Period = as.character(cut(Year, c(1989, 1995, 2004, 2012, 2020), labels = c("1990-1995", "1997-2004", "2005-2012", "2013-2019"), right = TRUE))) %>% 
  # mutate(Period = ifelse(Lake == "Michigan" & Year < 2006, "1995-2005", Period)) %>%
  # filter(Year >= 1997) %>% 
  modify_at("Group", as.factor) %>% 
  mutate(Lake = factor(Lake, levels = c("Michigan", "Ontario", "Huron", "Superior", "Erie")))

Mysids$Season = factor(as.character(Mysids$Season), levels = c("Spring", "Summer", "Fall"))

Mysids <- 
  Mysids %>% 
  mutate(Period = ifelse(Group == "NOAA", recode(
    Period,
    `1990-1995` = "1995-2002",
    `1997-2004` = "1995-2002",
    `2005-2012` = "2007-2012"),
    Period)
  ) %>% 

  mutate(Period = ifelse(Group == "DFO", recode(
    Period,
    `1997-2004` = "2002-2004",
    `2013-2019` = "2013-2017"),
    Period)
    ) %>% 
  
  mutate(Period_Name = recode(
    Period,
    `1990-1995` = "Early1990s",
    `1995-2002` = "Early2000s",
    `1997-2004` = "Early2000s",
    `2002-2004` = "Early2000s",
    `2005-2012` = "Late2000s",
    `2007-2012` = "Late2000s",
    `2013-2017` = "The2010s",
    `2013-2019` = "The2010s",
  )) %>% 
  
  mutate(
    
    Period_Name = factor(Period_Name, levels = c(
      "Early1990s", "Early2000s", "Late2000s", "The2010s"
      )),
         
         Period_Numeric = as.numeric(Period_Name))
```

#### Examine `Mysids` tibble

    ## # A tibble: 2,010 x 16
    ## # Groups:   Visit, Lake, Year, Season [1,653]
    ##    Visit Lake   Year Season Station Date       StationDepth DepthZone   Dens
    ##    <chr> <fct> <dbl> <fct>  <chr>   <date>            <dbl> <fct>      <dbl>
    ##  1 E009~ Erie   2000 Spring ER09    2000-04-16           49 Mid-depth NaN   
    ##  2 E009~ Erie   2000 Summer ER09    2000-08-04           30 Mid-depth   0   
    ##  3 E009~ Erie   2001 Summer ER09    2001-08-05           51 Mid-depth  15.2 
    ##  4 E009~ Erie   2002 Summer ER09    2002-08-08           48 Mid-depth   0   
    ##  5 E009~ Erie   2008 Summer ER09    2008-08-11           49 Mid-depth   6.79
    ##  6 E009~ Erie   2009 Summer ER09    2009-08-19           49 Mid-depth   0   
    ##  7 E009~ Erie   2010 Summer ER09    2010-08-11           49 Mid-depth   0   
    ##  8 E009~ Erie   2014 Summer ER09    2014-08-15           49 Mid-depth   0   
    ##  9 E009~ Erie   2015 Summer ER09    2015-08-13           49 Mid-depth  10.1 
    ## 10 E009~ Erie   2018 Summer ER09    2018-08-13           49 Mid-depth  19.0 
    ## # ... with 2,000 more rows, and 7 more variables: Biom <dbl>, DN <chr>,
    ## #   TimeEDT <dttm>, Group <fct>, Period <chr>, Period_Name <fct>,
    ## #   Period_Numeric <dbl>

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-3.png)<!-- -->

<br>

# 3\. Compare Values Among Lakes

    ## # A tibble: 0 x 12
    ## # Groups:   Lake, Period_Name, Period, Period_Numeric, Year, Group [0]
    ## # ... with 12 variables: Lake <fct>, Period_Name <fct>, Period <chr>,
    ## #   Period_Numeric <dbl>, Year <dbl>, Group <fct>, Season <fct>,
    ## #   N_Stations <int>, dens_mean <dbl>, se2_dens <dbl>, biom_mean <dbl>,
    ## #   se2_biom <dbl>

| Lake     | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s | The2010s |
| :------- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | -------: |
| Michigan | 1997-2004 |               2 | GLNPO\_Zoop | Spring |      158.7 |        \- |       \- |
| Michigan | 1995-2002 |               2 | NOAA        | Spring |      155.6 |        \- |       \- |
| Michigan | 1997-2004 |               2 | GLNPO\_Zoop | Summer |      333.8 |        \- |       \- |
| Michigan | 1995-2002 |               2 | NOAA        | Summer |      247.5 |        \- |       \- |
| Michigan | 1995-2002 |               2 | NOAA        | Fall   |      132.8 |        \- |       \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |     122.6 |       \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |      96.6 |       \- |
| Michigan | 2007-2012 |               3 | NOAA        | Spring |         \- |      56.6 |       \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |     158.3 |       \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |     188.6 |       \- |
| Michigan | 2007-2012 |               3 | NOAA        | Summer |         \- |      99.7 |       \- |
| Michigan | 2005-2012 |               3 | USGS        | Summer |         \- |     200.1 |       \- |
| Michigan | 2007-2012 |               3 | NOAA        | Fall   |         \- |      78.6 |       \- |
| Michigan | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- |     79.3 |
| Michigan | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |     48.3 |
| Michigan | 2013-2019 |               4 | NOAA        | Spring |         \- |        \- |     32.3 |
| Michigan | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- |    152.9 |
| Michigan | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- |    126.8 |
| Michigan | 2013-2019 |               4 | NOAA        | Summer |         \- |        \- |     65.4 |
| Michigan | 2013-2019 |               4 | USGS        | Summer |         \- |        \- |    146.2 |
| Michigan | 2013-2019 |               4 | NOAA        | Fall   |         \- |        \- |     60.1 |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-1.png)<!-- -->

| Lake    | Period    | Period\_Numeric | Group       | Season | Early1990s | Early2000s | Late2000s | The2010s |
| :------ | :-------- | --------------: | :---------- | :----- | ---------: | ---------: | --------: | -------: |
| Ontario | 1990-1995 |               1 | DFO         | Fall   |      438.9 |         \- |        \- |       \- |
| Ontario | 1997-2004 |               2 | GLNPO\_Zoop | Spring |         \- |       82.3 |        \- |       \- |
| Ontario | 1997-2004 |               2 | GLNPO\_Zoop | Summer |         \- |      136.0 |        \- |       \- |
| Ontario | 2002-2004 |               2 | DFO         | Fall   |         \- |      258.5 |        \- |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |         \- |     101.8 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |         \- |     168.6 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |         \- |     377.4 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |         \- |     277.8 |       \- |
| Ontario | 2005-2012 |               3 | DFO         | Fall   |         \- |         \- |     208.5 |       \- |
| Ontario | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |         \- |        \- |    199.6 |
| Ontario | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |         \- |        \- |    114.4 |
| Ontario | 2013-2017 |               4 | DFO         | Summer |         \- |         \- |        \- |    332.6 |
| Ontario | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |         \- |        \- |    278.2 |
| Ontario | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |         \- |        \- |    182.3 |
| Ontario | 2013-2017 |               4 | DFO         | Fall   |         \- |         \- |        \- |    164.8 |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-2.png)<!-- -->

| Lake  | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s | The2010s |
| :---- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | -------: |
| Huron | 1997-2004 |               2 | GLNPO\_Zoop | Spring |       53.3 |        \- |       \- |
| Huron | 1997-2004 |               2 | GLNPO\_Zoop | Summer |      135.2 |        \- |       \- |
| Huron | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |      16.5 |       \- |
| Huron | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |      16.2 |       \- |
| Huron | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |      51.0 |       \- |
| Huron | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |      43.1 |       \- |
| Huron | 2005-2012 |               3 | USGS        | Summer |         \- |      64.2 |       \- |
| Huron | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- |      8.2 |
| Huron | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |      7.6 |
| Huron | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- |     30.0 |
| Huron | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- |     22.9 |
| Huron | 2013-2019 |               4 | USGS        | Summer |         \- |        \- |     44.7 |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-3.png)<!-- -->

| Lake     | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s | The2010s |
| :------- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | -------: |
| Superior | 1997-2004 |               2 | GLNPO\_Zoop | Spring |      104.6 |        \- |       \- |
| Superior | 1997-2004 |               2 | GLNPO\_Zoop | Summer |      125.1 |        \- |       \- |
| Superior | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |      78.4 |       \- |
| Superior | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |      70.1 |       \- |
| Superior | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |     212.7 |       \- |
| Superior | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |     146.3 |       \- |
| Superior | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- |    145.6 |
| Superior | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |     91.3 |
| Superior | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- |    216.0 |
| Superior | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- |    146.0 |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-4.png)<!-- -->

| Lake     | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s |  The2010s |
| :------- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | --------: |
| Michigan | 1997-2004 |               2 | GLNPO\_Zoop | Spring |   306.8022 |        \- |        \- |
| Michigan | 1995-2002 |               2 | NOAA        | Spring |   315.5142 |        \- |        \- |
| Michigan | 1997-2004 |               2 | GLNPO\_Zoop | Summer |   528.1741 |        \- |        \- |
| Michigan | 1995-2002 |               2 | NOAA        | Summer |   629.6175 |        \- |        \- |
| Michigan | 1995-2002 |               2 | NOAA        | Fall   |   444.1379 |        \- |        \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |  282.4554 |        \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |  126.6841 |        \- |
| Michigan | 2007-2012 |               3 | NOAA        | Spring |         \- |  143.2405 |        \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |  407.5532 |        \- |
| Michigan | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |  218.2483 |        \- |
| Michigan | 2007-2012 |               3 | NOAA        | Summer |         \- |  301.0591 |        \- |
| Michigan | 2005-2012 |               3 | USGS        | Summer |         \- |  406.2924 |        \- |
| Michigan | 2007-2012 |               3 | NOAA        | Fall   |         \- |  291.2230 |        \- |
| Michigan | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- | 160.12008 |
| Michigan | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |  33.79689 |
| Michigan | 2013-2019 |               4 | NOAA        | Spring |         \- |        \- | 102.45803 |
| Michigan | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- | 292.26811 |
| Michigan | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- |  96.73558 |
| Michigan | 2013-2019 |               4 | NOAA        | Summer |         \- |        \- | 171.42784 |
| Michigan | 2013-2019 |               4 | USGS        | Summer |         \- |        \- | 425.71137 |
| Michigan | 2013-2019 |               4 | NOAA        | Fall   |         \- |        \- | 200.58159 |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-5.png)<!-- -->

| Lake    | Period    | Period\_Numeric | Group       | Season | Early1990s | Early2000s | Late2000s | The2010s |
| :------ | :-------- | --------------: | :---------- | :----- | ---------: | ---------: | --------: | -------: |
| Ontario | 1990-1995 |               1 | DFO         | Fall   |   2022.262 |         \- |        \- |       \- |
| Ontario | 1997-2004 |               2 | GLNPO\_Zoop | Spring |         \- |   120.2587 |        \- |       \- |
| Ontario | 1997-2004 |               2 | GLNPO\_Zoop | Summer |         \- |   267.8324 |        \- |       \- |
| Ontario | 2002-2004 |               2 | DFO         | Fall   |         \- |   894.6626 |        \- |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |         \- |  286.7644 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |         \- |  322.7647 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- |         \- |  817.2097 |       \- |
| Ontario | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |         \- |  640.7953 |       \- |
| Ontario | 2005-2012 |               3 | DFO         | Fall   |         \- |         \- |  741.8831 |       \- |
| Ontario | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |         \- |        \- | 341.3433 |
| Ontario | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |         \- |        \- | 101.9425 |
| Ontario | 2013-2017 |               4 | DFO         | Summer |         \- |         \- |        \- | 815.2259 |
| Ontario | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |         \- |        \- | 590.9945 |
| Ontario | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |         \- |        \- | 213.8916 |
| Ontario | 2013-2017 |               4 | DFO         | Fall   |         \- |         \- |        \- | 724.0704 |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-6.png)<!-- -->

| Lake  | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s |   The2010s |
| :---- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | ---------: |
| Huron | 1997-2004 |               2 | GLNPO\_Zoop | Spring |   72.45905 |        \- |         \- |
| Huron | 1997-2004 |               2 | GLNPO\_Zoop | Summer |  134.36174 |        \- |         \- |
| Huron | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- |  42.42568 |         \- |
| Huron | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |  40.04098 |         \- |
| Huron | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- | 102.78869 |         \- |
| Huron | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- |  40.76219 |         \- |
| Huron | 2005-2012 |               3 | USGS        | Summer |         \- | 182.54682 |         \- |
| Huron | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- |  20.621588 |
| Huron | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |   6.811528 |
| Huron | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- |  58.002158 |
| Huron | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- |  23.834262 |
| Huron | 2013-2019 |               4 | USGS        | Summer |         \- |        \- | 129.922753 |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-7.png)<!-- -->

| Lake     | Period    | Period\_Numeric | Group       | Season | Early2000s | Late2000s |  The2010s |
| :------- | :-------- | --------------: | :---------- | :----- | ---------: | --------: | --------: |
| Superior | 1997-2004 |               2 | GLNPO\_Zoop | Spring |   98.45359 |        \- |        \- |
| Superior | 1997-2004 |               2 | GLNPO\_Zoop | Summer |  154.06955 |        \- |        \- |
| Superior | 2005-2012 |               3 | GLNPO\_Mys  | Spring |         \- | 208.58226 |        \- |
| Superior | 2005-2012 |               3 | GLNPO\_Zoop | Spring |         \- |  77.06876 |        \- |
| Superior | 2005-2012 |               3 | GLNPO\_Mys  | Summer |         \- | 456.40534 |        \- |
| Superior | 2005-2012 |               3 | GLNPO\_Zoop | Summer |         \- | 174.54974 |        \- |
| Superior | 2013-2019 |               4 | GLNPO\_Mys  | Spring |         \- |        \- | 317.84984 |
| Superior | 2013-2019 |               4 | GLNPO\_Zoop | Spring |         \- |        \- |  87.36271 |
| Superior | 2013-2019 |               4 | GLNPO\_Mys  | Summer |         \- |        \- | 406.94728 |
| Superior | 2013-2019 |               4 | GLNPO\_Zoop | Summer |         \- |        \- | 145.07593 |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-8.png)<!-- -->

    ## [1] "Density for All Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-9.png)<!-- -->

    ## [1] "Biomass for All Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-10.png)<!-- -->

| Period\_Numeric | Period\_Name | Period    | Lake     | Group       | N\_Years | dens\_Spring | se2\_dens\_Spring | biom\_Spring | se2\_biom\_Spring | dens\_Summer | se2\_dens\_Summer | biom\_Summer | se2\_biom\_Summer | dens\_Fall | se2\_dens\_Fall | biom\_Fall | se2\_biom\_Fall |
| --------------: | :----------- | :-------- | :------- | :---------- | -------: | -----------: | ----------------: | -----------: | ----------------: | -----------: | ----------------: | -----------: | ----------------: | ---------: | --------------: | ---------: | --------------: |
|               1 | Early1990s   | 1990-1995 | Ontario  | DFO         |        3 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      438.9 |       101.48762 |  2022.2617 |       915.12314 |
|               2 | Early2000s   | 1997-2004 | Superior | GLNPO\_Zoop |        7 |        104.6 |        31.1313915 |    98.453586 |         58.231530 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Superior | GLNPO\_Zoop |        8 |           \- |                \- |           \- |                \- |        125.1 |         27.738535 |  154.0695466 |         68.030778 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Michigan | GLNPO\_Zoop |        7 |        158.7 |        57.0132888 |   306.802206 |        224.054347 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Michigan | GLNPO\_Zoop |        8 |           \- |                \- |           \- |                \- |        333.8 |        126.860047 |  528.1740625 |        311.409479 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1995-2002 | Michigan | NOAA        |        6 |        155.6 |        43.3567834 |   315.514202 |        106.137311 |        247.5 |         81.514089 |  629.6174626 |        210.983290 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1995-2002 | Michigan | NOAA        |        4 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      132.8 |        65.48068 |   444.1379 |       193.57174 |
|               2 | Early2000s   | 1997-2004 | Huron    | GLNPO\_Zoop |        7 |         53.3 |        34.7403351 |    72.459048 |         67.904622 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Huron    | GLNPO\_Zoop |        8 |           \- |                \- |           \- |                \- |        135.2 |         59.722118 |  134.3617361 |         45.352431 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Erie     | GLNPO\_Zoop |        2 |          0.0 |         0.0000000 |     0.000000 |          0.000000 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Erie     | GLNPO\_Zoop |        6 |           \- |                \- |           \- |                \- |          3.2 |          3.555934 |    3.7466667 |          6.834779 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2004 | Ontario  | GLNPO\_Zoop |        7 |         82.3 |        22.9770900 |   120.258675 |         44.514564 |        136.0 |         46.334918 |  267.8323810 |        147.271768 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 2002-2004 | Ontario  | DFO         |        3 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      258.5 |        80.22128 |   894.6626 |       291.59765 |
|               3 | Late2000s    | 2005-2012 | Superior | GLNPO\_Mys  |        6 |         78.4 |        15.4607460 |   208.582261 |         41.729525 |        212.7 |         68.788268 |  456.4053394 |        149.201311 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Superior | GLNPO\_Zoop |        8 |         70.1 |        13.9258111 |    77.068763 |         18.129508 |        146.3 |         43.004642 |  174.5497391 |         60.005557 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Michigan | GLNPO\_Mys  |        7 |        122.6 |        29.8803783 |   282.455408 |         81.323783 |        158.3 |         65.705662 |  407.5532208 |        257.483632 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Michigan | GLNPO\_Zoop |        8 |         96.6 |        22.8863262 |   126.684097 |         55.406862 |        188.6 |         63.561385 |  218.2483125 |        120.222592 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Michigan | USGS        |        8 |           \- |                \- |           \- |                \- |        200.1 |         73.329340 |  406.2924396 |        274.420574 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2007-2012 | Michigan | NOAA        |        6 |         56.6 |        27.1195747 |   143.240538 |         69.939047 |         99.7 |         35.499772 |  301.0590633 |        122.258292 |       78.6 |        23.63709 |   291.2230 |        77.13195 |
|               3 | Late2000s    | 2005-2012 | Huron    | GLNPO\_Mys  |        7 |         16.5 |         6.5842525 |    42.425682 |         16.053898 |         51.0 |         11.318734 |  102.7886941 |         27.131868 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Huron    | GLNPO\_Zoop |        8 |         16.2 |        12.7149764 |    40.040979 |         44.786835 |         43.1 |         27.928582 |   40.7621875 |         30.442615 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Huron    | USGS        |        8 |           \- |                \- |           \- |                \- |         64.2 |         13.855582 |  182.5468246 |         66.910794 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Erie     | GLNPO\_Mys  |        3 |          0.2 |         0.4000000 |     1.574510 |          3.149019 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Erie     | GLNPO\_Mys  |        5 |           \- |                \- |           \- |                \- |          1.0 |          1.070701 |    0.6803465 |          1.054176 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Erie     | GLNPO\_Zoop |        4 |          0.0 |         0.0000000 |     0.000000 |          0.000000 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Erie     | GLNPO\_Zoop |        6 |           \- |                \- |           \- |                \- |          1.4 |          1.197405 |    0.7328704 |          1.046704 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Ontario  | GLNPO\_Mys  |        7 |        101.8 |        42.1117108 |   286.764401 |        166.923125 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Ontario  | GLNPO\_Mys  |        6 |           \- |                \- |           \- |                \- |        377.4 |        281.881079 |  817.2096724 |        337.113836 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Ontario  | GLNPO\_Zoop |        8 |        168.6 |        84.8490756 |   322.764722 |        323.803647 |        277.8 |        125.595942 |  640.7952778 |        397.487250 |         \- |              \- |         \- |              \- |
|               3 | Late2000s    | 2005-2012 | Ontario  | DFO         |        8 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      208.5 |        47.98850 |   741.8831 |       157.29927 |
|               4 | The2010s     | 2013-2019 | Superior | GLNPO\_Mys  |        7 |        145.6 |        29.3604667 |   317.849844 |         64.779173 |        216.0 |         22.905888 |  406.9472826 |         47.602963 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Superior | GLNPO\_Zoop |        6 |         91.3 |        32.8117730 |    87.362706 |         25.052449 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Superior | GLNPO\_Zoop |        7 |           \- |                \- |           \- |                \- |        146.0 |         36.582528 |  145.0759348 |         42.237851 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Michigan | GLNPO\_Mys  |        7 |         79.3 |        32.9757610 |   160.120079 |         50.199692 |        152.9 |         50.207854 |  292.2681102 |         87.157039 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Michigan | GLNPO\_Zoop |        6 |         48.3 |        28.3899826 |    33.796889 |         18.057061 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Michigan | GLNPO\_Zoop |        7 |           \- |                \- |           \- |                \- |        126.8 |         42.570534 |   96.7355782 |         41.134571 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Michigan | USGS        |        7 |           \- |                \- |           \- |                \- |        146.2 |         60.208660 |  425.7113715 |        214.418410 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Michigan | NOAA        |        7 |         32.3 |        18.5414043 |   102.458025 |         54.047760 |         65.4 |         38.383464 |  171.4278387 |         96.842725 |       60.1 |        30.68028 |   200.5816 |       106.78808 |
|               4 | The2010s     | 2013-2019 | Huron    | GLNPO\_Mys  |        7 |          8.2 |         3.1640842 |    20.621588 |          9.235574 |         30.0 |          6.314566 |   58.0021577 |         12.677054 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Huron    | GLNPO\_Zoop |        6 |          7.6 |         2.6059760 |     6.811528 |          4.172382 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Huron    | GLNPO\_Zoop |        7 |           \- |                \- |           \- |                \- |         22.9 |         12.309921 |   23.8342619 |         19.725323 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Huron    | USGS        |        7 |           \- |                \- |           \- |                \- |         44.7 |         10.346540 |  129.9227526 |         40.970052 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Erie     | GLNPO\_Mys  |        5 |          0.6 |         0.9436101 |     4.349987 |          6.273840 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Erie     | GLNPO\_Mys  |        4 |           \- |                \- |           \- |                \- |          6.6 |          6.305751 |    1.7067676 |          1.685544 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Erie     | GLNPO\_Zoop |        4 |          0.0 |         0.0000000 |     0.000000 |          0.000000 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Erie     | GLNPO\_Zoop |        5 |           \- |                \- |           \- |                \- |          3.1 |          3.363332 |    1.0018333 |          1.403028 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Ontario  | GLNPO\_Mys  |        7 |        199.6 |        59.6789004 |   341.343259 |         90.552405 |        278.2 |        122.582378 |  590.9944744 |        180.337017 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Ontario  | GLNPO\_Zoop |        6 |        114.4 |        30.3016685 |   101.942500 |         53.686654 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2019 | Ontario  | GLNPO\_Zoop |        7 |           \- |                \- |           \- |                \- |        182.3 |         56.591425 |  213.8916190 |         64.652506 |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2017 | Ontario  | DFO         |        1 |           \- |                \- |           \- |                \- |        332.6 |                \- |  815.2259259 |                \- |         \- |              \- |         \- |              \- |
|               4 | The2010s     | 2013-2017 | Ontario  | DFO         |        4 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      164.8 |        39.21593 |   724.0704 |        99.97280 |

Whole Table

#### Models of Lake Differences

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

    ## Warning: Removed 10 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/visualize%20fits-1.png)<!-- -->

    ## Warning: Removed 10 rows containing missing values (geom_col).

![](GLNPO_Long_term_2019_files/figure-gfm/visualize%20fits-2.png)<!-- -->

    ## Warning: Removed 5 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 10 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/visualize%20fits-3.png)<!-- -->

    ## Warning: Removed 10 rows containing missing values (geom_col).

![](GLNPO_Long_term_2019_files/figure-gfm/visualize%20fits-4.png)<!-- -->

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Dens_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Michigan 1997-2004     246. 
    ## 2 Superior 1997-2004     115. 
    ## 3 Ontario  1997-2004     109. 
    ## 4 Huron    1997-2004      94.2
    ## 5 Erie     1997-2004       1.6

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie Michigan -5.857 -6.941 -4.773 0.000
    ## 2      Erie Superior -5.226 -6.310 -4.142 0.000
    ## 3      Erie  Ontario -5.108 -6.205 -4.011 0.000
    ## 4      Erie    Huron -4.698 -5.782 -3.614 0.000
    ## 5     Huron Michigan -1.159 -2.063 -0.255 0.006
    ## 6  Michigan  Ontario  0.749 -0.171  1.669 0.163
    ## 7  Michigan Superior  0.631 -0.273  1.535 0.297
    ## 8     Huron Superior -0.528 -1.432  0.376 0.477
    ## 9     Huron  Ontario -0.410 -1.330  0.510 0.720
    ## 10 Superior  Ontario  0.118 -0.802  1.038 0.996

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Dens_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Ontario  2005-2012   231.   
    ## 2 Michigan 2005-2012   142.   
    ## 3 Superior 2005-2012   127.   
    ## 4 Huron    2005-2012    31.7  
    ## 5 Erie     2005-2012     0.650

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie  Ontario -5.944 -6.521 -5.367 0.000
    ## 2      Erie Michigan -5.706 -6.280 -5.132 0.000
    ## 3      Erie Superior -5.533 -6.115 -4.952 0.000
    ## 4      Erie    Huron -3.951 -4.524 -3.377 0.000
    ## 5     Huron  Ontario -1.993 -2.494 -1.492 0.000
    ## 6     Huron Michigan -1.755 -2.252 -1.259 0.000
    ## 7     Huron Superior -1.583 -2.088 -1.077 0.000
    ## 8  Superior  Ontario -0.411 -0.920  0.099 0.175
    ## 9  Michigan  Ontario -0.238 -0.739  0.263 0.683
    ## 10 Michigan Superior  0.173 -0.333  0.678 0.879

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Dens_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Ontario  2013-2019    194.  
    ## 2 Superior 2013-2019    150.  
    ## 3 Michigan 2013-2019    102.  
    ## 4 Huron    2013-2019     17.2 
    ## 5 Erie     2013-2019      2.58

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie  Ontario -5.417 -6.052 -4.781 0.000
    ## 2      Erie Superior -5.251 -5.886 -4.616 0.000
    ## 3      Erie Michigan -4.697 -5.333 -4.062 0.000
    ## 4      Erie    Huron -2.904 -3.539 -2.269 0.000
    ## 5     Huron  Ontario -2.512 -3.081 -1.944 0.000
    ## 6     Huron Superior -2.347 -2.915 -1.779 0.000
    ## 7     Huron Michigan -1.793 -2.362 -1.225 0.000
    ## 8  Michigan  Ontario -0.719 -1.287 -0.151 0.006
    ## 9  Michigan Superior -0.554 -1.122  0.015 0.060
    ## 10 Superior  Ontario -0.165 -0.734  0.403 0.928

    ## # A tibble: 15 x 4
    ##    Lake     Period    Dens_mean DensTukey
    ##    <fct>    <chr>         <dbl> <chr>    
    ##  1 Michigan 1997-2004   246.    A        
    ##  2 Superior 1997-2004   115.    AB       
    ##  3 Ontario  1997-2004   109.    AB       
    ##  4 Huron    1997-2004    94.2   B        
    ##  5 Erie     1997-2004     1.6   C        
    ##  6 Ontario  2005-2012   231.    A        
    ##  7 Michigan 2005-2012   142.    A        
    ##  8 Superior 2005-2012   127.    A        
    ##  9 Huron    2005-2012    31.7   B        
    ## 10 Erie     2005-2012     0.650 C        
    ## 11 Ontario  2013-2019   194.    A        
    ## 12 Superior 2013-2019   150.    AB       
    ## 13 Michigan 2013-2019   102.    B        
    ## 14 Huron    2013-2019    17.2   C        
    ## 15 Erie     2013-2019     2.58  D

    ## Warning: Removed 10 rows containing missing values (position_stack).

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-1.png)<!-- -->

    ## Warning: Removed 10 rows containing missing values (geom_col).

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-2.png)<!-- -->

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Biom_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Michigan 1997-2004    417.  
    ## 2 Ontario  1997-2004    194.  
    ## 3 Superior 1997-2004    126.  
    ## 4 Huron    1997-2004    103.  
    ## 5 Erie     1997-2004      1.87

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie Michigan -7.167 -8.675 -5.658 0.000
    ## 2      Erie  Ontario -6.608 -8.135 -5.082 0.000
    ## 3      Erie Superior -6.182 -7.690 -4.673 0.000
    ## 4      Erie    Huron -5.871 -7.379 -4.362 0.000
    ## 5     Huron Michigan -1.296 -2.554 -0.038 0.040
    ## 6  Michigan Superior  0.985 -0.273  2.243 0.193
    ## 7     Huron  Ontario -0.738 -2.018  0.543 0.491
    ## 8  Michigan  Ontario  0.558 -0.722  1.838 0.737
    ## 9  Superior  Ontario -0.427 -1.707  0.853 0.881
    ## 10    Huron Superior -0.311 -1.569  0.947 0.957

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Biom_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Ontario  2005-2012   517.   
    ## 2 Michigan 2005-2012   259.   
    ## 3 Superior 2005-2012   229.   
    ## 4 Huron    2005-2012    56.5  
    ## 5 Erie     2005-2012     0.747

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie  Ontario -7.631 -8.478 -6.783 0.000
    ## 2      Erie Michigan -7.131 -7.973 -6.289 0.000
    ## 3      Erie Superior -6.972 -7.826 -6.119 0.000
    ## 4      Erie    Huron -5.172 -6.014 -4.330 0.000
    ## 5     Huron  Ontario -2.459 -3.194 -1.723 0.000
    ## 6     Huron Michigan -1.959 -2.689 -1.230 0.000
    ## 7     Huron Superior -1.801 -2.543 -1.058 0.000
    ## 8  Superior  Ontario -0.658 -1.407  0.090 0.113
    ## 9  Michigan  Ontario -0.500 -1.235  0.236 0.334
    ## 10 Michigan Superior  0.159 -0.583  0.901 0.976

    ## # A tibble: 5 x 3
    ## # Groups:   Lake [5]
    ##   Lake     Period    Biom_mean
    ##   <fct>    <chr>         <dbl>
    ## 1 Ontario  2013-2019    312.  
    ## 2 Superior 2013-2019    239.  
    ## 3 Michigan 2013-2019    146.  
    ## 4 Huron    2013-2019     27.3 
    ## 5 Erie     2013-2019      1.76

    ##       Lake1    Lake2   diff    lwr    upr p adj
    ## 1      Erie  Ontario -6.598 -7.371 -5.826 0.000
    ## 2      Erie Superior -6.426 -7.198 -5.653 0.000
    ## 3      Erie Michigan -5.745 -6.518 -4.973 0.000
    ## 4      Erie    Huron -3.902 -4.674 -3.130 0.000
    ## 5     Huron  Ontario -2.696 -3.387 -2.006 0.000
    ## 6     Huron Superior -2.524 -3.214 -1.833 0.000
    ## 7     Huron Michigan -1.843 -2.534 -1.153 0.000
    ## 8  Michigan  Ontario -0.853 -1.544 -0.162 0.008
    ## 9  Michigan Superior -0.680 -1.371  0.010 0.056
    ## 10 Superior  Ontario -0.173 -0.864  0.518 0.958

    ## # A tibble: 15 x 4
    ##    Lake     Period    Biom_mean BiomTukey
    ##    <fct>    <chr>         <dbl> <chr>    
    ##  1 Michigan 1997-2004   417.    A        
    ##  2 Ontario  1997-2004   194.    AB       
    ##  3 Superior 1997-2004   126.    AB       
    ##  4 Huron    1997-2004   103.    B        
    ##  5 Erie     1997-2004     1.87  C        
    ##  6 Ontario  2005-2012   517.    A        
    ##  7 Michigan 2005-2012   259.    A        
    ##  8 Superior 2005-2012   229.    A        
    ##  9 Huron    2005-2012    56.5   B        
    ## 10 Erie     2005-2012     0.747 C        
    ## 11 Ontario  2013-2019   312.    A        
    ## 12 Superior 2013-2019   239.    AB       
    ## 13 Michigan 2013-2019   146.    B        
    ## 14 Huron    2013-2019    27.3   C        
    ## 15 Erie     2013-2019     1.76  D

    ## Warning: Removed 10 rows containing missing values (position_stack).

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-3.png)<!-- -->

    ## Warning: Removed 10 rows containing missing values (geom_col).

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-4.png)<!-- -->

    ## Warning: Removed 10 rows containing missing values (geom_col).

    ## Warning: Removed 50 rows containing missing values (geom_text).

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-5.png)<!-- -->

<br>

# 4\. Examine Time Series Trends in Each Lake

### Plot trends with smoother gams for each season, group and lake.

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-5.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-6.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-7.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-8.png)<!-- -->

    ## [1] "Density: all Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-9.png)<!-- -->

    ## [1] "Biomass: all Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-10.png)<!-- -->

### Fit GAM models for each Lake

Looks like Fourth-root transformation will be the best. Natural-Log is
also good. Fourth-root does better at homoscedasticity for biomass,
while natural log might do slightly better at homoscedasticity for
density. Fourth-root does better at normality for both types of data.
Fourth-root also has .

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
    ## TransDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.01958    0.07517  40.168  < 2e-16 ***
    ## SeasonSummer     0.59616    0.06882   8.662  < 2e-16 ***
    ## SeasonFall       0.31976    0.12833   2.492   0.0130 *  
    ## GroupGLNPO_Zoop -0.26831    0.08966  -2.993   0.0029 ** 
    ## GroupNOAA       -0.43836    0.09511  -4.609 5.08e-06 ***
    ## GroupUSGS       -0.08130    0.09510  -0.855   0.3930    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 8.199  8.802 28.85  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.416   Deviance explained =   43%
    ## -REML = 558.18  Scale est. = 0.4199    n = 542

``` r
summary(Ontario_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.67411    0.24002  15.307  < 2e-16 ***
    ## SeasonSummer     0.64388    0.12862   5.006 7.23e-07 ***
    ## SeasonFall       0.02991    0.24478   0.122   0.9028    
    ## GroupGLNPO_Mys  -0.20975    0.23575  -0.890   0.3739    
    ## GroupGLNPO_Zoop -0.64576    0.22975  -2.811   0.0051 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df    F  p-value    
    ## s(Year) 2.949  3.655 18.5 5.05e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.167   Deviance explained = 17.6%
    ## -REML = 795.09  Scale est. = 0.70301   n = 633

``` r
summary(Huron_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      1.77011    0.09463  18.706  < 2e-16 ***
    ## SeasonSummer     0.78448    0.10188   7.700 2.02e-13 ***
    ## GroupGLNPO_Zoop -0.38509    0.11087  -3.473  0.00059 ***
    ## GroupUSGS        0.09463    0.12355   0.766  0.44434    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df    F  p-value    
    ## s(Year) 2.626  3.276 14.8 2.32e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.339   Deviance explained = 35.1%
    ## -REML = 346.62  Scale est. = 0.54092   n = 305

``` r
summary(Superior_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.23174    0.06321  51.127  < 2e-16 ***
    ## SeasonSummer     0.44473    0.06174   7.203 2.74e-12 ***
    ## GroupGLNPO_Zoop -0.39190    0.07068  -5.545 5.21e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value  
    ## s(Year) 4.233  5.214 2.343  0.0384 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.201   Deviance explained = 21.2%
    ## -REML = 421.42  Scale est. = 0.3993    n = 428

``` r
summary(Erie_Dens_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransDens ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.3541     0.1513   2.341 0.021767 *  
    ## SeasonSummer      0.5694     0.1405   4.054 0.000118 ***
    ## GroupGLNPO_Zoop  -0.3424     0.1512  -2.265 0.026283 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value  
    ## s(Year) 2.358  2.916 2.653  0.0772 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.246   Deviance explained = 28.6%
    ## -REML = 82.931  Scale est. = 0.3766    n = 84

``` r
#

summary(Michigan_Biom_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransBiom ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.63909    0.09298  39.138  < 2e-16 ***
    ## SeasonSummer     0.67944    0.08527   7.968 1.23e-14 ***
    ## SeasonFall       0.61626    0.16016   3.848 0.000136 ***
    ## GroupGLNPO_Zoop -1.06322    0.11257  -9.445  < 2e-16 ***
    ## GroupNOAA       -0.26180    0.11770  -2.224 0.026612 *  
    ## GroupUSGS        0.15128    0.13047   1.159 0.246855    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 8.477  8.912 32.77  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.466   Deviance explained = 48.1%
    ## -REML = 602.79  Scale est. = 0.64227   n = 483

``` r
summary(Ontario_Biom_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransBiom ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.5801     0.3192  14.351  < 2e-16 ***
    ## SeasonSummer      0.9226     0.1699   5.430 8.07e-08 ***
    ## SeasonFall        0.5894     0.3254   1.811   0.0706 .  
    ## GroupGLNPO_Mys   -0.3406     0.3139  -1.085   0.2783    
    ## GroupGLNPO_Zoop  -1.3354     0.3058  -4.366 1.48e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df    F p-value    
    ## s(Year) 3.684  4.547 29.7  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.385   Deviance explained = 39.2%
    ## -REML = 970.23  Scale est. = 1.2249    n = 633

``` r
summary(Huron_Biom_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransBiom ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       2.2269     0.1063  20.943  < 2e-16 ***
    ## SeasonSummer      0.7755     0.1146   6.764 8.16e-11 ***
    ## GroupGLNPO_Zoop  -0.9986     0.1259  -7.931 5.62e-14 ***
    ## GroupUSGS         0.5338     0.1461   3.653  0.00031 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F  p-value    
    ## s(Year) 7.278  8.249 11.54 1.33e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.487   Deviance explained = 50.5%
    ## -REML = 362.15  Scale est. = 0.67072   n = 284

``` r
summary(Superior_Biom_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransBiom ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.94675    0.06816   57.91  < 2e-16 ***
    ## SeasonSummer     0.51353    0.06652    7.72 8.65e-14 ***
    ## GroupGLNPO_Zoop -1.15087    0.07665  -15.02  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F  p-value    
    ## s(Year) 6.976  8.042 9.752 1.77e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =    0.5   Deviance explained = 51.1%
    ## -REML = 457.98  Scale est. = 0.46186   n = 428

``` r
summary(Erie_Biom_GAM)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## TransBiom ~ s(Year, bs = "tp") + Season + Group
    ## 
    ## Parametric coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.5077     0.1393   3.645 0.000477 ***
    ## SeasonSummer      0.2957     0.1292   2.288 0.024799 *  
    ## GroupGLNPO_Zoop  -0.4240     0.1390  -3.051 0.003109 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##          edf Ref.df     F p-value
    ## s(Year) 2.01  2.492 1.786    0.21
    ## 
    ## R-sq.(adj) =  0.175   Deviance explained = 21.5%
    ## -REML = 76.006  Scale est. = 0.31988   n = 84

<br> <br>

### Plot fitted GAM models for each lake by season

**Need to visualize mysid densities:**

  - In 4 deep lakes

  - Over \~ 20-25 years (1990/1995/1997 - 2019)

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

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-5.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-6.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-7.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-8.png)<!-- -->

    ## # A tibble: 7 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1995,2001) 221.  415.  318. 
    ## 2 [2001,2003) 280.  442.  370. 
    ## 3 [2003,2008) 192.  444.  294. 
    ## 4 [2008,2012) 132.  191.  162. 
    ## 5 [2012,2015) 133.  182.  159. 
    ## 6 [2015,2019)  41.8 182.  115. 
    ## 7 [2019,2020)  39.3  39.3  39.3

    ## # A tibble: 7 x 4
    ##   Trend         min    max   mean
    ##   <fct>       <dbl>  <dbl>  <dbl>
    ## 1 [1995,2001) 341.   734.   520. 
    ## 2 [2001,2003) 761.  1676.  1263. 
    ## 3 [2003,2008) 413.  1695.   935. 
    ## 4 [2008,2012) 257.   410.   327. 
    ## 5 [2012,2015) 259.   384.   326. 
    ## 6 [2015,2019)  87.2  382.   235. 
    ## 7 [2019,2020)  82.4   82.4   82.4

    ## # A tibble: 3 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1997,2002)  97.7 149.  123. 
    ## 2 [2002,2008)  46.7  96.8  69.6
    ## 3 [2008,2020)  25.6  46.2  32.7

    ## # A tibble: 3 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1997,2002) 157.  316.  193. 
    ## 2 [2002,2008)  64.8 448.  273. 
    ## 3 [2008,2020)  38.7  67.5  58.2

    ## # A tibble: 4 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1997,1998)  131.  141.  136.
    ## 2 [1998,2005)  142.  185.  172.
    ## 3 [2005,2014)  166.  214.  184.
    ## 4 [2014,2020)  163.  214.  193.

    ## # A tibble: 4 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1997,1998)  125.  148.  136.
    ## 2 [1998,2003)  152.  609.  373.
    ## 3 [2003,2005)  480.  608.  555.
    ## 4 [2005,2020)  323.  472.  407.

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2685  0.4007  0.4874  0.7149  0.6384  2.9340

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2428  0.2814  0.3266  0.4108  0.3901  1.2471

    ## # A tibble: 3 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1990,1991)  566.  595.  581.
    ## 2 [1991,2001)  306.  563.  422.
    ## 3 [2001,2020)  243.  304.  254.

    ## # A tibble: 3 x 4
    ##   Trend         min   max  mean
    ##   <fct>       <dbl> <dbl> <dbl>
    ## 1 [1990,1991) 1966. 2194. 2079.
    ## 2 [1991,2001)  737. 1942. 1161.
    ## 3 [2001,2020)  527.  734.  598.

#### …And versions with data plotted for supplementary figure(s):

The plot below shows the same above GAM fits superimposed on
model-adjusted data points. The model adjustment in the data points is
that of re-predicting the values assuming the GLNPO Mysid net was used
to collect the sample. Thus, each point is the predicted GAM value for
the lake, season, and year shown and gvien the `GLNPO_Mys` `Group`
effect, plus the residual associated with that point’s fit to the GAM.

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20-%20GAM%20adjusted%20for%20GLNPO_Mys-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20-%20GAM%20adjusted%20for%20GLNPO_Mys-2.png)<!-- -->

The below plots show the raw data, color coded by collection group. The
GAM model fits are not plotted with these data.

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20with%20raw%20data-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20with%20raw%20data-2.png)<!-- -->

<br>

# 5\. Examine averages and trends in life history rates in the lakes

## Input/wrangle any remaining individual data needed

#### Zooplankton net Mysid Lengths Data

    ## # A tibble: 9,351 x 5
    ## # Groups:   Visit, Lake, Year, Season [597]
    ##    Lake   Year Season Visit   Length_mm
    ##    <fct> <dbl> <chr>  <chr>       <dbl>
    ##  1 Erie   2001 Summer E009G01      3.67
    ##  2 Erie   2001 Summer E009G01      4.01
    ##  3 Erie   2001 Summer E009G01      4.84
    ##  4 Erie   2008 Summer E009G08      3.50
    ##  5 Erie   2015 Summer E009G15      3.43
    ##  6 Erie   2015 Summer E009G15      3.47
    ##  7 Erie   2018 Summer E009G18      3.44
    ##  8 Erie   2018 Summer E009G18      3.56
    ##  9 Erie   2018 Summer E009G18      3.46
    ## 10 Erie   2003 Summer E063G03      5.15
    ## # ... with 9,341 more rows

    ## # A tibble: 9,351 x 8
    ## # Groups:   Visit [597]
    ##    Lake.x Year.x Season.x Visit   Length_mm Lake.y Year.y Season.y
    ##    <fct>   <dbl> <chr>    <chr>       <dbl> <fct>   <int> <fct>   
    ##  1 Erie     2001 Summer   E009G01      3.67 Erie     2001 Summer  
    ##  2 Erie     2001 Summer   E009G01      4.01 Erie     2001 Summer  
    ##  3 Erie     2001 Summer   E009G01      4.84 Erie     2001 Summer  
    ##  4 Erie     2008 Summer   E009G08      3.50 Erie     2008 Summer  
    ##  5 Erie     2015 Summer   E009G15      3.43 Erie     2015 Summer  
    ##  6 Erie     2015 Summer   E009G15      3.47 Erie     2015 Summer  
    ##  7 Erie     2018 Summer   E009G18      3.44 Erie     2018 Summer  
    ##  8 Erie     2018 Summer   E009G18      3.56 Erie     2018 Summer  
    ##  9 Erie     2018 Summer   E009G18      3.46 Erie     2018 Summer  
    ## 10 Erie     2003 Summer   E063G03      5.15 Erie     2003 Summer  
    ## # ... with 9,341 more rows

#### GLNPO Mysid Net Length Data

    ## # A tibble: 546 x 3
    ##    Visit      N_Lengths N_Lengths_Cum
    ##    <chr>          <int>         <int>
    ##  1 06GB50M5         631           631
    ##  2 17H07HB80         78           709
    ##  3 17H07MR80         18           727
    ##  4 17H07OSEA         77           804
    ##  5 17H07OSWE         28           832
    ##  6 17H07SR80N        20           852
    ##  7 8009             232          1084
    ##  8 8010             183          1267
    ##  9 8032              48          1315
    ## 10 8033             417          1732
    ## # ... with 536 more rows

    ## # A tibble: 6 x 3
    ##   Visit       N_Lengths N_Lengths_Cum
    ##   <chr>           <int>         <int>
    ## 1 S22bG09           148        218681
    ## 2 S22bG18           202        218883
    ## 3 S23bG16           232        219115
    ## 4 SMEDG17           116        219231
    ## 5 SUXG11           1072        220303
    ## 6 TCSUM12HU37       328        220631

    ## # A tibble: 220,631 x 2
    ##    Visit    Length
    ##    <chr>     <dbl>
    ##  1 06GB50M5    2.5
    ##  2 06GB50M5    2.4
    ##  3 06GB50M5    2.8
    ##  4 06GB50M5    2.7
    ##  5 06GB50M5    2.2
    ##  6 06GB50M5    2.7
    ##  7 06GB50M5    2.7
    ##  8 06GB50M5    2.8
    ##  9 06GB50M5    2.3
    ## 10 06GB50M5    2.5
    ## # ... with 220,621 more rows

    ## # A tibble: 215,785 x 5
    ##    Visit    Length Lake   Year Season
    ##    <chr>     <dbl> <fct> <int> <fct> 
    ##  1 06GB50M5    2.5 Huron  2006 Summer
    ##  2 06GB50M5    2.4 Huron  2006 Summer
    ##  3 06GB50M5    2.8 Huron  2006 Summer
    ##  4 06GB50M5    2.7 Huron  2006 Summer
    ##  5 06GB50M5    2.2 Huron  2006 Summer
    ##  6 06GB50M5    2.7 Huron  2006 Summer
    ##  7 06GB50M5    2.7 Huron  2006 Summer
    ##  8 06GB50M5    2.8 Huron  2006 Summer
    ##  9 06GB50M5    2.3 Huron  2006 Summer
    ## 10 06GB50M5    2.5 Huron  2006 Summer
    ## # ... with 215,775 more rows

#### Wrangle USGS and NOAA Individual Data

<br>

## Calculate summary values from life history data

### Brooding Females Data Plotting and Analysis

    ## # A tibble: 5 x 7
    ## # Groups:   Lake, Season [5]
    ##   Lake         Season  N_BF BF_Length BF_Length_2SE Brood_Count Brood_Count_2SE
    ##   <fct>        <fct>  <int>     <dbl>         <dbl>       <dbl>           <dbl>
    ## 1 Eastern_Erie Spring     5      15.5         0.506        25.4            5.12
    ## 2 Ontario      Spring    NA      14.3        NA            16.9           NA   
    ## 3 Michigan     Spring    NA      14.5        NA            16.5           NA   
    ## 4 Huron        Spring    NA      14.2        NA            14.3           NA   
    ## 5 Superior     Spring    NA      15.0        NA            13.3           NA

#### Brooding Females - all groups and seasons

    ## Warning: Removed 21 rows containing missing values (geom_point).

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-2.png)<!-- -->

    ## # A tibble: 15 x 8
    ## # Groups:   Lake, Season [11]
    ##    Lake     Season Group     Brood_Count_Mean Brood_Count_SE BF_Length_Mean
    ##    <fct>    <fct>  <chr>                <dbl>          <dbl>          <dbl>
    ##  1 Erie     Spring GLNPO_Mys             26.2          1.93            15.4
    ##  2 Michigan Fall   NOAA                  21.7          0.261           17.1
    ##  3 Ontario  Spring GLNPO_Mys             15.9          0.331           14.2
    ##  4 Michigan Summer NOAA                  18.1          0.382           16.5
    ##  5 Michigan Spring GLNPO_Mys             15.9          0.234           14.6
    ##  6 Huron    Summer GLNPO_Mys             15.3          0.763           14.4
    ##  7 Michigan Winter NOAA                  15.0          0.299           15.0
    ##  8 Michigan Summer GLNPO_Mys             15.5          0.358           15.5
    ##  9 Michigan Spring NOAA                  14.6          0.214           15.0
    ## 10 Huron    Spring GLNPO_Mys             13.9          0.290           14.6
    ## 11 Ontario  Summer GLNPO_Mys             14.8          1.30            16.2
    ## 12 Superior Summer GLNPO_Mys             13.0          0.409           15.2
    ## 13 Superior Spring GLNPO_Mys             12.4          0.165           14.9
    ## 14 Michigan Summer USGS                 NaN           NA               15.4
    ## 15 Huron    Summer USGS                 NaN           NA               12.7
    ##    BF_Length_SE Brood_per_mm
    ##           <dbl>        <dbl>
    ##  1       0.148         1.69 
    ##  2       0.0387        1.27 
    ##  3       0.0520        1.12 
    ##  4       0.0765        1.10 
    ##  5       0.0660        1.09 
    ##  6       0.227         1.07 
    ##  7       0.0781        1.00 
    ##  8       0.0904        1.00 
    ##  9       0.0424        0.976
    ## 10       0.0533        0.953
    ## 11       0.151         0.911
    ## 12       0.103         0.855
    ## 13       0.0437        0.830
    ## 14      NA           NaN    
    ## 15      NA           NaN

    ## Warning: Ignoring unknown aesthetics: linestyle

    ## Warning: Removed 982 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 982 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-3.png)<!-- -->

#### Brooding females - GLNPO Spring across lakes

    ## Warning: Removed 578 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 578 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-1.png)<!-- -->

    ## Warning: Removed 578 rows containing non-finite values (stat_smooth).

    ## Warning in qt((1 - level)/2, df): NaNs produced
    
    ## Warning in qt((1 - level)/2, df): NaNs produced

    ## Warning: Removed 578 rows containing missing values (geom_point).

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf
    
    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-2.png)<!-- -->

    ## Warning: Removed 31 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 31 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-3.png)<!-- -->

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake     Season Group     Brood_Count_Mean Brood_Count_SE BF_Length_Mean
    ##   <fct>    <fct>  <chr>                <dbl>          <dbl>          <dbl>
    ## 1 Erie     Spring GLNPO_Mys             26.2          1.93            15.4
    ## 2 Ontario  Spring GLNPO_Mys             15.9          0.331           14.2
    ## 3 Michigan Spring GLNPO_Mys             15.9          0.234           14.6
    ## 4 Huron    Spring GLNPO_Mys             13.9          0.290           14.6
    ## 5 Superior Spring GLNPO_Mys             12.4          0.165           14.9
    ##   BF_Length_SE Brood_per_mm
    ##          <dbl>        <dbl>
    ## 1       0.148         1.69 
    ## 2       0.0520        1.12 
    ## 3       0.0660        1.09 
    ## 4       0.0533        0.953
    ## 5       0.0437        0.830

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake  Season Group Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <fct> <fct>  <chr>            <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Erie  Spring GLNP~             26.2          1.93            15.4       0.148 
    ## 2 Onta~ Spring GLNP~             15.9          0.331           14.2       0.0520
    ## 3 Mich~ Spring GLNP~             15.9          0.234           14.6       0.0660
    ## 4 Huron Spring GLNP~             13.9          0.290           14.6       0.0533
    ## 5 Supe~ Spring GLNP~             12.4          0.165           14.9       0.0437
    ## # ... with 1 more variable: Brood_per_mm <dbl>

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake     Season Group     Brood_Count_Mean Brood_Count_SE BF_Length_Mean
    ##   <fct>    <fct>  <chr>                <dbl>          <dbl>          <dbl>
    ## 1 Erie     Spring GLNPO_Mys             24.2          3.36            15.7
    ## 2 Ontario  Spring GLNPO_Mys             17.5          0.249           14.3
    ## 3 Michigan Spring GLNPO_Mys             16.4          0.389           14.9
    ## 4 Huron    Spring GLNPO_Mys             13.7          0.702           14.8
    ## 5 Superior Spring GLNPO_Mys             13.2          0.251           15.0
    ##   BF_Length_SE Brood_per_mm
    ##          <dbl>        <dbl>
    ## 1       0.151         1.55 
    ## 2       0.0820        1.22 
    ## 3       0.0982        1.10 
    ## 4       0.0933        0.929
    ## 5       0.0579        0.880

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake  Season Group Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <fct> <fct>  <chr>            <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Erie  Spring GLNP~             24.2          3.36            15.7       0.151 
    ## 2 Onta~ Spring GLNP~             17.5          0.249           14.3       0.0820
    ## 3 Mich~ Spring GLNP~             16.4          0.389           14.9       0.0982
    ## 4 Huron Spring GLNP~             13.7          0.702           14.8       0.0933
    ## 5 Supe~ Spring GLNP~             13.2          0.251           15.0       0.0579
    ## # ... with 1 more variable: Brood_per_mm <dbl>

    ## # A tibble: 2 x 6
    ##   Season Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <fct>             <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Fall               21.7          0.261           17.1       0.0387
    ## 2 Spring             14.6          0.214           15.0       0.0424
    ##   Brood_per_mm
    ##          <dbl>
    ## 1        1.27 
    ## 2        0.976

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-5.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-6.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-7.png)<!-- -->

#### Brooding females - NOAA Spring-Fall in Michigan

    ## Warning: Removed 19 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 19 rows containing missing values (geom_point).

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-2.png)<!-- -->

    ## Warning: Removed 979 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 979 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-3.png)<!-- -->

    ## # A tibble: 6 x 7
    ## # Groups:   Group [2]
    ##   Group     Season Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <chr>     <fct>             <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 GLNPO_Mys Spring             15.1         0.0903           14.6       0.0136
    ## 2 GLNPO_Mys Summer             14.5         0.137            15.3       0.0344
    ## 3 NOAA      Fall               21.7         0.261            17.1       0.0387
    ## 4 NOAA      Summer             18.1         0.382            16.5       0.0765
    ## 5 NOAA      Winter             15.0         0.299            15.0       0.0781
    ## 6 NOAA      Spring             14.6         0.214            15.0       0.0424
    ##   Brood_per_mm
    ##          <dbl>
    ## 1        1.04 
    ## 2        0.947
    ## 3        1.27 
    ## 4        1.10 
    ## 5        1.00 
    ## 6        0.976

#### Brooding Female Analyses

    ## Call:
    ##    aov(formula = Embryos ~ Lake * Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Terms:
    ##                      Lake    Length Lake:Length Residuals
    ## Sum of Squares   3631.972  6703.859      13.379 23422.495
    ## Deg. of Freedom         4         1           4      1085
    ## 
    ## Residual standard error: 4.646241
    ## Estimated effects may be unbalanced
    ## 578 observations deleted due to missingness

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908  42.061 <2e-16 ***
    ## Length         1   6704    6704 310.543 <2e-16 ***
    ## Lake:Length    4     13       3   0.155  0.961    
    ## Residuals   1085  23422      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 578 observations deleted due to missingness

    ## 
    ## Call:
    ## aov(formula = Embryos ~ Lake * Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.1446  -2.8510   0.1858   2.8406  17.9854 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)           9.2866    63.8517   0.145    0.884
    ## LakeOntario         -14.4146    63.9111  -0.226    0.822
    ## LakeMichigan        -18.0450    63.9048  -0.282    0.778
    ## LakeHuron           -20.5549    64.1306  -0.321    0.749
    ## LakeSuperior        -20.1932    63.8865  -0.316    0.752
    ## Length                1.0370     4.1070   0.252    0.801
    ## LakeOntario:Length    0.5085     4.1115   0.124    0.902
    ## LakeMichigan:Length   0.6739     4.1107   0.164    0.870
    ## LakeHuron:Length      0.7621     4.1282   0.185    0.854
    ## LakeSuperior:Length   0.5693     4.1094   0.139    0.890
    ## 
    ## Residual standard error: 4.646 on 1085 degrees of freedom
    ##   (578 observations deleted due to missingness)
    ## Multiple R-squared:  0.3064, Adjusted R-squared:  0.3007 
    ## F-statistic: 53.27 on 9 and 1085 DF,  p-value: < 2.2e-16

    ## Start:  AIC=3373.92
    ## Embryos ~ Lake * Length
    ## 
    ##               Df Sum of Sq   RSS    AIC
    ## - Lake:Length  4    13.379 23436 3366.5
    ## <none>                     23423 3373.9
    ## 
    ## Step:  AIC=3366.55
    ## Embryos ~ Lake + Length
    ## 
    ##          Df Sum of Sq   RSS    AIC
    ## <none>                23436 3366.5
    ## - Lake    4    5279.0 28715 3581.0
    ## - Length  1    6703.9 30140 3640.0

    ## Call:
    ##    aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Terms:
    ##                      Lake    Length Residuals
    ## Sum of Squares   3631.972  6703.859 23435.873
    ## Deg. of Freedom         4         1      1089
    ## 
    ## Residual standard error: 4.639024
    ## Estimated effects may be unbalanced
    ## 578 observations deleted due to missingness

    ## Call:
    ##    aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Terms:
    ##                      Lake    Length Residuals
    ## Sum of Squares   3631.972  6703.859 23435.873
    ## Deg. of Freedom         4         1      1089
    ## 
    ## Residual standard error: 4.639024
    ## Estimated effects may be unbalanced
    ## 578 observations deleted due to missingness

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908   42.19 <2e-16 ***
    ## Length         1   6704    6704  311.51 <2e-16 ***
    ## Residuals   1089  23436      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 578 observations deleted due to missingness

    ## 
    ## Call:
    ## aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3794  -2.8854   0.1987   2.8569  18.0086 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.06308    2.52288   0.025 0.980058    
    ## LakeOntario   -6.40320    2.09711  -3.053 0.002318 ** 
    ## LakeMichigan  -7.63451    2.09768  -3.640 0.000286 ***
    ## LakeHuron     -8.93372    2.14886  -4.157 3.47e-05 ***
    ## LakeSuperior -11.33516    2.08526  -5.436 6.73e-08 ***
    ## Length         1.63056    0.09238  17.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.639 on 1089 degrees of freedom
    ##   (578 observations deleted due to missingness)
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.3029 
    ## F-statistic: 96.06 on 5 and 1089 DF,  p-value: < 2.2e-16

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Cross-Lake%20Analyses-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Cross-Lake%20Analyses-2.png)<!-- -->

    ## Warning in replications(paste("~", xx), data = mf): non-factors ignored: Length

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## $Lake
    ##                          diff        lwr        upr     p adj
    ## Ontario-Erie       -8.5180812 -14.238769 -2.7973932 0.0004863
    ## Michigan-Erie      -8.8788136 -14.607181 -3.1504459 0.0002397
    ## Huron-Erie        -11.0666667 -16.928824 -5.2045092 0.0000029
    ## Superior-Erie     -12.1436399 -17.839939 -6.4473411 0.0000001
    ## Michigan-Ontario   -0.3607324  -1.489298  0.7678329 0.9066716
    ## Huron-Ontario      -2.5485855  -4.229167 -0.8680042 0.0003547
    ## Superior-Ontario   -3.6255587  -4.578073 -2.6730441 0.0000000
    ## Huron-Michigan     -2.1878531  -3.894393 -0.4813131 0.0043573
    ## Superior-Michigan  -3.2648264  -4.262428 -2.2672246 0.0000000
    ## Superior-Huron     -1.0769733  -2.672562  0.5186154 0.3486140

    ## Warning in replications(paste("~", xx), data = mf): non-factors ignored: Length

    ##                         diff        lwr        upr        p adj
    ## Huron-Erie       -11.0666667 -16.928824 -5.2045092 2.939666e-06
    ## Huron-Michigan    -2.1878531  -3.894393 -0.4813131 4.357320e-03
    ## Michigan-Ontario  -0.3607324  -1.489298  0.7678329 9.066716e-01
    ## Superior-Ontario  -3.6255587  -4.578073 -2.6730441 0.000000e+00

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Cross-Lake%20Analyses-3.png)<!-- -->

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Trends%20Analyses-1.png)<!-- -->

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Year          1     38   38.35   1.508   0.22
    ## Residuals   510  12969   25.43               
    ## 133 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Year          1     96   96.31   3.053 0.0819 .
    ## Residuals   235   7413   31.54                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 236 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Year          1  117.3  117.30   5.679 0.0184 *
    ## Residuals   148 3056.8   20.65                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 13 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Year          1    305  304.98   10.47 0.00141 **
    ## Residuals   211   6148   29.14                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 18 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Year          1      2    2.31   0.046  0.831
    ## Residuals   129   6491   50.32               
    ## 12 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Year          1    179  179.40   3.496 0.0632 .
    ## Residuals   171   8775   51.32                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 13 observations deleted due to missingness

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Year         1    0.3   0.268   0.015  0.904
    ## Residuals   70 1281.7  18.310               
    ## 41 observations deleted due to missingness

    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Year          1    650   650.4   22.69 3.11e-06 ***
    ## Residuals   269   7710    28.7                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 166 observations deleted due to missingness

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7135 -3.7135 -0.4467  3.4643 17.5533 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -165.84586  145.85584  -1.137    0.256
    ## Year           0.08893    0.07242   1.228    0.220
    ## 
    ## Residual standard error: 5.043 on 510 degrees of freedom
    ##   (133 observations deleted due to missingness)
    ## Multiple R-squared:  0.002949,   Adjusted R-squared:  0.0009936 
    ## F-statistic: 1.508 on 1 and 510 DF,  p-value: 0.22

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.1587  -4.0686  -0.6136   3.5238  19.5238 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -441.7508   262.2672  -1.684   0.0934 .
    ## Year           0.2275     0.1302   1.747   0.0819 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.616 on 235 degrees of freedom
    ##   (236 observations deleted due to missingness)
    ## Multiple R-squared:  0.01283,    Adjusted R-squared:  0.008626 
    ## F-statistic: 3.053 on 1 and 235 DF,  p-value: 0.08187

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.1809  -2.9954  -0.3182   3.2916  14.4180 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 489.36683  199.54825   2.452   0.0154 *
    ## Year         -0.23627    0.09914  -2.383   0.0184 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.545 on 148 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.03696,    Adjusted R-squared:  0.03045 
    ## F-statistic: 5.679 on 1 and 148 DF,  p-value: 0.01844

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.4580  -3.4580  -0.5941   3.4059  23.4739 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 326.49266   96.41592   3.386 0.000845 ***
    ## Year         -0.15533    0.04801  -3.235 0.001410 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.398 on 211 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.04726,    Adjusted R-squared:  0.04275 
    ## F-statistic: 10.47 on 1 and 211 DF,  p-value: 0.00141

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3831  -4.1562   0.7819   4.6376  15.4932 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -61.78143  387.32921  -0.160    0.874
    ## Year          0.04125    0.19247   0.214    0.831
    ## 
    ## Residual standard error: 7.094 on 129 degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## Multiple R-squared:  0.000356,   Adjusted R-squared:  -0.007393 
    ## F-statistic: 0.04594 on 1 and 129 DF,  p-value: 0.8306

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -17.00  -3.96   1.04   5.04  16.64 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 320.57267  159.76269   2.007   0.0464 *
    ## Year         -0.14869    0.07953  -1.870   0.0632 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.164 on 171 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.02003,    Adjusted R-squared:  0.0143 
    ## F-statistic: 3.496 on 1 and 171 DF,  p-value: 0.06323

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.3258 -3.2184 -0.3818  3.6182 10.7489 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  51.92814  310.89875   0.167    0.868
    ## Year         -0.01868    0.15448  -0.121    0.904
    ## 
    ## Residual standard error: 4.279 on 70 degrees of freedom
    ##   (41 observations deleted due to missingness)
    ## Multiple R-squared:  0.0002088,  Adjusted R-squared:  -0.01407 
    ## F-statistic: 0.01462 on 1 and 70 DF,  p-value: 0.9041

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.892  -3.790  -0.316   3.396  13.651 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -905.01357  193.52951  -4.676 4.63e-06 ***
    ## Year           0.45756    0.09605   4.764 3.11e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.354 on 269 degrees of freedom
    ##   (166 observations deleted due to missingness)
    ## Multiple R-squared:  0.07779,    Adjusted R-squared:  0.07437 
    ## F-statistic: 22.69 on 1 and 269 DF,  p-value: 3.114e-06

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Trends%20Analyses-2.png)<!-- -->

    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Year          1   17.4  17.444   7.736 0.00557 **
    ## Residuals   642 1447.6   2.255                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Year          1  114.5  114.46   38.75 1.07e-09 ***
    ## Residuals   470 1388.3    2.95                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Year          1   29.0  29.039   13.84 0.000275 ***
    ## Residuals   161  337.9   2.098                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##              Df Sum Sq Mean Sq F value  Pr(>F)    
    ## Year          1   41.7   41.72    17.6 3.9e-05 ***
    ## Residuals   229  542.8    2.37                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## Year          1   8.89   8.887   4.007 0.0472 *
    ## Residuals   141 312.70   2.218                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Year          1    1.7   1.699   0.717  0.398
    ## Residuals   184  436.2   2.370

    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Year          1  15.64  15.640   8.622 0.00404 **
    ## Residuals   111 201.36   1.814                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Year          1   17.4  17.378   7.571 0.00618 **
    ## Residuals   434  996.2   2.295                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7651 -1.1234  0.0512  1.0679  4.5479 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -81.89074   34.81724  -2.352  0.01897 * 
    ## Year          0.04810    0.01729   2.781  0.00557 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.502 on 642 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01191,    Adjusted R-squared:  0.01037 
    ## F-statistic: 7.736 on 1 and 642 DF,  p-value: 0.005571

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5039 -1.0943 -0.0943  1.0460  6.5356 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -246.83243   41.97445  -5.881 7.77e-09 ***
    ## Year           0.12988    0.02086   6.225 1.07e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.719 on 470 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07617,    Adjusted R-squared:  0.0742 
    ## F-statistic: 38.75 on 1 and 470 DF,  p-value: 1.068e-09

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.071 -1.052 -0.341  0.757  4.435 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 241.22225   60.87345   3.963 0.000111 ***
    ## Year         -0.11250    0.03024  -3.720 0.000275 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 161 degrees of freedom
    ## Multiple R-squared:  0.07915,    Adjusted R-squared:  0.07343 
    ## F-statistic: 13.84 on 1 and 161 DF,  p-value: 0.0002749

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2545 -1.0088 -0.2802  0.7980  5.1581 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 125.95299   26.45046   4.762  3.4e-06 ***
    ## Year         -0.05525    0.01317  -4.195  3.9e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.54 on 229 degrees of freedom
    ## Multiple R-squared:  0.07137,    Adjusted R-squared:  0.06732 
    ## F-statistic:  17.6 on 1 and 229 DF,  p-value: 3.898e-05

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2817 -1.1458  0.0829  1.0176  4.8485 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 173.18846   77.85833   2.224   0.0277 *
    ## Year         -0.07745    0.03869  -2.002   0.0472 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.489 on 141 degrees of freedom
    ## Multiple R-squared:  0.02764,    Adjusted R-squared:  0.02074 
    ## F-statistic: 4.007 on 1 and 141 DF,  p-value: 0.04722

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3329 -1.0907 -0.0107  1.1138  5.1449 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -11.13875   33.49110  -0.333    0.740
    ## Year          0.01411    0.01667   0.847    0.398
    ## 
    ## Residual standard error: 1.54 on 184 degrees of freedom
    ## Multiple R-squared:  0.003879,   Adjusted R-squared:  -0.001534 
    ## F-statistic: 0.7166 on 1 and 184 DF,  p-value: 0.3984

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9056 -1.0180 -0.0883  0.8695  3.2865 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -211.91722   77.00985  -2.752  0.00692 **
    ## Year           0.11241    0.03828   2.936  0.00404 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.347 on 111 degrees of freedom
    ## Multiple R-squared:  0.07207,    Adjusted R-squared:  0.06371 
    ## F-statistic: 8.622 on 1 and 111 DF,  p-value: 0.004039

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4404 -1.0768 -0.0288  0.9056  4.5360 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -87.28339   36.91025  -2.365  0.01848 * 
    ## Year          0.05045    0.01834   2.752  0.00618 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.515 on 434 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01715,    Adjusted R-squared:  0.01488 
    ## F-statistic: 7.571 on 1 and 434 DF,  p-value: 0.006179

#### Key plots and analyses of Brooding Females data

    ## Warning: Removed 578 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 578 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-1.png)<!-- -->

    ## Warning: Removed 578 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 578 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-2.png)<!-- -->

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908   42.19 <2e-16 ***
    ## Length         1   6704    6704  311.51 <2e-16 ***
    ## Residuals   1089  23436      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 578 observations deleted due to missingness

    ## 
    ## Call:
    ## aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3794  -2.8854   0.1987   2.8569  18.0086 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.06308    2.52288   0.025 0.980058    
    ## LakeOntario   -6.40320    2.09711  -3.053 0.002318 ** 
    ## LakeMichigan  -7.63451    2.09768  -3.640 0.000286 ***
    ## LakeHuron     -8.93372    2.14886  -4.157 3.47e-05 ***
    ## LakeSuperior -11.33516    2.08526  -5.436 6.73e-08 ***
    ## Length         1.63056    0.09238  17.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.639 on 1089 degrees of freedom
    ##   (578 observations deleted due to missingness)
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.3029 
    ## F-statistic: 96.06 on 5 and 1089 DF,  p-value: < 2.2e-16

    ## Warning in replications(paste("~", xx), data = mf): non-factors ignored: Length

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## $Lake
    ##                          diff        lwr        upr     p adj
    ## Ontario-Erie       -8.5180812 -14.238769 -2.7973932 0.0004863
    ## Michigan-Erie      -8.8788136 -14.607181 -3.1504459 0.0002397
    ## Huron-Erie        -11.0666667 -16.928824 -5.2045092 0.0000029
    ## Superior-Erie     -12.1436399 -17.839939 -6.4473411 0.0000001
    ## Michigan-Ontario   -0.3607324  -1.489298  0.7678329 0.9066716
    ## Huron-Ontario      -2.5485855  -4.229167 -0.8680042 0.0003547
    ## Superior-Ontario   -3.6255587  -4.578073 -2.6730441 0.0000000
    ## Huron-Michigan     -2.1878531  -3.894393 -0.4813131 0.0043573
    ## Superior-Michigan  -3.2648264  -4.262428 -2.2672246 0.0000000
    ## Superior-Huron     -1.0769733  -2.672562  0.5186154 0.3486140

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-3.png)<!-- -->

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake  Season Group Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <fct> <fct>  <chr>            <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Erie  Spring GLNP~             26.2          1.93            15.4       0.148 
    ## 2 Onta~ Spring GLNP~             15.9          0.331           14.2       0.0520
    ## 3 Mich~ Spring GLNP~             15.9          0.234           14.6       0.0660
    ## 4 Huron Spring GLNP~             13.9          0.290           14.6       0.0533
    ## 5 Supe~ Spring GLNP~             12.4          0.165           14.9       0.0437
    ## # ... with 1 more variable: Brood_per_mm <dbl>

    ## Warning: Removed 31 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 31 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-4.png)<!-- -->

    ## # A tibble: 2 x 6
    ##   Season Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <fct>             <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Fall               21.7          0.261           17.1       0.0387
    ## 2 Spring             14.6          0.214           15.0       0.0424
    ## # ... with 1 more variable: Brood_per_mm <dbl>

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 3 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-1.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7135 -3.7135 -0.4467  3.4643 17.5533 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -165.84586  145.85584  -1.137    0.256
    ## Year           0.08893    0.07242   1.228    0.220
    ## 
    ## Residual standard error: 5.043 on 510 degrees of freedom
    ##   (133 observations deleted due to missingness)
    ## Multiple R-squared:  0.002949,   Adjusted R-squared:  0.0009936 
    ## F-statistic: 1.508 on 1 and 510 DF,  p-value: 0.22

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.1587  -4.0686  -0.6136   3.5238  19.5238 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -441.7508   262.2672  -1.684   0.0934 .
    ## Year           0.2275     0.1302   1.747   0.0819 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.616 on 235 degrees of freedom
    ##   (236 observations deleted due to missingness)
    ## Multiple R-squared:  0.01283,    Adjusted R-squared:  0.008626 
    ## F-statistic: 3.053 on 1 and 235 DF,  p-value: 0.08187

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.3258 -3.2184 -0.3818  3.6182 10.7489 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  51.92814  310.89875   0.167    0.868
    ## Year         -0.01868    0.15448  -0.121    0.904
    ## 
    ## Residual standard error: 4.279 on 70 degrees of freedom
    ##   (41 observations deleted due to missingness)
    ## Multiple R-squared:  0.0002088,  Adjusted R-squared:  -0.01407 
    ## F-statistic: 0.01462 on 1 and 70 DF,  p-value: 0.9041

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.892  -3.790  -0.316   3.396  13.651 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -905.01357  193.52951  -4.676 4.63e-06 ***
    ## Year           0.45756    0.09605   4.764 3.11e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.354 on 269 degrees of freedom
    ##   (166 observations deleted due to missingness)
    ## Multiple R-squared:  0.07779,    Adjusted R-squared:  0.07437 
    ## F-statistic: 22.69 on 1 and 269 DF,  p-value: 3.114e-06

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-2.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7651 -1.1234  0.0512  1.0679  4.5479 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -81.89074   34.81724  -2.352  0.01897 * 
    ## Year          0.04810    0.01729   2.781  0.00557 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.502 on 642 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01191,    Adjusted R-squared:  0.01037 
    ## F-statistic: 7.736 on 1 and 642 DF,  p-value: 0.005571

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5039 -1.0943 -0.0943  1.0460  6.5356 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -246.83243   41.97445  -5.881 7.77e-09 ***
    ## Year           0.12988    0.02086   6.225 1.07e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.719 on 470 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07617,    Adjusted R-squared:  0.0742 
    ## F-statistic: 38.75 on 1 and 470 DF,  p-value: 1.068e-09

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9056 -1.0180 -0.0883  0.8695  3.2865 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -211.91722   77.00985  -2.752  0.00692 **
    ## Year           0.11241    0.03828   2.936  0.00404 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.347 on 111 degrees of freedom
    ## Multiple R-squared:  0.07207,    Adjusted R-squared:  0.06371 
    ## F-statistic: 8.622 on 1 and 111 DF,  p-value: 0.004039

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4404 -1.0768 -0.0288  0.9056  4.5360 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -87.28339   36.91025  -2.365  0.01848 * 
    ## Year          0.05045    0.01834   2.752  0.00618 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.515 on 434 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01715,    Adjusted R-squared:  0.01488 
    ## F-statistic: 7.571 on 1 and 434 DF,  p-value: 0.006179

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-3.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.1809  -2.9954  -0.3182   3.2916  14.4180 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 489.36683  199.54825   2.452   0.0154 *
    ## Year         -0.23627    0.09914  -2.383   0.0184 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.545 on 148 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.03696,    Adjusted R-squared:  0.03045 
    ## F-statistic: 5.679 on 1 and 148 DF,  p-value: 0.01844

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.4580  -3.4580  -0.5941   3.4059  23.4739 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 326.49266   96.41592   3.386 0.000845 ***
    ## Year         -0.15533    0.04801  -3.235 0.001410 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.398 on 211 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.04726,    Adjusted R-squared:  0.04275 
    ## F-statistic: 10.47 on 1 and 211 DF,  p-value: 0.00141

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3831  -4.1562   0.7819   4.6376  15.4932 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -61.78143  387.32921  -0.160    0.874
    ## Year          0.04125    0.19247   0.214    0.831
    ## 
    ## Residual standard error: 7.094 on 129 degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## Multiple R-squared:  0.000356,   Adjusted R-squared:  -0.007393 
    ## F-statistic: 0.04594 on 1 and 129 DF,  p-value: 0.8306

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -17.00  -3.96   1.04   5.04  16.64 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 320.57267  159.76269   2.007   0.0464 *
    ## Year         -0.14869    0.07953  -1.870   0.0632 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.164 on 171 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.02003,    Adjusted R-squared:  0.0143 
    ## F-statistic: 3.496 on 1 and 171 DF,  p-value: 0.06323

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.071 -1.052 -0.341  0.757  4.435 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 241.22225   60.87345   3.963 0.000111 ***
    ## Year         -0.11250    0.03024  -3.720 0.000275 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 161 degrees of freedom
    ## Multiple R-squared:  0.07915,    Adjusted R-squared:  0.07343 
    ## F-statistic: 13.84 on 1 and 161 DF,  p-value: 0.0002749

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2545 -1.0088 -0.2802  0.7980  5.1581 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 125.95299   26.45046   4.762  3.4e-06 ***
    ## Year         -0.05525    0.01317  -4.195  3.9e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.54 on 229 degrees of freedom
    ## Multiple R-squared:  0.07137,    Adjusted R-squared:  0.06732 
    ## F-statistic:  17.6 on 1 and 229 DF,  p-value: 3.898e-05

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2817 -1.1458  0.0829  1.0176  4.8485 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 173.18846   77.85833   2.224   0.0277 *
    ## Year         -0.07745    0.03869  -2.002   0.0472 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.489 on 141 degrees of freedom
    ## Multiple R-squared:  0.02764,    Adjusted R-squared:  0.02074 
    ## F-statistic: 4.007 on 1 and 141 DF,  p-value: 0.04722

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3329 -1.0907 -0.0107  1.1138  5.1449 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -11.13875   33.49110  -0.333    0.740
    ## Year          0.01411    0.01667   0.847    0.398
    ## 
    ## Residual standard error: 1.54 on 184 degrees of freedom
    ## Multiple R-squared:  0.003879,   Adjusted R-squared:  -0.001534 
    ## F-statistic: 0.7166 on 1 and 184 DF,  p-value: 0.3984

### Cohort Lengths Data Plotting and Analysis

#### Compile a mysids lengths tibble

``` r
Mysids_Lengths <- 
  bind_rows(
    GLNPO_Zoop_Lengths %>% 
      mutate(Group = "GLNPO_Zoop") %>% 
      select(everything(), Length = Length_mm),
    GLNPO_Mysid_Lengths %>% 
      mutate(Group = "GLNPO_Mys"),
    USGS_MI_HU_Lengths %>% 
      modify_at("Visit", as.character) %>% 
      mutate(Group = "USGS") %>% 
      mutate(Visit = paste(Group, Visit, sep = "_")),
    NOAA_Lengths %>% 
      mutate(Group = "NOAA"),
  ) %>% 
  mutate(VisPool = paste(Lake, Year, Season, Group, sep = "_")) %>% 
  group_by(VisPool)

write_csv(Mysids_Lengths, "Lengths_Data_Rmd_Output.csv")

Mysids_Lengths
```

<br>

``` r
Mysid_Mclust_Fits <- 
  
  Mysids_Lengths %>% 

  split(.$VisPool) %>%
  
    lapply(
      
      function(df){
        
        if(length(df$Length) >= 10){
          
          Mclust(df$Length, G = c(1:2), modelNames = "E")
          
        } else {
          
          df$Length
          
        }
        
      }
      
    )

# Mysid_Mclust_Fits

#

Extract_Mclust_values <- 
  
  function(Fit){
    
    if(Fit$G == 1){
      
      data.frame(
        Mean_1 = Fit$parameters$mean,
        Mean_2 = NA,
        Prop_1 = 1,
        Prop_2 = NA
      )
      
    } else if(Fit$G == 2){
      
      data.frame(
        Mean_1 = Fit$parameters$mean[1],
        Mean_2 = Fit$parameters$mean[2],
        Prop_1 = Fit$parameters$pro[1],
        Prop_2 = Fit$parameters$pro[2]
      )
      
    }
    
  }


Mysid_Mclust_Fit_Summaries <- 
  
  Mysid_Mclust_Fits %>% 
  
  lapply(
    
    function(Obj){
      
      if(class(Obj) == "Mclust"){
        
        Extract_Mclust_values(Obj)
        
      } else {
        
        data.frame(
          Mean_1 = mean(Obj, na.rm = TRUE),
          Mean_2 = NA,
          Prop_1 = 1,
          Prop_2 = NA
        )
        
      }
      
    }
    
  )

Mysid_Mclust_Fit_Summaries <- 
  
  bind_rows(Mysid_Mclust_Fit_Summaries) %>% 
  
  mutate(VisPool = names(Mysid_Mclust_Fit_Summaries)) %>% 
  
  select(VisPool, 
         
         Mean_Length_1 = Mean_1, 
         
         Mean_Length_2 = Mean_2, 
         
         Prop_1, Prop_2)


# Mysid_Mclust_Fit_Summaries


Mysid_SizeStruct <- 
  
  Mysids_Lengths %>% 
  
  group_by(Lake, Year, Season, Group, VisPool) %>% 
  
  summarize(N_Length = length(Length),
            
            Mean_Length_mm = mean(Length),
            
            Mean_Mass_mg = 1000 * mean(exp(-12.27 + 2.72 * log(Length)))
            
  ) %>% 
  
  left_join(Mysid_Mclust_Fit_Summaries)


# Mysid_SizeStruct


Mysid_SizeStruct <- 
  
  Mysid_SizeStruct %>% 
  
  left_join(
    
    Mysids %>% 
      
      left_join(Mysids_Lengths %>% select(Visit, VisPool), by = "Visit") %>% 
      
      group_by(VisPool) %>% 
      
      summarize(MeanDens = mean(Dens, na.rm = T))
    
    ) %>% 
  
  mutate(Dens1 = Prop_1 * MeanDens,
         Dens2 = Prop_2 * MeanDens) %>% 
  
  left_join(Mysids_Lines_DensPred) %>% 
  select(everything(), Dens_GAM = Dens_Pred) %>% 
  
  mutate(Dens1_GAM = Prop_1 * Dens_GAM,
         Dens2_GAM = Prop_2 * Dens_GAM)


Mysid_SizeStruct
```

``` r
Mysid_SizeStruct <- 
  
  Mysid_SizeStruct %>% 
  
  
  filter(Lake != "Erie",
         N_Length >= 10) %>% 
  
  
  filter(Season != "Summer" | 
           Lake != "Michigan" |
           Year != 2002) %>% 
  
  
  filter(Season != "Summer" | 
           Group != "GLNPO_Zoop" |
           
           (Lake == "Huron" &
               Year %in% c(1997, 2000 + c(0:1, 3:5, 8, 10:12, 15:16))) |
           
           (Lake == "Michigan" &
               Year %in% c(1998, 2000 + c(1:4, 7:10, 13:18))) |
           
           (Lake == "Ontario" &
               Year %in% c(2001, 2004:2018)) |
           
           (Lake == "Superior" &
               Year %in% c(1997:1998, 2000:2018))
          ) %>% 
  
  
  filter(Season != "Spring" | 
           Group != "GLNPO_Mysid" |
           
           (Lake == "Huron" &
               Year %in% c(2000 + c(6:7, 10:14, 16:18))) |
           
           (Lake == "Michigan" &
               Year %in% c(2000 + c(6:7, 9:13, 18))) |
           
           (Lake == "Ontario" &
               Year %in% c(2006:2019)) |
           
           (Lake == "Superior" &
               Year %in% c(2000 + c(8, 10:16, 18:19)))
          ) %>% 
  
  
  filter(Season != "Spring" | 
           Group != "GLNPO_Zoop" |
           
           (Lake == "Huron" &
               Year %in% c(2000 + c(3:5, 7:8, 17))) |
           
           (Lake == "Michigan" &
               Year %in% c(2000 + c(1:2, 5:10, 12, 15:17))) |
           
           (Lake == "Ontario" &
               Year %in% c(2000 + c(1:10, 13:17))) |
           
           (Lake == "Superior" &
               Year %in% c(1998, 2000 + c(2:16, 18)))
          ) %>% 
  
  
  # filter(Season != "Summer" | 
  #          Group != "USGS" |
  #          Year != 2012
  #          ) %>% 
  
  
  filter(Season != "Fall" | 
           Group != "NOAA" |
           !(Year %in% c(1996, 2013, 2018, 2019))
           )
```

#### Size Structure plots

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-5.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-6.png)<!-- -->

#### Table of Growth Rates by Lake (excluding Lake Erie)

    ## # A tibble: 4 x 7
    ##   Lake     N_Years Mean_Length_1 Mean_Length_2 Total_Growth
    ##   <chr>      <int>         <dbl>         <dbl>        <dbl>
    ## 1 Huron         14          5.26          12.5         9.52
    ## 2 Michigan      14          5.44          13.1        10.1 
    ## 3 Ontario       13          6.46          14.4        11.4 
    ## 4 Superior      13          5.01          12.1         9.10
    ##   Overall_Rate_per_month Measured_Rate_per_month
    ##                    <dbl>                   <dbl>
    ## 1                  0.680                   0.604
    ## 2                  0.723                   0.640
    ## 3                  0.815                   0.663
    ## 4                  0.650                   0.591

#### Lake Erie Rates

![](GLNPO_Long_term_2019_files/figure-gfm/Lake%20Erie%20Rates-1.png)<!-- -->

    ## # A tibble: 134 x 10
    ##     Visit   Sex      Stage Count Length Embryos  Year Lake  Season SeasonSex    
    ##     <chr>   <chr>    <chr> <dbl>  <dbl>   <dbl> <dbl> <fct> <fct>  <chr>        
    ##   1 E15MM12 Female   Early     1  15         30  2012 Erie  Spring SpringFemale 
    ##   2 E15MG13 Juvenile <NA>      1   4.63      NA  2013 Erie  Summer SummerJuveni~
    ##   3 E063M14 Female   Mid       1  16.5       30  2014 Erie  Spring SpringFemale 
    ##   4 E15MM14 Female   Early     1  15.3       28  2014 Erie  Spring SpringFemale 
    ##   5 E15MG07 Juvenile <NA>      1   4.5       NA  2007 Erie  Summer SummerJuveni~
    ##   6 E15MG07 Juvenile <NA>      1   4         NA  2007 Erie  Summer SummerJuveni~
    ##   7 E15MG07 Juvenile <NA>      1   4.5       NA  2007 Erie  Summer SummerJuveni~
    ##   8 E15MG07 Juvenile <NA>      1   5         NA  2007 Erie  Summer SummerJuveni~
    ##   9 E009G08 Juvenile <NA>      1   4.2       NA  2008 Erie  Summer SummerJuveni~
    ##  10 E009G08 Juvenile <NA>      1   4         NA  2008 Erie  Summer SummerJuveni~
    ##  11 E009G08 Juvenile <NA>      1   6.6       NA  2008 Erie  Summer SummerJuveni~
    ##  12 E009G08 Juvenile <NA>      1   5         NA  2008 Erie  Summer SummerJuveni~
    ##  13 E009G08 Juvenile <NA>      1   4         NA  2008 Erie  Summer SummerJuveni~
    ##  14 E009G08 Juvenile <NA>      1   4         NA  2008 Erie  Summer SummerJuveni~
    ##  15 E009G08 Juvenile <NA>      1   4         NA  2008 Erie  Summer SummerJuveni~
    ##  16 E15MG08 Female   <NA>      1  15.5       NA  2008 Erie  Summer SummerFemale 
    ##  17 E15MG08 Juvenile <NA>      1   5         NA  2008 Erie  Summer SummerJuveni~
    ##  18 E15MG08 Juvenile <NA>      1   4.5       NA  2008 Erie  Summer SummerJuveni~
    ##  19 E010G10 Juvenile <NA>      1   4.7       NA  2010 Erie  Summer SummerJuveni~
    ##  20 E93bG10 Juvenile <NA>      1   4         NA  2010 Erie  Summer SummerJuveni~
    ##  21 E009G15 Juvenile <NA>      1   4.86      NA  2015 Erie  Summer SummerJuveni~
    ##  22 E009G15 Juvenile <NA>      1   3.70      NA  2015 Erie  Summer SummerJuveni~
    ##  23 E009G15 Juvenile <NA>      1   5.88      NA  2015 Erie  Summer SummerJuveni~
    ##  24 E009G15 Juvenile <NA>      1   7.32      NA  2015 Erie  Summer SummerJuveni~
    ##  25 E009G15 Juvenile <NA>      1   3.68      NA  2015 Erie  Summer SummerJuveni~
    ##  26 E009G15 Juvenile <NA>      1   4.89      NA  2015 Erie  Summer SummerJuveni~
    ##  27 E009G15 Juvenile <NA>      1   3.81      NA  2015 Erie  Summer SummerJuveni~
    ##  28 E009G15 Juvenile <NA>      1   5.11      NA  2015 Erie  Summer SummerJuveni~
    ##  29 E009G15 Juvenile <NA>      1   3.78      NA  2015 Erie  Summer SummerJuveni~
    ##  30 E009G15 Juvenile <NA>      1   3.25      NA  2015 Erie  Summer SummerJuveni~
    ##  31 E009G15 Juvenile <NA>      1   4.75      NA  2015 Erie  Summer SummerJuveni~
    ##  32 E009G15 Juvenile <NA>      1   4.51      NA  2015 Erie  Summer SummerJuveni~
    ##  33 E009G15 Juvenile <NA>      1   3.93      NA  2015 Erie  Summer SummerJuveni~
    ##  34 E009G15 Juvenile <NA>      1   4.10      NA  2015 Erie  Summer SummerJuveni~
    ##  35 E009G15 Juvenile <NA>      1   5.22      NA  2015 Erie  Summer SummerJuveni~
    ##  36 E009G15 Juvenile <NA>      1   4.26      NA  2015 Erie  Summer SummerJuveni~
    ##  37 E009G15 Juvenile <NA>      1   3.84      NA  2015 Erie  Summer SummerJuveni~
    ##  38 E009M18 Female   Early     1  15.3       22  2018 Erie  Spring SpringFemale 
    ##  39 E009M18 Male     Matu~     1  12.6       NA  2018 Erie  Spring SpringMale   
    ##  40 E009M18 Female   Early     1  15.6       17  2018 Erie  Spring SpringFemale 
    ##  41 E009M18 Male     Matu~     1  13.3       NA  2018 Erie  Spring SpringMale   
    ##  42 E009G18 Juvenile <NA>      1   3.19      NA  2018 Erie  Summer SummerJuveni~
    ##  43 E009G18 Juvenile <NA>      1   3.60      NA  2018 Erie  Summer SummerJuveni~
    ##  44 E009G18 Juvenile <NA>      1   3.26      NA  2018 Erie  Summer SummerJuveni~
    ##  45 E009G18 Juvenile <NA>      1   4.27      NA  2018 Erie  Summer SummerJuveni~
    ##  46 E009G18 Juvenile <NA>      1   3.40      NA  2018 Erie  Summer SummerJuveni~
    ##  47 E009G18 Juvenile <NA>      1   3.02      NA  2018 Erie  Summer SummerJuveni~
    ##  48 E009G18 Juvenile <NA>      1   2.87      NA  2018 Erie  Summer SummerJuveni~
    ##  49 E009G18 Juvenile <NA>      1   4.66      NA  2018 Erie  Summer SummerJuveni~
    ##  50 E009G18 Juvenile <NA>      1   3.37      NA  2018 Erie  Summer SummerJuveni~
    ##  51 E009G18 Juvenile <NA>      1   3.64      NA  2018 Erie  Summer SummerJuveni~
    ##  52 E009G18 Juvenile <NA>      1   3.90      NA  2018 Erie  Summer SummerJuveni~
    ##  53 E009G18 Juvenile <NA>      1   3.84      NA  2018 Erie  Summer SummerJuveni~
    ##  54 E009G18 Juvenile <NA>      1   4.65      NA  2018 Erie  Summer SummerJuveni~
    ##  55 E009G18 Juvenile <NA>      1   3.52      NA  2018 Erie  Summer SummerJuveni~
    ##  56 E009G18 Juvenile <NA>      1   3.69      NA  2018 Erie  Summer SummerJuveni~
    ##  57 E009G18 Juvenile <NA>      1   3.17      NA  2018 Erie  Summer SummerJuveni~
    ##  58 E009G18 Juvenile <NA>      1   4.12      NA  2018 Erie  Summer SummerJuveni~
    ##  59 E009G18 Juvenile <NA>      1   3.51      NA  2018 Erie  Summer SummerJuveni~
    ##  60 E009G18 Juvenile <NA>      1   3.38      NA  2018 Erie  Summer SummerJuveni~
    ##  61 E009G18 Juvenile <NA>      1   2.90      NA  2018 Erie  Summer SummerJuveni~
    ##  62 E009G18 Juvenile <NA>      1   4.79      NA  2018 Erie  Summer SummerJuveni~
    ##  63 E009G18 Juvenile <NA>      1   3.55      NA  2018 Erie  Summer SummerJuveni~
    ##  64 E009G18 Juvenile <NA>      1   7.91      NA  2018 Erie  Summer SummerJuveni~
    ##  65 E009G18 Juvenile <NA>      1   3.69      NA  2018 Erie  Summer SummerJuveni~
    ##  66 E009G18 Juvenile <NA>      1   4.06      NA  2018 Erie  Summer SummerJuveni~
    ##  67 E009G18 Juvenile <NA>      1   4.68      NA  2018 Erie  Summer SummerJuveni~
    ##  68 E009G18 Juvenile <NA>      1   3.75      NA  2018 Erie  Summer SummerJuveni~
    ##  69 E009G18 Juvenile <NA>      1   3.75      NA  2018 Erie  Summer SummerJuveni~
    ##  70 E009G18 Juvenile <NA>      1   4.19      NA  2018 Erie  Summer SummerJuveni~
    ##  71 E009G18 Juvenile <NA>      1   3.78      NA  2018 Erie  Summer SummerJuveni~
    ##  72 E009G18 Juvenile <NA>      1   3.96      NA  2018 Erie  Summer SummerJuveni~
    ##  73 E15MG18 Juvenile <NA>      1   3.97      NA  2018 Erie  Summer SummerJuveni~
    ##  74 E15MG18 Juvenile <NA>      1   3.39      NA  2018 Erie  Summer SummerJuveni~
    ##  75 E15MG18 Juvenile <NA>      1   2.82      NA  2018 Erie  Summer SummerJuveni~
    ##  76 E15MG18 Juvenile <NA>      1   2.87      NA  2018 Erie  Summer SummerJuveni~
    ##  77 E15MG18 Juvenile <NA>      1   3.26      NA  2018 Erie  Summer SummerJuveni~
    ##  78 E15MG18 Juvenile <NA>      1   2.65      NA  2018 Erie  Summer SummerJuveni~
    ##  79 E15MG18 Juvenile <NA>      1   5.58      NA  2018 Erie  Summer SummerJuveni~
    ##  80 E15MG18 Juvenile <NA>      1   3.65      NA  2018 Erie  Summer SummerJuveni~
    ##  81 E15MG18 Juvenile <NA>      1   3.16      NA  2018 Erie  Summer SummerJuveni~
    ##  82 E15MG18 Juvenile <NA>      1   3.19      NA  2018 Erie  Summer SummerJuveni~
    ##  83 E15MG18 Juvenile <NA>      1   3.03      NA  2018 Erie  Summer SummerJuveni~
    ##  84 E15MG18 Juvenile <NA>      1   3.34      NA  2018 Erie  Summer SummerJuveni~
    ##  85 E15MG18 Juvenile <NA>      1   3.68      NA  2018 Erie  Summer SummerJuveni~
    ##  86 E15MG18 Juvenile <NA>      1   3.66      NA  2018 Erie  Summer SummerJuveni~
    ##  87 E15MG18 Juvenile <NA>      1   3.76      NA  2018 Erie  Summer SummerJuveni~
    ##  88 E15MG18 Juvenile <NA>      1   3.64      NA  2018 Erie  Summer SummerJuveni~
    ##  89 E15MG18 Juvenile <NA>      1   3.18      NA  2018 Erie  Summer SummerJuveni~
    ##  90 E15MG18 Juvenile <NA>      1   3.52      NA  2018 Erie  Summer SummerJuveni~
    ##  91 E15MG18 Juvenile <NA>      1   2.91      NA  2018 Erie  Summer SummerJuveni~
    ##  92 E15MG18 Juvenile <NA>      1   3.26      NA  2018 Erie  Summer SummerJuveni~
    ##  93 E15MG18 Juvenile <NA>      1   2.87      NA  2018 Erie  Summer SummerJuveni~
    ##  94 E15MG18 Juvenile <NA>      1   3.26      NA  2018 Erie  Summer SummerJuveni~
    ##  95 E15MG18 Juvenile <NA>      1   3.04      NA  2018 Erie  Summer SummerJuveni~
    ##  96 E063G18 Juvenile <NA>      1   4.89      NA  2018 Erie  Summer SummerJuveni~
    ##  97 E063G18 Juvenile <NA>      1   3.41      NA  2018 Erie  Summer SummerJuveni~
    ##  98 E063G18 Juvenile <NA>      1   3.66      NA  2018 Erie  Summer SummerJuveni~
    ##  99 E063G18 Juvenile <NA>      1   3.4       NA  2018 Erie  Summer SummerJuveni~
    ## 100 E063G18 Juvenile <NA>      1   3.84      NA  2018 Erie  Summer SummerJuveni~
    ## 101 E063G18 Juvenile <NA>      1   3.79      NA  2018 Erie  Summer SummerJuveni~
    ## 102 E063G18 Juvenile <NA>      1   3.33      NA  2018 Erie  Summer SummerJuveni~
    ## 103 E063G18 Juvenile <NA>      1   5.13      NA  2018 Erie  Summer SummerJuveni~
    ## 104 E063G18 Juvenile <NA>      1   3.31      NA  2018 Erie  Summer SummerJuveni~
    ## 105 E063G18 Juvenile <NA>      1   4.33      NA  2018 Erie  Summer SummerJuveni~
    ## 106 E063G18 Juvenile <NA>      1   3.44      NA  2018 Erie  Summer SummerJuveni~
    ## 107 E063G18 Juvenile <NA>      1   6.90      NA  2018 Erie  Summer SummerJuveni~
    ## 108 E009G19 Juvenile <NA>      1   3.31      NA  2019 Erie  Summer SummerJuveni~
    ## 109 E009G19 Juvenile <NA>      1   3.19      NA  2019 Erie  Summer SummerJuveni~
    ## 110 E009G19 Juvenile <NA>      1   3.58      NA  2019 Erie  Summer SummerJuveni~
    ## 111 E009G19 Juvenile <NA>      1   3.99      NA  2019 Erie  Summer SummerJuveni~
    ## 112 E009G19 Juvenile <NA>      1   3.79      NA  2019 Erie  Summer SummerJuveni~
    ## 113 E009G19 Juvenile <NA>      1   2.65      NA  2019 Erie  Summer SummerJuveni~
    ## 114 E009G19 Juvenile <NA>      1   3.64      NA  2019 Erie  Summer SummerJuveni~
    ## 115 E009G19 Juvenile <NA>      1   3.61      NA  2019 Erie  Summer SummerJuveni~
    ## 116 E009G19 Juvenile <NA>      1   3.46      NA  2019 Erie  Summer SummerJuveni~
    ## 117 E009G19 Juvenile <NA>      1   2.37      NA  2019 Erie  Summer SummerJuveni~
    ## 118 E009G19 Juvenile <NA>      1   4.37      NA  2019 Erie  Summer SummerJuveni~
    ## 119 E009G19 Juvenile <NA>      1   3.35      NA  2019 Erie  Summer SummerJuveni~
    ## 120 E009G19 Juvenile <NA>      1   3.36      NA  2019 Erie  Summer SummerJuveni~
    ## 121 E009G19 Juvenile <NA>      1   3.28      NA  2019 Erie  Summer SummerJuveni~
    ## 122 E009G19 Juvenile <NA>      1   3.65      NA  2019 Erie  Summer SummerJuveni~
    ## 123 E009G19 Juvenile <NA>      1   3.82      NA  2019 Erie  Summer SummerJuveni~
    ## 124 E009G19 Juvenile <NA>      1   3.75      NA  2019 Erie  Summer SummerJuveni~
    ## 125 E009G19 Juvenile <NA>      1   3.23      NA  2019 Erie  Summer SummerJuveni~
    ## 126 E009G19 Juvenile <NA>      1   3.45      NA  2019 Erie  Summer SummerJuveni~
    ## 127 E009G19 Juvenile <NA>      1   3.52      NA  2019 Erie  Summer SummerJuveni~
    ## 128 E009G19 Juvenile <NA>      1   3.79      NA  2019 Erie  Summer SummerJuveni~
    ## 129 E93bG19 Juvenile <NA>      1   4.39      NA  2019 Erie  Summer SummerJuveni~
    ## 130 E93bG19 Juvenile <NA>      1   2.98      NA  2019 Erie  Summer SummerJuveni~
    ## 131 E93bG19 Juvenile <NA>      1   2.82      NA  2019 Erie  Summer SummerJuveni~
    ## 132 E93bG19 Juvenile <NA>      1   3.66      NA  2019 Erie  Summer SummerJuveni~
    ## 133 E93bG19 Juvenile <NA>      1   3.04      NA  2019 Erie  Summer SummerJuveni~
    ## 134 E010A19 Male     Matu~     1  12.5       NA  2019 Erie  Spring SpringMale

    ## # A tibble: 3 x 7
    ##   Season Cohort Mean_Length N_Length Month  Growth Mortality
    ##   <fct>  <fct>        <dbl>    <int> <dbl>   <dbl>     <dbl>
    ## 1 Summer Age-0         3.89      125     0 NaN         0    
    ## 2 Spring Age-1+       14.5         8     8   1.33      0.984
    ## 3 Summer Age-1+       15.5         1    12   0.967     0.992

    ## [1] "Proportion" "93.3"

    ## [1] "Proportion,  Spring" "94"

<br> <br>

-----

### Growth and Survival estimates

#### Age Structure, Mortality, and Growth: Cross-Lakes Comparisons

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Lake         3 0.04579 0.015262   2.708  0.056 .
    ## Residuals   46 0.25922 0.005635                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Call:
    ## aov(formula = Prop_1 ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.23027 -0.04557  0.01529  0.05056  0.11409 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.71634    0.02167  33.056  < 2e-16 ***
    ## LakeMichigan  0.04440    0.03005   1.478  0.14635    
    ## LakeHuron     0.05149    0.03005   1.713  0.09338 .  
    ## LakeOntario   0.08680    0.03065   2.832  0.00684 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07507 on 46 degrees of freedom
    ## Multiple R-squared:  0.1501, Adjusted R-squared:  0.09469 
    ## F-statistic: 2.708 on 3 and 46 DF,  p-value: 0.056

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Prop_1 ~ Lake, data = SizeStructAnnualSummary %>% filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## $Lake
    ##                          diff          lwr        upr     p adj
    ## Michigan-Superior 0.044401511 -0.035699632 0.12450265 0.4590602
    ## Huron-Superior    0.051488582 -0.028612560 0.13158972 0.3285925
    ## Ontario-Superior  0.086795329  0.005107872 0.16848279 0.0333822
    ## Huron-Michigan    0.007087071 -0.071395699 0.08556984 0.9950263
    ## Ontario-Michigan  0.042393818 -0.037707324 0.12249496 0.4992608
    ## Ontario-Huron     0.035306747 -0.044794395 0.11540789 0.6455243

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Lake         3   2154   718.0   2.168  0.105
    ## Residuals   46  15230   331.1

    ## 
    ## Call:
    ## aov(formula = Surv_rate ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.442 -14.697  -3.328  12.056  44.593 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    45.126      5.253   8.591 4.04e-11 ***
    ## LakeMichigan  -15.780      7.284  -2.166   0.0355 *  
    ## LakeHuron     -12.984      7.284  -1.782   0.0813 .  
    ## LakeOntario   -16.503      7.429  -2.222   0.0313 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.2 on 46 degrees of freedom
    ## Multiple R-squared:  0.1239, Adjusted R-squared:  0.06676 
    ## F-statistic: 2.168 on 3 and 46 DF,  p-value: 0.1046

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Surv_rate ~ Lake, data = SizeStructAnnualSummary %>% filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## $Lake
    ##                          diff       lwr       upr     p adj
    ## Michigan-Superior -15.7798562 -35.19599  3.636278 0.1480405
    ## Huron-Superior    -12.9836535 -32.39979  6.432481 0.2947201
    ## Ontario-Superior  -16.5027003 -36.30335  3.297949 0.1326036
    ## Huron-Michigan      2.7962027 -16.22765 21.820051 0.9793454
    ## Ontario-Michigan   -0.7228441 -20.13898 18.693290 0.9996447
    ## Ontario-Huron      -3.5190468 -22.93518 15.897087 0.9624670

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Lake         3   2154   718.0   2.168  0.105
    ## Residuals   46  15230   331.1

    ## 
    ## Call:
    ## aov(formula = Mort_rate ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.593 -12.056   3.328  14.697  26.442 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    54.874      5.253  10.447 9.95e-14 ***
    ## LakeMichigan   15.780      7.284   2.166   0.0355 *  
    ## LakeHuron      12.984      7.284   1.782   0.0813 .  
    ## LakeOntario    16.503      7.429   2.222   0.0313 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.2 on 46 degrees of freedom
    ## Multiple R-squared:  0.1239, Adjusted R-squared:  0.06676 
    ## F-statistic: 2.168 on 3 and 46 DF,  p-value: 0.1046

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Mort_rate ~ Lake, data = SizeStructAnnualSummary %>% filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## $Lake
    ##                         diff        lwr      upr     p adj
    ## Michigan-Superior 15.7798562  -3.636278 35.19599 0.1480405
    ## Huron-Superior    12.9836535  -6.432481 32.39979 0.2947201
    ## Ontario-Superior  16.5027003  -3.297949 36.30335 0.1326036
    ## Huron-Michigan    -2.7962027 -21.820051 16.22765 0.9793454
    ## Ontario-Michigan   0.7228441 -18.693290 20.13898 0.9996447
    ## Ontario-Huron      3.5190468 -15.897087 22.93518 0.9624670

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Lake         3 0.02627 0.008757   2.233  0.097 .
    ## Residuals   46 0.18042 0.003922                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Call:
    ## aov(formula = Growth_Calc ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.147220 -0.047035  0.007684  0.043675  0.115751 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.58852    0.01808  32.553   <2e-16 ***
    ## LakeMichigan  0.04669    0.02507   1.862   0.0689 .  
    ## LakeHuron     0.01566    0.02507   0.625   0.5352    
    ## LakeOntario   0.05766    0.02557   2.255   0.0289 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06263 on 46 degrees of freedom
    ## Multiple R-squared:  0.1271, Adjusted R-squared:  0.07018 
    ## F-statistic: 2.233 on 3 and 46 DF,  p-value: 0.09704

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Growth_Calc ~ Lake, data = SizeStructAnnualSummary %>% filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## $Lake
    ##                          diff         lwr        upr     p adj
    ## Michigan-Superior  0.04669123 -0.02013530 0.11351777 0.2583139
    ## Huron-Superior     0.01566289 -0.05116365 0.08248943 0.9236062
    ## Ontario-Superior   0.05765640 -0.01049356 0.12580637 0.1238704
    ## Huron-Michigan    -0.03102834 -0.09650471 0.03444802 0.5906242
    ## Ontario-Michigan   0.01096517 -0.05586137 0.07779171 0.9716882
    ## Ontario-Huron      0.04199352 -0.02483302 0.10882005 0.3483423

    ## # A tibble: 8 x 8
    ## # Groups:   Group, Lake [4]
    ##   Group Lake  Season Surv_Mean Surv_2_Measured Growth_Mean Overall_Rate_pe~
    ##   <chr> <ord> <chr>      <dbl>           <dbl>       <dbl>            <dbl>
    ## 1 GLNP~ Onta~ Spring      NA              88.2        0.68            0.619
    ## 2 GLNP~ Mich~ Spring      NA             105.         0.56            0.565
    ## 3 GLNP~ Huron Spring      NA             145.         0.5             0.557
    ## 4 GLNP~ Supe~ Spring      NA             149.         0.45            0.527
    ## 5 GLNP~ Onta~ Summer      28.6            24.5        0.65            0.830
    ## 6 GLNP~ Mich~ Summer      29.4            31.5        0.64            0.723
    ## 7 GLNP~ Huron Summer      32.1            30.2        0.6             0.680
    ## 8 GLNP~ Supe~ Summer      45.1            39.6        0.59            0.651
    ## # ... with 1 more variable: Measured_Rate_per_month <dbl>

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-1.png)<!-- -->

    ## Warning: Removed 4 rows containing missing values (geom_point).
    
    ## Warning: Removed 4 row(s) containing missing values (geom_path).

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-3.png)<!-- -->

#### Age Structure, Mortality, and Growth: Time Series

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-1.png)<!-- -->

    ##             Df   Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.000011 1.12e-05   0.004  0.952
    ## Residuals   10 0.029103 2.91e-03

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00217 0.002175   0.436  0.523
    ## Residuals   11 0.05488 0.004990

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00115 0.001154   0.214  0.656
    ## Residuals    8 0.04318 0.005397               
    ## 1 observation deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00145 0.001447    0.28  0.605
    ## Residuals   15 0.07765 0.005177               
    ## 1 observation deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00312 0.003121   0.506  0.493
    ## Residuals   10 0.06173 0.006173

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Year         1 0.01816 0.018159   3.682 0.0813 .
    ## Residuals   11 0.05425 0.004932                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00429 0.004289   0.273  0.613
    ## Residuals   10 0.15734 0.015734               
    ## 1 observation deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00181 0.001807   0.183  0.678
    ## Residuals   10 0.09883 0.009883

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.07903 -0.05084  0.01147  0.03715  0.07740 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  1.2807295  9.0789720   0.141    0.891
    ## Year        -0.0002804  0.0045113  -0.062    0.952
    ## 
    ## Residual standard error: 0.05395 on 10 degrees of freedom
    ## Multiple R-squared:  0.0003863,  Adjusted R-squared:  -0.09958 
    ## F-statistic: 0.003864 on 1 and 10 DF,  p-value: 0.9517

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.107018 -0.024601 -0.006357  0.055589  0.103994 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -6.194625  10.534687  -0.588    0.568
    ## Year         0.003457   0.005236   0.660    0.523
    ## 
    ## Residual standard error: 0.07064 on 11 degrees of freedom
    ## Multiple R-squared:  0.03812,    Adjusted R-squared:  -0.04933 
    ## F-statistic: 0.4359 on 1 and 11 DF,  p-value: 0.5227

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.135301 -0.004639  0.028964  0.040813  0.066809 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  7.809226  15.388198   0.507    0.626
    ## Year        -0.003534   0.007643  -0.462    0.656
    ## 
    ## Residual standard error: 0.07347 on 8 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.02604,    Adjusted R-squared:  -0.09571 
    ## F-statistic: 0.2139 on 1 and 8 DF,  p-value: 0.6561

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.108344 -0.058516 -0.009735  0.045437  0.130208 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  3.223764   4.703208   0.685    0.504
    ## Year        -0.001238   0.002342  -0.529    0.605
    ## 
    ## Residual standard error: 0.07195 on 15 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01829,    Adjusted R-squared:  -0.04715 
    ## F-statistic: 0.2795 on 1 and 15 DF,  p-value: 0.6048

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09064 -0.06955 -0.01175  0.06154  0.12387 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 10.134766  13.222578   0.766    0.461
    ## Year        -0.004672   0.006570  -0.711    0.493
    ## 
    ## Residual standard error: 0.07857 on 10 degrees of freedom
    ## Multiple R-squared:  0.04812,    Adjusted R-squared:  -0.04706 
    ## F-statistic: 0.5056 on 1 and 10 DF,  p-value: 0.4933

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.166271 -0.021673 -0.003987  0.051220  0.086330 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -19.329419  10.473415  -1.846   0.0920 .
    ## Year          0.009989   0.005205   1.919   0.0813 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07023 on 11 degrees of freedom
    ## Multiple R-squared:  0.2508, Adjusted R-squared:  0.1827 
    ## F-statistic: 3.682 on 1 and 11 DF,  p-value: 0.08131

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.178882 -0.071886  0.002399  0.067569  0.258573 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -9.845207  20.280713  -0.485    0.638
    ## Year         0.005262   0.010078   0.522    0.613
    ## 
    ## Residual standard error: 0.1254 on 10 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.02654,    Adjusted R-squared:  -0.07081 
    ## F-statistic: 0.2726 on 1 and 10 DF,  p-value: 0.613

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21782 -0.04960  0.02714  0.06750  0.11942 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  7.958026  16.730586   0.476    0.645
    ## Year        -0.003555   0.008313  -0.428    0.678
    ## 
    ## Residual standard error: 0.09941 on 10 degrees of freedom
    ## Multiple R-squared:  0.01796,    Adjusted R-squared:  -0.08024 
    ## F-statistic: 0.1829 on 1 and 10 DF,  p-value: 0.678

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-2.png)<!-- -->

    ##             Df   Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.001603 0.001603   0.552  0.475
    ## Residuals   10 0.029034 0.002903

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00949 0.009490   1.705  0.218
    ## Residuals   11 0.06124 0.005568

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00696 0.006958   1.269  0.297
    ## Residuals    7 0.03837 0.005482               
    ## 2 observations deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00007 0.000069   0.006  0.939
    ## Residuals   14 0.15750 0.011250               
    ## 2 observations deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00316 0.003163   0.391  0.546
    ## Residuals   10 0.08096 0.008096

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00005 0.000046   0.014  0.909
    ## Residuals   11 0.03694 0.003358

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00687 0.006866   0.975  0.352
    ## Residuals    8 0.05633 0.007041               
    ## 3 observations deleted due to missingness

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Year         1 0.00020 0.000201   0.048  0.831
    ## Residuals   10 0.04186 0.004186

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.06910 -0.04698  0.00678  0.02843  0.08747 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -6.150152   9.068258  -0.678    0.513
    ## Year         0.003348   0.004506   0.743    0.475
    ## 
    ## Residual standard error: 0.05388 on 10 degrees of freedom
    ## Multiple R-squared:  0.05233,    Adjusted R-squared:  -0.04244 
    ## F-statistic: 0.5522 on 1 and 10 DF,  p-value: 0.4745

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.096392 -0.081246  0.000611  0.062989  0.094088 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -13.893749  11.128338  -1.249    0.238
    ## Year          0.007221   0.005531   1.306    0.218
    ## 
    ## Residual standard error: 0.07462 on 11 degrees of freedom
    ## Multiple R-squared:  0.1342, Adjusted R-squared:  0.05546 
    ## F-statistic: 1.705 on 1 and 11 DF,  p-value: 0.2183

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13733 -0.03141 -0.01841  0.05723  0.07787 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 22.323231  19.250418   1.160    0.284
    ## Year        -0.010769   0.009558  -1.127    0.297
    ## 
    ## Residual standard error: 0.07404 on 7 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1535, Adjusted R-squared:  0.03258 
    ## F-statistic: 1.269 on 1 and 7 DF,  p-value: 0.297

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.180640 -0.060707 -0.008539  0.064000  0.175991 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  1.245713   7.591695   0.164    0.872
    ## Year        -0.000295   0.003779  -0.078    0.939
    ## 
    ## Residual standard error: 0.1061 on 14 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.000435,   Adjusted R-squared:  -0.07096 
    ## F-statistic: 0.006093 on 1 and 14 DF,  p-value: 0.9389

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.179652 -0.035858 -0.001753  0.042527  0.138062 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -8.815924  15.143065  -0.582    0.573
    ## Year         0.004703   0.007524   0.625    0.546
    ## 
    ## Residual standard error: 0.08998 on 10 degrees of freedom
    ## Multiple R-squared:  0.03759,    Adjusted R-squared:  -0.05865 
    ## F-statistic: 0.3906 on 1 and 10 DF,  p-value: 0.546

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.075076 -0.048348 -0.004698  0.035495  0.097962 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.4102306  8.6423449  -0.047    0.963
    ## Year         0.0005042  0.0042954   0.117    0.909
    ## 
    ## Residual standard error: 0.05795 on 11 degrees of freedom
    ## Multiple R-squared:  0.001251,   Adjusted R-squared:  -0.08954 
    ## F-statistic: 0.01378 on 1 and 11 DF,  p-value: 0.9087

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13287 -0.05180  0.01323  0.04492  0.11837 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 16.723495  16.439079   1.017    0.339
    ## Year        -0.008063   0.008166  -0.987    0.352
    ## 
    ## Residual standard error: 0.08391 on 8 degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## Multiple R-squared:  0.1086, Adjusted R-squared:  -0.002776 
    ## F-statistic: 0.9751 on 1 and 8 DF,  p-value: 0.3523

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.14070 -0.02271  0.02203  0.04125  0.06153 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  3.030875  10.888837   0.278    0.786
    ## Year        -0.001185   0.005411  -0.219    0.831
    ## 
    ## Residual standard error: 0.0647 on 10 degrees of freedom
    ## Multiple R-squared:  0.004773,   Adjusted R-squared:  -0.09475 
    ## F-statistic: 0.04796 on 1 and 10 DF,  p-value: 0.8311

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-3.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-4.png)<!-- -->

### Brooding and Spent Females Relative Densities

### Summary Table and Key Plots of Cross-Lake and Time Series Life History Plots

    ## # A tibble: 4 x 7
    ##   Lake     Prop_1_Mean Prop_1_2_SE Mort_Mean Mort_2_SE Growth_Mean Growth_2_SE
    ##   <ord>          <dbl>       <dbl>     <dbl>     <dbl>       <dbl>       <dbl>
    ## 1 Superior       0.716      0.0294      54.9     10.6         0.59      0.0306
    ## 2 Michigan       0.761      0.0383      70.6      7.31        0.64      0.0427
    ## 3 Huron          0.768      0.0433      67.9     11.3         0.6       0.0311
    ## 4 Ontario        0.803      0.0554      71.4     11.6         0.65      0.0358

    ## Call:
    ##    aov(formula = Prop_1 ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Terms:
    ##                       Lake  Residuals
    ## Sum of Squares  0.04578512 0.25921712
    ## Deg. of Freedom          3         46
    ## 
    ## Residual standard error: 0.07506767
    ## Estimated effects may be unbalanced

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Lake         3 0.04579 0.015262   2.708  0.056 .
    ## Residuals   46 0.25922 0.005635                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Prop_1 ~ Lake, data = SizeStructAnnualSummary %>% filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## $Lake
    ##                          diff          lwr        upr     p adj
    ## Michigan-Superior 0.044401511 -0.035699632 0.12450265 0.4590602
    ## Huron-Superior    0.051488582 -0.028612560 0.13158972 0.3285925
    ## Ontario-Superior  0.086795329  0.005107872 0.16848279 0.0333822
    ## Huron-Michigan    0.007087071 -0.071395699 0.08556984 0.9950263
    ## Ontario-Michigan  0.042393818 -0.037707324 0.12249496 0.4992608
    ## Ontario-Huron     0.035306747 -0.044794395 0.11540789 0.6455243

    ## Call:
    ##    aov(formula = Mort_rate ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Terms:
    ##                      Lake Residuals
    ## Sum of Squares   2153.878 15230.417
    ## Deg. of Freedom         3        46
    ## 
    ## Residual standard error: 18.19604
    ## Estimated effects may be unbalanced

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Lake         3   2154   718.0   2.168  0.105
    ## Residuals   46  15230   331.1

    ## 
    ## Call:
    ## aov(formula = Mort_rate ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.593 -12.056   3.328  14.697  26.442 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    54.874      5.253  10.447 9.95e-14 ***
    ## LakeMichigan   15.780      7.284   2.166   0.0355 *  
    ## LakeHuron      12.984      7.284   1.782   0.0813 .  
    ## LakeOntario    16.503      7.429   2.222   0.0313 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.2 on 46 degrees of freedom
    ## Multiple R-squared:  0.1239, Adjusted R-squared:  0.06676 
    ## F-statistic: 2.168 on 3 and 46 DF,  p-value: 0.1046

    ## Call:
    ##    aov(formula = Growth_Calc ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Terms:
    ##                       Lake  Residuals
    ## Sum of Squares  0.02627235 0.18041980
    ## Deg. of Freedom          3         46
    ## 
    ## Residual standard error: 0.06262723
    ## Estimated effects may be unbalanced

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)  
    ## Lake         3 0.02627 0.008757   2.233  0.097 .
    ## Residuals   46 0.18042 0.003922                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Call:
    ## aov(formula = Growth_Calc ~ Lake, data = SizeStructAnnualSummary %>% 
    ##     filter(Group == "GLNPO_Mys", Season == "Summer"))
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.147220 -0.047035  0.007684  0.043675  0.115751 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.58852    0.01808  32.553   <2e-16 ***
    ## LakeMichigan  0.04669    0.02507   1.862   0.0689 .  
    ## LakeHuron     0.01566    0.02507   0.625   0.5352    
    ## LakeOntario   0.05766    0.02557   2.255   0.0289 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06263 on 46 degrees of freedom
    ## Multiple R-squared:  0.1271, Adjusted R-squared:  0.07018 
    ## F-statistic: 2.233 on 3 and 46 DF,  p-value: 0.09704

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-1.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.07903 -0.05084  0.01147  0.03715  0.07740 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  1.2807295  9.0789720   0.141    0.891
    ## Year        -0.0002804  0.0045113  -0.062    0.952
    ## 
    ## Residual standard error: 0.05395 on 10 degrees of freedom
    ## Multiple R-squared:  0.0003863,  Adjusted R-squared:  -0.09958 
    ## F-statistic: 0.003864 on 1 and 10 DF,  p-value: 0.9517

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.107018 -0.024601 -0.006357  0.055589  0.103994 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -6.194625  10.534687  -0.588    0.568
    ## Year         0.003457   0.005236   0.660    0.523
    ## 
    ## Residual standard error: 0.07064 on 11 degrees of freedom
    ## Multiple R-squared:  0.03812,    Adjusted R-squared:  -0.04933 
    ## F-statistic: 0.4359 on 1 and 11 DF,  p-value: 0.5227

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.166271 -0.021673 -0.003987  0.051220  0.086330 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -19.329419  10.473415  -1.846   0.0920 .
    ## Year          0.009989   0.005205   1.919   0.0813 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07023 on 11 degrees of freedom
    ## Multiple R-squared:  0.2508, Adjusted R-squared:  0.1827 
    ## F-statistic: 3.682 on 1 and 11 DF,  p-value: 0.08131

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21782 -0.04960  0.02714  0.06750  0.11942 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  7.958026  16.730586   0.476    0.645
    ## Year        -0.003555   0.008313  -0.428    0.678
    ## 
    ## Residual standard error: 0.09941 on 10 degrees of freedom
    ## Multiple R-squared:  0.01796,    Adjusted R-squared:  -0.08024 
    ## F-statistic: 0.1829 on 1 and 10 DF,  p-value: 0.678

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-3.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.06910 -0.04698  0.00678  0.02843  0.08747 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -6.150152   9.068258  -0.678    0.513
    ## Year         0.003348   0.004506   0.743    0.475
    ## 
    ## Residual standard error: 0.05388 on 10 degrees of freedom
    ## Multiple R-squared:  0.05233,    Adjusted R-squared:  -0.04244 
    ## F-statistic: 0.5522 on 1 and 10 DF,  p-value: 0.4745

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.096392 -0.081246  0.000611  0.062989  0.094088 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -13.893749  11.128338  -1.249    0.238
    ## Year          0.007221   0.005531   1.306    0.218
    ## 
    ## Residual standard error: 0.07462 on 11 degrees of freedom
    ## Multiple R-squared:  0.1342, Adjusted R-squared:  0.05546 
    ## F-statistic: 1.705 on 1 and 11 DF,  p-value: 0.2183

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.075076 -0.048348 -0.004698  0.035495  0.097962 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.4102306  8.6423449  -0.047    0.963
    ## Year         0.0005042  0.0042954   0.117    0.909
    ## 
    ## Residual standard error: 0.05795 on 11 degrees of freedom
    ## Multiple R-squared:  0.001251,   Adjusted R-squared:  -0.08954 
    ## F-statistic: 0.01378 on 1 and 11 DF,  p-value: 0.9087

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.14070 -0.02271  0.02203  0.04125  0.06153 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  3.030875  10.888837   0.278    0.786
    ## Year        -0.001185   0.005411  -0.219    0.831
    ## 
    ## Residual standard error: 0.0647 on 10 degrees of freedom
    ## Multiple R-squared:  0.004773,   Adjusted R-squared:  -0.09475 
    ## F-statistic: 0.04796 on 1 and 10 DF,  p-value: 0.8311

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.108344 -0.058516 -0.009735  0.045437  0.130208 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  3.223764   4.703208   0.685    0.504
    ## Year        -0.001238   0.002342  -0.529    0.605
    ## 
    ## Residual standard error: 0.07195 on 15 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01829,    Adjusted R-squared:  -0.04715 
    ## F-statistic: 0.2795 on 1 and 15 DF,  p-value: 0.6048

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09064 -0.06955 -0.01175  0.06154  0.12387 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 10.134766  13.222578   0.766    0.461
    ## Year        -0.004672   0.006570  -0.711    0.493
    ## 
    ## Residual standard error: 0.07857 on 10 degrees of freedom
    ## Multiple R-squared:  0.04812,    Adjusted R-squared:  -0.04706 
    ## F-statistic: 0.5056 on 1 and 10 DF,  p-value: 0.4933

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.135301 -0.004639  0.028964  0.040813  0.066809 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  7.809226  15.388198   0.507    0.626
    ## Year        -0.003534   0.007643  -0.462    0.656
    ## 
    ## Residual standard error: 0.07347 on 8 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.02604,    Adjusted R-squared:  -0.09571 
    ## F-statistic: 0.2139 on 1 and 8 DF,  p-value: 0.6561

    ## 
    ## Call:
    ## lm(formula = Prop_1 ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.178882 -0.071886  0.002399  0.067569  0.258573 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -9.845207  20.280713  -0.485    0.638
    ## Year         0.005262   0.010078   0.522    0.613
    ## 
    ## Residual standard error: 0.1254 on 10 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.02654,    Adjusted R-squared:  -0.07081 
    ## F-statistic: 0.2726 on 1 and 10 DF,  p-value: 0.613

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.180640 -0.060707 -0.008539  0.064000  0.175991 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  1.245713   7.591695   0.164    0.872
    ## Year        -0.000295   0.003779  -0.078    0.939
    ## 
    ## Residual standard error: 0.1061 on 14 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.000435,   Adjusted R-squared:  -0.07096 
    ## F-statistic: 0.006093 on 1 and 14 DF,  p-value: 0.9389

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.179652 -0.035858 -0.001753  0.042527  0.138062 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -8.815924  15.143065  -0.582    0.573
    ## Year         0.004703   0.007524   0.625    0.546
    ## 
    ## Residual standard error: 0.08998 on 10 degrees of freedom
    ## Multiple R-squared:  0.03759,    Adjusted R-squared:  -0.05865 
    ## F-statistic: 0.3906 on 1 and 10 DF,  p-value: 0.546

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13733 -0.03141 -0.01841  0.05723  0.07787 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 22.323231  19.250418   1.160    0.284
    ## Year        -0.010769   0.009558  -1.127    0.297
    ## 
    ## Residual standard error: 0.07404 on 7 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1535, Adjusted R-squared:  0.03258 
    ## F-statistic: 1.269 on 1 and 7 DF,  p-value: 0.297

    ## 
    ## Call:
    ## lm(formula = Growth_Calc ~ Year, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13287 -0.05180  0.01323  0.04492  0.11837 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 16.723495  16.439079   1.017    0.339
    ## Year        -0.008063   0.008166  -0.987    0.352
    ## 
    ## Residual standard error: 0.08391 on 8 degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## Multiple R-squared:  0.1086, Adjusted R-squared:  -0.002776 
    ## F-statistic: 0.9751 on 1 and 8 DF,  p-value: 0.3523

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-5.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-6.png)<!-- -->

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908   42.19 <2e-16 ***
    ## Length         1   6704    6704  311.51 <2e-16 ***
    ## Residuals   1089  23436      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 578 observations deleted due to missingness

    ## 
    ## Call:
    ## aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3794  -2.8854   0.1987   2.8569  18.0086 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.06308    2.52288   0.025 0.980058    
    ## LakeOntario   -6.40320    2.09711  -3.053 0.002318 ** 
    ## LakeMichigan  -7.63451    2.09768  -3.640 0.000286 ***
    ## LakeHuron     -8.93372    2.14886  -4.157 3.47e-05 ***
    ## LakeSuperior -11.33516    2.08526  -5.436 6.73e-08 ***
    ## Length         1.63056    0.09238  17.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.639 on 1089 degrees of freedom
    ##   (578 observations deleted due to missingness)
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.3029 
    ## F-statistic: 96.06 on 5 and 1089 DF,  p-value: < 2.2e-16

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## $Lake
    ##                          diff        lwr        upr     p adj
    ## Ontario-Erie       -8.5180812 -14.238769 -2.7973932 0.0004863
    ## Michigan-Erie      -8.8788136 -14.607181 -3.1504459 0.0002397
    ## Huron-Erie        -11.0666667 -16.928824 -5.2045092 0.0000029
    ## Superior-Erie     -12.1436399 -17.839939 -6.4473411 0.0000001
    ## Michigan-Ontario   -0.3607324  -1.489298  0.7678329 0.9066716
    ## Huron-Ontario      -2.5485855  -4.229167 -0.8680042 0.0003547
    ## Superior-Ontario   -3.6255587  -4.578073 -2.6730441 0.0000000
    ## Huron-Michigan     -2.1878531  -3.894393 -0.4813131 0.0043573
    ## Superior-Michigan  -3.2648264  -4.262428 -2.2672246 0.0000000
    ## Superior-Huron     -1.0769733  -2.672562  0.5186154 0.3486140

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-7.png)<!-- -->

    ## # A tibble: 5 x 6
    ##   Lake    Season Brood_Count_Mean Brood_Count_2_SE BF_Length_Mean BF_Length_2_SE
    ##   <fct>   <fct>             <dbl>            <dbl>          <dbl>          <dbl>
    ## 1 Superi~ Spring             12.4            0.330           14.9         0.0873
    ## 2 Michig~ Spring             15.9            0.468           14.6         0.132 
    ## 3 Huron   Spring             13.9            0.579           14.6         0.107 
    ## 4 Erie    Spring             26.2            3.86            15.4         0.296 
    ## 5 Ontario Spring             15.9            0.662           14.2         0.104

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-8.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7135 -3.7135 -0.4467  3.4643 17.5533 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -165.84586  145.85584  -1.137    0.256
    ## Year           0.08893    0.07242   1.228    0.220
    ## 
    ## Residual standard error: 5.043 on 510 degrees of freedom
    ##   (133 observations deleted due to missingness)
    ## Multiple R-squared:  0.002949,   Adjusted R-squared:  0.0009936 
    ## F-statistic: 1.508 on 1 and 510 DF,  p-value: 0.22

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.1587  -4.0686  -0.6136   3.5238  19.5238 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -441.7508   262.2672  -1.684   0.0934 .
    ## Year           0.2275     0.1302   1.747   0.0819 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.616 on 235 degrees of freedom
    ##   (236 observations deleted due to missingness)
    ## Multiple R-squared:  0.01283,    Adjusted R-squared:  0.008626 
    ## F-statistic: 3.053 on 1 and 235 DF,  p-value: 0.08187

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.1809  -2.9954  -0.3182   3.2916  14.4180 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 489.36683  199.54825   2.452   0.0154 *
    ## Year         -0.23627    0.09914  -2.383   0.0184 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.545 on 148 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.03696,    Adjusted R-squared:  0.03045 
    ## F-statistic: 5.679 on 1 and 148 DF,  p-value: 0.01844

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.4580  -3.4580  -0.5941   3.4059  23.4739 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 326.49266   96.41592   3.386 0.000845 ***
    ## Year         -0.15533    0.04801  -3.235 0.001410 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.398 on 211 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.04726,    Adjusted R-squared:  0.04275 
    ## F-statistic: 10.47 on 1 and 211 DF,  p-value: 0.00141

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3831  -4.1562   0.7819   4.6376  15.4932 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -61.78143  387.32921  -0.160    0.874
    ## Year          0.04125    0.19247   0.214    0.831
    ## 
    ## Residual standard error: 7.094 on 129 degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## Multiple R-squared:  0.000356,   Adjusted R-squared:  -0.007393 
    ## F-statistic: 0.04594 on 1 and 129 DF,  p-value: 0.8306

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -17.00  -3.96   1.04   5.04  16.64 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 320.57267  159.76269   2.007   0.0464 *
    ## Year         -0.14869    0.07953  -1.870   0.0632 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.164 on 171 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.02003,    Adjusted R-squared:  0.0143 
    ## F-statistic: 3.496 on 1 and 171 DF,  p-value: 0.06323

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.3258 -3.2184 -0.3818  3.6182 10.7489 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  51.92814  310.89875   0.167    0.868
    ## Year         -0.01868    0.15448  -0.121    0.904
    ## 
    ## Residual standard error: 4.279 on 70 degrees of freedom
    ##   (41 observations deleted due to missingness)
    ## Multiple R-squared:  0.0002088,  Adjusted R-squared:  -0.01407 
    ## F-statistic: 0.01462 on 1 and 70 DF,  p-value: 0.9041

    ## 
    ## Call:
    ## lm(formula = Embryos ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.892  -3.790  -0.316   3.396  13.651 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -905.01357  193.52951  -4.676 4.63e-06 ***
    ## Year           0.45756    0.09605   4.764 3.11e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.354 on 269 degrees of freedom
    ##   (166 observations deleted due to missingness)
    ## Multiple R-squared:  0.07779,    Adjusted R-squared:  0.07437 
    ## F-statistic: 22.69 on 1 and 269 DF,  p-value: 3.114e-06

![](GLNPO_Long_term_2019_files/figure-gfm/Summary%20life%20history%20rate%20comparison%20tests-9.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Superior"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7651 -1.1234  0.0512  1.0679  4.5479 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -81.89074   34.81724  -2.352  0.01897 * 
    ## Year          0.04810    0.01729   2.781  0.00557 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.502 on 642 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01191,    Adjusted R-squared:  0.01037 
    ## F-statistic: 7.736 on 1 and 642 DF,  p-value: 0.005571

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5039 -1.0943 -0.0943  1.0460  6.5356 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -246.83243   41.97445  -5.881 7.77e-09 ***
    ## Year           0.12988    0.02086   6.225 1.07e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.719 on 470 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07617,    Adjusted R-squared:  0.0742 
    ## F-statistic: 38.75 on 1 and 470 DF,  p-value: 1.068e-09

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Huron"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9056 -1.0180 -0.0883  0.8695  3.2865 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -211.91722   77.00985  -2.752  0.00692 **
    ## Year           0.11241    0.03828   2.936  0.00404 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.347 on 111 degrees of freedom
    ## Multiple R-squared:  0.07207,    Adjusted R-squared:  0.06371 
    ## F-statistic: 8.622 on 1 and 111 DF,  p-value: 0.004039

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "GLNPO_Mys", Lake == 
    ##         "Ontario"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4404 -1.0768 -0.0288  0.9056  4.5360 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -87.28339   36.91025  -2.365  0.01848 * 
    ## Year          0.05045    0.01834   2.752  0.00618 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.515 on 434 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01715,    Adjusted R-squared:  0.01488 
    ## F-statistic: 7.571 on 1 and 434 DF,  p-value: 0.006179

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.071 -1.052 -0.341  0.757  4.435 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 241.22225   60.87345   3.963 0.000111 ***
    ## Year         -0.11250    0.03024  -3.720 0.000275 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 161 degrees of freedom
    ## Multiple R-squared:  0.07915,    Adjusted R-squared:  0.07343 
    ## F-statistic: 13.84 on 1 and 161 DF,  p-value: 0.0002749

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Spring", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2545 -1.0088 -0.2802  0.7980  5.1581 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 125.95299   26.45046   4.762  3.4e-06 ***
    ## Year         -0.05525    0.01317  -4.195  3.9e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.54 on 229 degrees of freedom
    ## Multiple R-squared:  0.07137,    Adjusted R-squared:  0.06732 
    ## F-statistic:  17.6 on 1 and 229 DF,  p-value: 3.898e-05

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan", 
    ##         Year >= 2007))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2817 -1.1458  0.0829  1.0176  4.8485 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 173.18846   77.85833   2.224   0.0277 *
    ## Year         -0.07745    0.03869  -2.002   0.0472 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.489 on 141 degrees of freedom
    ## Multiple R-squared:  0.02764,    Adjusted R-squared:  0.02074 
    ## F-statistic: 4.007 on 1 and 141 DF,  p-value: 0.04722

    ## 
    ## Call:
    ## lm(formula = Length ~ Year, data = Mysids_BroodingFemales %>% 
    ##     filter(Season == "Fall", Group == "NOAA", Lake == "Michigan"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3329 -1.0907 -0.0107  1.1138  5.1449 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -11.13875   33.49110  -0.333    0.740
    ## Year          0.01411    0.01667   0.847    0.398
    ## 
    ## Residual standard error: 1.54 on 184 degrees of freedom
    ## Multiple R-squared:  0.003879,   Adjusted R-squared:  -0.001534 
    ## F-statistic: 0.7166 on 1 and 184 DF,  p-value: 0.3984

# 6\. Compare to food resources

    ## Warning: Removed 21 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-2.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-3.png)<!-- -->

    ## # A tibble: 303 x 8
    ##    Lake  Season  Year Dens_Pred Biom_Pred Lake_B Annual_Chl_mg_m3
    ##    <chr> <chr>  <dbl>     <dbl>     <dbl> <fct>             <dbl>
    ##  1 Mich~ Spring  1995      215.      420. <NA>              NA   
    ##  2 Mich~ Spring  1996      235.      386. <NA>              NA   
    ##  3 Mich~ Spring  1997      219.      305. <NA>              NA   
    ##  4 Mich~ Spring  1998      166.      212. Michi~             1.15
    ##  5 Mich~ Spring  1999      123.      171. Michi~             1.14
    ##  6 Mich~ Spring  2000      115.      225. Michi~             1.12
    ##  7 Mich~ Spring  2001      149.      437. Michi~             1.16
    ##  8 Mich~ Spring  2002      213.      817. Michi~             1.03
    ##  9 Mich~ Spring  2003      254.     1079. Michi~             1.03
    ## 10 Mich~ Spring  2004      228.      933. Michi~             1.00
    ## # ... with 293 more rows, and 1 more variable: Mar_Jun_Chl_mg_m3 <dbl>

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-4.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-5.png)<!-- -->

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Dens_Pred ~ s(Mar_Jun_Chl_mg_m3)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  183.294      6.595   27.79   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                        edf Ref.df     F p-value    
    ## s(Mar_Jun_Chl_mg_m3) 3.595  4.432 27.41  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.593   Deviance explained = 61.1%
    ## -REML = 459.86  Scale est. = 3653.4    n = 84

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Biom_Pred ~ s(Mar_Jun_Chl_mg_m3)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   439.56      26.32    16.7   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                        edf Ref.df     F  p-value    
    ## s(Mar_Jun_Chl_mg_m3) 3.077  3.817 11.85 1.48e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.35   Deviance explained = 37.5%
    ## -REML = 572.58  Scale est. = 58178     n = 84

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-6.png)<!-- -->

    ## Warning: Ignoring unknown aesthetics: Group

    ## Warning: Removed 6 rows containing missing values (geom_smooth).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-7.png)<!-- -->

    ## Warning: Ignoring unknown aesthetics: Group

    ## Warning: Removed 5 rows containing missing values (geom_smooth).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-8.png)<!-- -->

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Dens_Pred ~ s(Total)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   143.43       7.89   18.18   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##            edf Ref.df    F  p-value    
    ## s(Total) 3.145  3.942 24.3 2.46e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.478   Deviance explained = 49.4%
    ## -REML =  605.3  Scale est. = 6537      n = 105

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Biom_Pred ~ s(Total)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   345.11      24.61   14.02   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##            edf Ref.df     F  p-value    
    ## s(Total) 2.644  3.327 19.33 7.49e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.379   Deviance explained = 39.5%
    ## -REML = 721.82  Scale est. = 63587     n = 105

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-9.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-10.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-8.181811e-07,2.724136e-07]
    ## (score 459.8621 & scale 3653.374).
    ## Hessian positive definite, eigenvalue range [1.073102,41.04217].
    ## Model rank =  10 / 10 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                       k' edf k-index p-value
    ## s(Mar_Jun_Chl_mg_m3) 9.0 3.6    1.05    0.58

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-11.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-0.0001042666,3.166405e-06]
    ## (score 572.5769 & scale 58178.16).
    ## Hessian positive definite, eigenvalue range [0.8293793,41.02695].
    ## Model rank =  10 / 10 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##                        k'  edf k-index p-value
    ## s(Mar_Jun_Chl_mg_m3) 9.00 3.08    0.96    0.32

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-12.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-1.04154e-05,2.887593e-07]
    ## (score 605.2982 & scale 6537.003).
    ## Hessian positive definite, eigenvalue range [0.8638103,51.52272].
    ## Model rank =  10 / 10 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##            k'  edf k-index p-value
    ## s(Total) 9.00 3.14    1.08    0.78

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-13.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [-6.990281e-05,9.471105e-07]
    ## (score 721.8181 & scale 63587.03).
    ## Hessian positive definite, eigenvalue range [0.630178,51.51335].
    ## Model rank =  10 / 10 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##            k'  edf k-index p-value
    ## s(Total) 9.00 2.64    1.14    0.88

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Dens_Pred ~ s(Mar_Jun_Chl_mg_m3)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  183.294      6.595   27.79   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                        edf Ref.df     F p-value    
    ## s(Mar_Jun_Chl_mg_m3) 3.595  4.432 27.41  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.593   Deviance explained = 61.1%
    ## -REML = 459.86  Scale est. = 3653.4    n = 84

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Biom_Pred ~ s(Mar_Jun_Chl_mg_m3)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   439.56      26.32    16.7   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                        edf Ref.df     F  p-value    
    ## s(Mar_Jun_Chl_mg_m3) 3.077  3.817 11.85 1.48e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.35   Deviance explained = 37.5%
    ## -REML = 572.58  Scale est. = 58178     n = 84

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Dens_Pred ~ s(Total)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   143.43       7.89   18.18   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##            edf Ref.df    F  p-value    
    ## s(Total) 3.145  3.942 24.3 2.46e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.478   Deviance explained = 49.4%
    ## -REML =  605.3  Scale est. = 6537      n = 105

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## Biom_Pred ~ s(Total)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   345.11      24.61   14.02   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##            edf Ref.df     F  p-value    
    ## s(Total) 2.644  3.327 19.33 7.49e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.379   Deviance explained = 39.5%
    ## -REML = 721.82  Scale est. = 63587     n = 105

    ## Warning: Removed 10 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 10 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 10 rows containing missing values (geom_label).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food%20all%20in%20one-1.png)<!-- -->

    ## Warning: Removed 10 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 10 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food%20all%20in%20one-2.png)<!-- -->

    ## # A tibble: 1,354 x 9
    ##     Mean    SE LWR_1_SE UPR_1_SE LWR_2_SE UPR_2_SE Xaxis Food_Type
    ##    <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl> <dbl> <chr>    
    ##  1  31.8  19.3     12.5     51.1    -6.80     70.5 0.5   Mar_Jun_~
    ##  2  35.6  18.6     17.1     54.2    -1.49     72.8 0.51  Mar_Jun_~
    ##  3  39.4  17.8     21.6     57.3     3.77     75.1 0.52  Mar_Jun_~
    ##  4  43.2  17.1     26.1     60.4     8.98     77.5 0.53  Mar_Jun_~
    ##  5  47.0  16.5     30.6     63.5    14.1      79.9 0.54  Mar_Jun_~
    ##  6  50.8  15.8     35.0     66.6    19.2      82.4 0.55  Mar_Jun_~
    ##  7  54.6  15.2     39.4     69.8    24.2      85.0 0.56  Mar_Jun_~
    ##  8  58.4  14.6     43.8     73.0    29.2      87.7 0.57  Mar_Jun_~
    ##  9  62.2  14.1     48.1     76.3    34.0      90.4 0.580 Mar_Jun_~
    ## 10  66.0  13.6     52.4     79.6    38.8      93.1 0.59  Mar_Jun_~
    ## # ... with 1,344 more rows, and 1 more variable: Mysid_Abundance_type <chr>

    ## Warning: Removed 10 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food%20all%20in%20one-3.png)<!-- -->

    ## # A tibble: 794 x 6
    ##    Lake     Group      Year Type     TimeFrame             Abundance
    ##    <fct>    <fct>     <dbl> <chr>    <chr>                     <dbl>
    ##  1 Michigan GLNPO_Mys  2006 MeanBiom Same_Year                  723.
    ##  2 Michigan GLNPO_Mys  2006 MeanBiom Summer_to_Next_Spring      790.
    ##  3 Michigan GLNPO_Mys  2006 MeanBiom Prev_Fall_to_Summer        723.
    ##  4 Michigan GLNPO_Mys  2007 MeanBiom Same_Year                  354.
    ##  5 Michigan GLNPO_Mys  2007 MeanBiom Prev_Year                  723.
    ##  6 Michigan GLNPO_Mys  2007 MeanBiom Summer_to_Next_Spring      359.
    ##  7 Michigan GLNPO_Mys  2007 MeanBiom Prev_Fall_to_Summer        354.
    ##  8 Michigan GLNPO_Mys  2008 MeanBiom Same_Year                  386.
    ##  9 Michigan GLNPO_Mys  2008 MeanBiom Prev_Year                  354.
    ## 10 Michigan GLNPO_Mys  2008 MeanBiom Summer_to_Next_Spring      299.
    ## # ... with 784 more rows

    ## # A tibble: 410 x 6
    ## # Groups:   Lake [5]
    ##    Lake  Lake_B        Year TimeFrame             SpringChla Group    
    ##    <chr> <fct>        <int> <chr>                      <dbl> <chr>    
    ##  1 Erie  Eastern_Erie  1998 Same_Year                   2.97 GLNPO_Mys
    ##  2 Erie  Eastern_Erie  1998 Summer_to_Next_Spring       2.75 GLNPO_Mys
    ##  3 Erie  Eastern_Erie  1998 Prev_Fall_to_Summer         2.97 GLNPO_Mys
    ##  4 Erie  Eastern_Erie  1999 Same_Year                   2.75 GLNPO_Mys
    ##  5 Erie  Eastern_Erie  1999 Prev_Year                   2.97 GLNPO_Mys
    ##  6 Erie  Eastern_Erie  1999 Summer_to_Next_Spring       1.46 GLNPO_Mys
    ##  7 Erie  Eastern_Erie  1999 Prev_Fall_to_Summer         2.75 GLNPO_Mys
    ##  8 Erie  Eastern_Erie  2000 Same_Year                   1.46 GLNPO_Mys
    ##  9 Erie  Eastern_Erie  2000 Prev_Year                   2.75 GLNPO_Mys
    ## 10 Erie  Eastern_Erie  2000 Summer_to_Next_Spring       2.66 GLNPO_Mys
    ## # ... with 400 more rows

    ## # A tibble: 410 x 5
    ##    Lake_B        Year TimeFrame             ZoopBiom Group    
    ##    <chr>        <dbl> <chr>                    <dbl> <chr>    
    ##  1 Eastern_Erie  1998 Same_Year                 650. GLNPO_Mys
    ##  2 Eastern_Erie  1998 Summer_to_Next_Spring     648. GLNPO_Mys
    ##  3 Eastern_Erie  1998 Prev_Fall_to_Summer       650. GLNPO_Mys
    ##  4 Eastern_Erie  1999 Same_Year                1201. GLNPO_Mys
    ##  5 Eastern_Erie  1999 Prev_Year                 650. GLNPO_Mys
    ##  6 Eastern_Erie  1999 Summer_to_Next_Spring    1216. GLNPO_Mys
    ##  7 Eastern_Erie  1999 Prev_Fall_to_Summer      1201. GLNPO_Mys
    ##  8 Eastern_Erie  2001 Same_Year                 925. GLNPO_Mys
    ##  9 Eastern_Erie  2001 Prev_Year                1201. GLNPO_Mys
    ## 10 Eastern_Erie  2001 Summer_to_Next_Spring    1072. GLNPO_Mys
    ## # ... with 400 more rows

    ## # A tibble: 1,536 x 8
    ##    Lake    Group    Year TimeFrame       X_Variable X_Value Rate_Name  Rate_Mean
    ##    <chr>   <chr>   <dbl> <chr>           <chr>        <dbl> <chr>          <dbl>
    ##  1 Michig~ GLNPO_~  2006 Summer_to_Next~ MeanBiom   7.90e+2 Mort_rate     91.3  
    ##  2 Michig~ GLNPO_~  2006 Summer_to_Next~ MeanBiom   7.90e+2 Growth_Ca~     0.592
    ##  3 Michig~ GLNPO_~  2006 Summer_to_Next~ MeanDens   2.43e+2 Mort_rate     91.3  
    ##  4 Michig~ GLNPO_~  2006 Summer_to_Next~ MeanDens   2.43e+2 Growth_Ca~     0.592
    ##  5 Michig~ GLNPO_~  2006 Summer_to_Next~ SpringChla 7.85e-1 Mort_rate     91.3  
    ##  6 Michig~ GLNPO_~  2006 Summer_to_Next~ SpringChla 7.85e-1 Growth_Ca~     0.592
    ##  7 Michig~ GLNPO_~  2006 Summer_to_Next~ ZoopBiom   2.56e+3 Mort_rate     91.3  
    ##  8 Michig~ GLNPO_~  2006 Summer_to_Next~ ZoopBiom   2.56e+3 Growth_Ca~     0.592
    ##  9 Michig~ GLNPO_~  2006 Prev_Fall_to_S~ MeanBiom   7.23e+2 Prop_A0        0.656
    ## 10 Michig~ GLNPO_~  2006 Prev_Fall_to_S~ MeanDens   2.21e+2 Prop_A0        0.656
    ## # ... with 1,526 more rows

![](GLNPO_Long_term_2019_files/figure-gfm/Mysid%20food%20with%20life%20history%20rates-1.png)<!-- -->

    ## # A tibble: 10 x 7
    ##    Rate             Predictor  Group        SlopeEst  pValue       r2 Signif
    ##    <chr>            <chr>      <chr>           <dbl>   <dbl>    <dbl> <fct> 
    ##  1 Growth_Calc      ZoopBiom   GLNPO_Mys  0.0000461  0.00745 0.152    "*"   
    ##  2 Growth_Calc      SpringChla GLNPO_Mys  0.0310     0.0467  0.0870   "*"   
    ##  3 Mort_rate        ZoopBiom   GLNPO_Mys  0.0103     0.0502  0.0844   "."   
    ##  4 Brood_Spring     SpringChla GLNPO_Mys  1.43       0.0750  0.0831   "."   
    ##  5 Brood_Spring     ZoopBiom   GLNPO_Mys  0.00154    0.113   0.0666   ""    
    ##  6 Prop_A0          SpringChla GLNPO_Mys  0.0199     0.290   0.0233   ""    
    ##  7 BF_Length_Spring SpringChla GLNPO_Mys -0.173      0.356   0.0198   ""    
    ##  8 Mort_rate        SpringChla GLNPO_Mys  3.35       0.482   0.0113   ""    
    ##  9 BF_Length_Spring ZoopBiom   GLNPO_Mys  0.0000851  0.686   0.00383  ""    
    ## 10 Prop_A0          ZoopBiom   GLNPO_Mys  0.00000197 0.925   0.000186 ""

    ## # A tibble: 30 x 7
    ##    Rate             Predictor Group        SlopeEst pValue        r2 Signif
    ##    <chr>            <chr>     <chr>           <dbl>  <dbl>     <dbl> <fct> 
    ##  1 Prop_A0          MeanDens  GLNPO_Mys  0.000201   0.0522 0.0762    "."   
    ##  2 BF_Length_Spring MeanDens  NOAA       0.00800    0.117  0.208     ""    
    ##  3 Brood_Spring     MeanDens  NOAA      -0.0334     0.134  0.193     ""    
    ##  4 Brood_Fall       MeanDens  NOAA      -0.0555     0.150  0.179     ""    
    ##  5 BF_Length_Spring MeanBiom  NOAA       0.00247    0.159  0.172     ""    
    ##  6 Brood_Spring     MeanBiom  NOAA      -0.0105     0.166  0.166     ""    
    ##  7 Brood_Fall       MeanBiom  NOAA      -0.0143     0.276  0.107     ""    
    ##  8 Mort_rate        MeanBiom  USGS      -0.0250     0.226  0.0848    ""    
    ##  9 Growth_Calc      MeanDens  USGS       0.000373   0.317  0.0589    ""    
    ## 10 Mort_rate        MeanDens  GLNPO_Mys  0.0394     0.129  0.0473    ""    
    ## 11 Mort_rate        MeanBiom  GLNPO_Mys  0.0175     0.171  0.0387    ""    
    ## 12 Growth_Calc      MeanBiom  USGS       0.0000789  0.487  0.0289    ""    
    ## 13 Prop_A0          MeanBiom  NOAA      -0.000174   0.604  0.0279    ""    
    ## 14 BF_Length_Spring MeanBiom  GLNPO_Mys -0.000584   0.253  0.0277    ""    
    ## 15 Brood_Spring     MeanDens  GLNPO_Mys  0.00547    0.325  0.0237    ""    
    ## 16 Brood_Spring     MeanBiom  GLNPO_Mys  0.00274    0.330  0.0232    ""    
    ## 17 Prop_A0          MeanDens  NOAA      -0.000416   0.651  0.0213    ""    
    ## 18 BF_Length_Spring MeanDens  GLNPO_Mys -0.000998   0.324  0.0207    ""    
    ## 19 Mort_rate        MeanDens  USGS      -0.0348     0.586  0.0178    ""    
    ## 20 Prop_A0          MeanDens  USGS      -0.000158   0.603  0.0137    ""    
    ## 21 Prop_A0          MeanBiom  GLNPO_Mys  0.0000407  0.442  0.0123    ""    
    ## 22 Prop_A0          MeanBiom  USGS      -0.0000473  0.641  0.0111    ""    
    ## 23 BF_Length_Fall   MeanBiom  NOAA       0.000544   0.791  0.00663   ""    
    ## 24 Mort_rate        MeanDens  NOAA      -0.0469     0.869  0.00319   ""    
    ## 25 Growth_Calc      MeanDens  NOAA       0.000148   0.871  0.00279   ""    
    ## 26 Mort_rate        MeanBiom  NOAA      -0.0149     0.879  0.00270   ""    
    ## 27 BF_Length_Fall   MeanDens  NOAA      -0.000630   0.919  0.000982  ""    
    ## 28 Growth_Calc      MeanDens  GLNPO_Mys  0.0000135  0.881  0.000469  ""    
    ## 29 Growth_Calc      MeanBiom  GLNPO_Mys  0.00000305 0.945  0.0000985 ""    
    ## 30 Growth_Calc      MeanBiom  NOAA      -0.00000563 0.985  0.0000351 ""

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   4.000   3.066   4.000   4.000

    ## # A tibble: 0 x 8
    ## # ... with 8 variables: Lake <chr>, Group <chr>, Year <dbl>, TimeFrame <chr>,
    ## #   X_Variable <chr>, X_Value <dbl>, Rate_Name <chr>, Rate_Mean <dbl>

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1       1       1       1       1       1

![](GLNPO_Long_term_2019_files/figure-gfm/Mysid%20food%20with%20life%20history%20rates-2.png)<!-- -->

    ## # A tibble: 33 x 7
    ##    Lake     Rate             Group      pValue Signif       r2  SlopeEst
    ##    <chr>    <chr>            <chr>       <dbl> <fct>     <dbl>     <dbl>
    ##  1 Huron    BF_Length_Spring GLNPO_Mys 0.144   ""     0.201     0.0851  
    ##  2 Huron    Brood_Spring     GLNPO_Mys 0.791   ""     0.00930  -0.106   
    ##  3 Huron    Growth_Calc      GLNPO_Mys 0.909   ""     0.00125   0.000504
    ##  4 Huron    Growth_Calc      USGS      0.352   ""     0.109    -0.00806 
    ##  5 Huron    Mort_rate_MvAv3  GLNPO_Mys 0.0852  "."    0.267     1.37    
    ##  6 Huron    Mort_rate_MvAv3  USGS      0.576   ""     0.0324    1.17    
    ##  7 Huron    Prop_A0          GLNPO_Mys 0.0813  "."    0.251     0.00999 
    ##  8 Huron    Prop_A0          USGS      0.613   ""     0.0265    0.00526 
    ##  9 Michigan BF_Length_Fall   NOAA      0.571   ""     0.0301   -0.0310  
    ## 10 Michigan BF_Length_Spring GLNPO_Mys 0.0974  "."    0.230     0.106   
    ## 11 Michigan BF_Length_Spring NOAA      0.00529 "*"    0.522    -0.125   
    ## 12 Michigan Brood_Fall       NOAA      0.195   ""     0.147     0.449   
    ## 13 Michigan Brood_Spring     GLNPO_Mys 0.215   ""     0.185     0.332   
    ## 14 Michigan Brood_Spring     NOAA      0.841   ""     0.00384   0.0465  
    ## 15 Michigan Growth_Calc      GLNPO_Mys 0.218   ""     0.134     0.00722 
    ## 16 Michigan Growth_Calc      NOAA      0.546   ""     0.0376    0.00470 
    ## 17 Michigan Growth_Calc      USGS      0.297   ""     0.154    -0.0108  
    ## 18 Michigan Mort_rate_MvAv3  GLNPO_Mys 0.0396  "*"    0.359     1.15    
    ## 19 Michigan Mort_rate_MvAv3  NOAA      0.674   ""     0.0185    0.814   
    ## 20 Michigan Mort_rate_MvAv3  USGS      0.198   ""     0.198     3.56    
    ## 21 Michigan Prop_A0          GLNPO_Mys 0.523   ""     0.0381    0.00346 
    ## 22 Michigan Prop_A0          NOAA      0.493   ""     0.0481   -0.00467 
    ## 23 Michigan Prop_A0          USGS      0.656   ""     0.0260   -0.00353 
    ## 24 Ontario  BF_Length_Spring GLNPO_Mys 0.460   ""     0.0557    0.0462  
    ## 25 Ontario  Brood_Spring     GLNPO_Mys 0.329   ""     0.106     0.345   
    ## 26 Ontario  Growth_Calc      GLNPO_Mys 0.831   ""     0.00477  -0.00118 
    ## 27 Ontario  Mort_rate_MvAv3  GLNPO_Mys 0.149   ""     0.217    -1.08    
    ## 28 Ontario  Prop_A0          GLNPO_Mys 0.678   ""     0.0180   -0.00356 
    ## 29 Superior BF_Length_Spring GLNPO_Mys 0.196   ""     0.161     0.0659  
    ## 30 Superior Brood_Spring     GLNPO_Mys 0.166   ""     0.182     0.194   
    ## 31 Superior Growth_Calc      GLNPO_Mys 0.475   ""     0.0523    0.00335 
    ## 32 Superior Mort_rate_MvAv3  GLNPO_Mys 0.102   ""     0.269     1.30    
    ## 33 Superior Prop_A0          GLNPO_Mys 0.952   ""     0.000386 -0.000280

    ## # A tibble: 14 x 6
    ##    Lake     Rate             Group      pValue    r2 SlopeEst
    ##    <chr>    <chr>            <chr>       <dbl> <dbl>    <dbl>
    ##  1 Huron    BF_Length_Spring GLNPO_Mys 0.144   0.201  0.0851 
    ##  2 Huron    Mort_rate_MvAv3  GLNPO_Mys 0.0852  0.267  1.37   
    ##  3 Huron    Prop_A0          GLNPO_Mys 0.0813  0.251  0.00999
    ##  4 Michigan BF_Length_Spring GLNPO_Mys 0.0974  0.230  0.106  
    ##  5 Michigan BF_Length_Spring NOAA      0.00529 0.522 -0.125  
    ##  6 Michigan Brood_Fall       NOAA      0.195   0.147  0.449  
    ##  7 Michigan Brood_Spring     GLNPO_Mys 0.215   0.185  0.332  
    ##  8 Michigan Growth_Calc      USGS      0.297   0.154 -0.0108 
    ##  9 Michigan Mort_rate_MvAv3  GLNPO_Mys 0.0396  0.359  1.15   
    ## 10 Michigan Mort_rate_MvAv3  USGS      0.198   0.198  3.56   
    ## 11 Ontario  Mort_rate_MvAv3  GLNPO_Mys 0.149   0.217 -1.08   
    ## 12 Superior BF_Length_Spring GLNPO_Mys 0.196   0.161  0.0659 
    ## 13 Superior Brood_Spring     GLNPO_Mys 0.166   0.182  0.194  
    ## 14 Superior Mort_rate_MvAv3  GLNPO_Mys 0.102   0.269  1.30

    ## # A tibble: 5 x 6
    ##   Predictor Rate             Group pValue    r2 SlopeEst
    ##   <chr>     <chr>            <chr>  <dbl> <dbl>    <dbl>
    ## 1 MeanBiom  BF_Length_Spring NOAA   0.159 0.172  0.00247
    ## 2 MeanBiom  Brood_Spring     NOAA   0.166 0.166 -0.0105 
    ## 3 MeanDens  BF_Length_Spring NOAA   0.117 0.208  0.00800
    ## 4 MeanDens  Brood_Fall       NOAA   0.150 0.179 -0.0555 
    ## 5 MeanDens  Brood_Spring     NOAA   0.134 0.193 -0.0334

    ## # A tibble: 1 x 6
    ##   Predictor Rate        Group      pValue    r2  SlopeEst
    ##   <chr>     <chr>       <chr>       <dbl> <dbl>     <dbl>
    ## 1 ZoopBiom  Growth_Calc GLNPO_Mys 0.00745 0.152 0.0000461

# End of Script.

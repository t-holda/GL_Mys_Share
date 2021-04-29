Summary of Analyses of
Great Lakes Mysis diluviana
Abundance and Life History Trends
During 1990-2019
================

<br>

# 1\. Upload GLNPO Data and Evaluate Usability of Zooplankton Net Mysid Catches

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


![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-1.png)<!-- -->


*(Adjusted r^2 values for the linear fits plotted above):*

  - *Density: 0.61*
  - *Biomass: 0.33*

![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-2.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/linear%20plots-3.png)<!-- -->


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

<br> Conclusions:

  - Correlations look good. Can use the data.
  - Lines do not seem to match 1:1 lines nor include them in confidence
    intervals. Zooplankton net values cannot be considered the same as
    the values one would obtain with mysid net.

Transform density data?

1.  Linear (untransformed) data

2.  Log\_e transformed data

3.  Square-root transformed data

4.  Fourth-root transformed data

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

### Load NOAA GLERL Muskegon Mysid Data

### Load USGS GLSC Mysid Net Data From Annual Acoustic Surveys in Lakes Michigan and Huron

### Upload DFO data into tibble

## Compile data sources into single tibble

### Examine `Mysids` tibble

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

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-2.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/plot%20timeline%20of%20sampling%20by%20different%20groups%20in%20each%20lake-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-9.png)<!-- -->

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

### Models of Lake Differences

Summer Mean Values:

![](GLNPO_Long_term_2019_files/figure-gfm/TukeyHSD%20Results-5.png)<!-- -->

<br>

# 4\. Examine Time Series Trends in Each Lake

### Plot trends with smoother gams for each season, group and lake.

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Trends%20over%20time-9.png)<!-- -->

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

<br>

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

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-2.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-4.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/plot%20cross-lake%20seasonal%20panels-7.png)<!-- -->

#### …And versions with data plotted for supplementary figure(s):

The plot below shows the same above GAM fits superimposed on
model-adjusted data points. The model adjustment in the data points is
that of re-predicting the values assuming the GLNPO Mysid net was used
to collect the sample. Thus, each point is the predicted GAM value for
the lake, season, and year shown and gvien the `GLNPO_Mys` `Group`
effect, plus the residual associated with that point’s fit to the GAM.

![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20-%20GAM%20adjusted%20for%20GLNPO_Mys-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/time%20trends%20with%20data%20by%20lake%20and%20season%20-%20GAM%20adjusted%20for%20GLNPO_Mys-2.png)<!-- -->

<br>

# 5\. Examine averages and trends in life history rates in the lakes

## Calculate summary values from life history data

#### Key plots and analyses of Brooding Females data

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-1.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-2.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-1.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-2.png)<!-- -->


NOAA Values:

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20cross-lakes-4.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females%20-%20trends-4.png)<!-- -->

<br>

### Cohort Lengths Data Plotting and Analysis

#### Size Structure plots

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-2.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-3.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-4.png)<!-- -->

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

    ## # A tibble: 3 x 7
    ##   Season Cohort Mean_Length N_Length Month  Growth Mortality
    ##   <fct>  <fct>        <dbl>    <int> <dbl>   <dbl>     <dbl>
    ## 1 Summer Age-0         3.89      125     0 NaN         0    
    ## 2 Spring Age-1+       14.5         8     8   1.33      0.984
    ## 3 Summer Age-1+       15.5         1    12   0.967     0.992

    ## [1] "Proportion" "93.3"

    ## [1] "Proportion,  Spring" "94"

<br>

-----

### Growth and Survival estimates

#### Age Structure, Mortality, and Growth: Cross-Lakes Comparisons

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-2.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20GLNPO%20Cross-Lake%20Analyses%20and%20Tables-3.png)<!-- -->

#### Age Structure, Mortality, and Growth: Time Series

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-1.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-2.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Age%20Structure,%20Mortality,%20and%20Growth%20Time%20Series%20Plots%20and%20Analysis-4.png)<!-- -->


# 6\. Compare to food resources

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-2.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food-6.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food%20all%20in%20one-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/mysid%20food%20all%20in%20one-3.png)<!-- -->

![](GLNPO_Long_term_2019_files/figure-gfm/Mysid%20food%20with%20life%20history%20rates-1.png)<!-- -->
![](GLNPO_Long_term_2019_files/figure-gfm/Mysid%20food%20with%20life%20history%20rates-2.png)<!-- -->

# End of Script.

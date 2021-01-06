Summary of Analyses of
Great Lakes Mysis diluviana
Abundance and Life History Trends
During 1990-2019
================

# Contents:

[Load Libraries](#load-package-libraries) 

[GLNPO Zooplankton Net Usage Results](#4th-root-transformation-fit-stats-and-plots)  
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
[…5… Size and Growth](#size-structure-plots) […5… Age Structure and
Survival](#survival-estimates)

<br>

## Plot and examine relationships between nets

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
  - Seems non-linear, transform the data and get updated statistics.
  
We tested some transformations and determined that 4th-root was the best. Natural log was second-best.  Here we use fourth-root.

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

n = 478

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


# 3\. Compare Values Among Lakes


| Lake     | Period\_Numeric | Group       | Season | Period\_Name | 1997-2005 | 1995-2002 | 2006-2015 | 2007-2015 | 2005-2015 | 2016-2019 |
| :------- | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: | --------: | --------: | --------: |
| Michigan |               2 | GLNPO\_Zoop | Spring | Early2000s   |     181.6 |        \- |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Spring | Early2000s   |        \- |     155.6 |        \- |        \- |        \- |        \- |
| Michigan |               2 | GLNPO\_Zoop | Summer | Early2000s   |     389.2 |        \- |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Summer | Early2000s   |        \- |     247.5 |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Fall   | Early2000s   |        \- |     132.8 |        \- |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- |        \- |     120.6 |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |        \- |      84.3 |        \- |        \- |        \- |
| Michigan |               3 | NOAA        | Spring | Early2010s   |        \- |        \- |        \- |      48.0 |        \- |        \- |
| Michigan |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- |        \- |     170.1 |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- |        \- |     171.2 |        \- |        \- |        \- |
| Michigan |               3 | NOAA        | Summer | Early2010s   |        \- |        \- |        \- |      98.2 |        \- |        \- |
| Michigan |               3 | USGS        | Summer | Early2010s   |        \- |        \- |        \- |        \- |     205.1 |        \- |
| Michigan |               3 | NOAA        | Fall   | Early2010s   |        \- |        \- |        \- |      79.4 |        \- |        \- |
| Michigan |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- |      51.8 |
| Michigan |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- |      53.5 |
| Michigan |               4 | NOAA        | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- |      33.6 |
| Michigan |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- |     119.4 |
| Michigan |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- |      93.9 |
| Michigan |               4 | NOAA        | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- |      43.0 |
| Michigan |               4 | USGS        | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- |      91.9 |
| Michigan |               4 | NOAA        | Fall   | Late2010s    |        \- |        \- |        \- |        \- |        \- |      44.3 |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-1.png)<!-- -->

| Lake    | Period\_Numeric | Group       | Season | Period\_Name | 1990-1995 | 1997-2005 | 2002-2005 | 2006-2015 | 2016-2019 | 2016-2017 |
| :------ | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: | --------: | --------: | --------: |
| Ontario |               1 | DFO         | Fall   | Early1990s   |     438.9 |        \- |        \- |        \- |        \- |        \- |
| Ontario |               2 | GLNPO\_Zoop | Spring | Early2000s   |        \- |      85.0 |        \- |        \- |        \- |        \- |
| Ontario |               2 | GLNPO\_Zoop | Summer | Early2000s   |        \- |     171.9 |        \- |        \- |        \- |        \- |
| Ontario |               2 | DFO         | Fall   | Early2000s   |        \- |        \- |     254.3 |        \- |        \- |        \- |
| Ontario |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- |        \- |        \- |     130.7 |        \- |        \- |
| Ontario |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |        \- |        \- |     169.3 |        \- |        \- |
| Ontario |               3 | DFO         | Summer | Early2010s   |        \- |        \- |        \- |     332.6 |        \- |        \- |
| Ontario |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- |        \- |        \- |     361.0 |        \- |        \- |
| Ontario |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- |        \- |        \- |     254.3 |        \- |        \- |
| Ontario |               3 | DFO         | Fall   | Early2010s   |        \- |        \- |        \- |     198.1 |        \- |        \- |
| Ontario |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- |        \- |        \- |     200.7 |        \- |
| Ontario |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |        \- |        \- |     125.0 |        \- |
| Ontario |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- |        \- |        \- |     240.7 |        \- |
| Ontario |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- |        \- |        \- |     175.2 |        \- |
| Ontario |               4 | DFO         | Fall   | Late2010s    |        \- |        \- |        \- |        \- |        \- |     151.4 |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-2.png)<!-- -->

| Lake  | Period\_Numeric | Group       | Season      | Period\_Name | 1997-2005 | 2006-2015 | 2005-2015 | 2016-2019 |
| :---- | --------------: | :---------- | :---------- | :----------- | --------: | --------: | --------: | --------: |
| Huron |               2 | GLNPO\_Zoop | Spring      | Early2000s   |      53.3 |        \- |        \- |        \- |
| Huron |               2 | GLNPO\_Zoop | Summer      | Early2000s   |     137.2 |        \- |        \- |        \- |
| Huron |               3 | GLNPO\_Mys  | Spring      | Early2010s   |        \- |      14.7 |        \- |        \- |
| Huron |               3 | GLNPO\_Zoop | Spring      | Early2010s   |        \- |       9.6 |        \- |        \- |
| Huron |               3 | GLNPO\_Mys  | Summer      | Early2010s   |        \- |      46.7 |        \- |        \- |
| Huron |               3 | GLNPO\_Zoop | Summer      | Early2010s   |        \- |      32.6 |        \- |        \- |
| Huron |               3 | USGS        | Late Summer | Early2010s   |        \- |        \- |      57.2 |        \- |
| Huron |               4 | GLNPO\_Mys  | Spring      | Late2010s    |        \- |        \- |        \- |       6.6 |
| Huron |               4 | GLNPO\_Zoop | Spring      | Late2010s    |        \- |        \- |        \- |       7.3 |
| Huron |               4 | GLNPO\_Mys  | Summer      | Late2010s    |        \- |        \- |        \- |      24.9 |
| Huron |               4 | GLNPO\_Zoop | Summer      | Late2010s    |        \- |        \- |        \- |      13.7 |
| Huron |               4 | USGS        | Late Summer | Late2010s    |        \- |        \- |        \- |      49.1 |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-3.png)<!-- -->

| Lake     | Period\_Numeric | Group       | Season | Period\_Name | 1997-2005 | 2006-2015 | 2016-2019 |
| :------- | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: |
| Superior |               2 | GLNPO\_Zoop | Spring | Early2000s   |     107.6 |        \- |        \- |
| Superior |               2 | GLNPO\_Zoop | Summer | Early2000s   |     112.3 |        \- |        \- |
| Superior |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- |     112.6 |        \- |
| Superior |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |      85.7 |        \- |
| Superior |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- |     218.2 |        \- |
| Superior |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- |     161.4 |        \- |
| Superior |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- |     119.1 |
| Superior |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |      58.4 |
| Superior |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- |     206.1 |
| Superior |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- |     144.7 |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-4.png)<!-- -->

| Lake     | Period\_Numeric | Group       | Season | Period\_Name | 1997-2005 | 1995-2002 | 2006-2015 | 2007-2015 | 2005-2015 | 2016-2019 |
| :------- | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: | --------: | --------: | --------: |
| Michigan |               2 | GLNPO\_Zoop | Spring | Early2000s   |  418.2062 |        \- |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Spring | Early2000s   |        \- |  315.5142 |        \- |        \- |        \- |        \- |
| Michigan |               2 | GLNPO\_Zoop | Summer | Early2000s   |  843.8804 |        \- |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Summer | Early2000s   |        \- |  629.6175 |        \- |        \- |        \- |        \- |
| Michigan |               2 | NOAA        | Fall   | Early2000s   |        \- |  444.1379 |        \- |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- |        \- | 257.14473 |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |        \- |  99.43603 |        \- |        \- |        \- |
| Michigan |               3 | NOAA        | Spring | Early2010s   |        \- |        \- |        \- |  127.8157 |        \- |        \- |
| Michigan |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- |        \- | 393.58202 |        \- |        \- |        \- |
| Michigan |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- |        \- | 178.76100 |        \- |        \- |        \- |
| Michigan |               3 | NOAA        | Summer | Early2010s   |        \- |        \- |        \- |  283.1267 |        \- |        \- |
| Michigan |               3 | USGS        | Summer | Early2010s   |        \- |        \- |        \- |        \- |  532.4298 |        \- |
| Michigan |               3 | NOAA        | Fall   | Early2010s   |        \- |        \- |        \- |  282.7255 |        \- |        \- |
| Michigan |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- | 131.64528 |
| Michigan |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- |  37.81067 |
| Michigan |               4 | NOAA        | Spring | Late2010s    |        \- |        \- |        \- |        \- |        \- | 106.57694 |
| Michigan |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- | 240.73227 |
| Michigan |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- |  71.30444 |
| Michigan |               4 | NOAA        | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- | 114.55214 |
| Michigan |               4 | USGS        | Summer | Late2010s    |        \- |        \- |        \- |        \- |        \- | 219.95359 |
| Michigan |               4 | NOAA        | Fall   | Late2010s    |        \- |        \- |        \- |        \- |        \- | 151.71991 |

Lake Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-5.png)<!-- -->

| Lake    | Period\_Numeric | Group       | Season | Period\_Name | 1990-1995 | 1997-2005 | 2002-2005 | 2006-2015 | 2016-2019 | 2016-2017 |
| :------ | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: | --------: | --------: | --------: |
| Ontario |               1 | DFO         | Fall   | Early1990s   |  2022.262 |        \- |        \- |        \- |        \- |        \- |
| Ontario |               2 | GLNPO\_Zoop | Spring | Early2000s   |        \- |  126.9664 |        \- |        \- |        \- |        \- |
| Ontario |               2 | GLNPO\_Zoop | Summer | Early2000s   |        \- |  462.1577 |        \- |        \- |        \- |        \- |
| Ontario |               2 | DFO         | Fall   | Early2000s   |        \- |        \- |  918.3259 |        \- |        \- |        \- |
| Ontario |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- |        \- |        \- |  283.5927 |        \- |        \- |
| Ontario |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |        \- |        \- |  269.1748 |        \- |        \- |
| Ontario |               3 | DFO         | Summer | Early2010s   |        \- |        \- |        \- |  815.2259 |        \- |        \- |
| Ontario |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- |        \- |        \- |  746.9020 |        \- |        \- |
| Ontario |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- |        \- |        \- |  527.8137 |        \- |        \- |
| Ontario |               3 | DFO         | Fall   | Early2010s   |        \- |        \- |        \- |  723.0504 |        \- |        \- |
| Ontario |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- |        \- |        \- |  390.2066 |        \- |
| Ontario |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |        \- |        \- |  126.0133 |        \- |
| Ontario |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- |        \- |        \- |  579.5253 |        \- |
| Ontario |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- |        \- |        \- |  246.0588 |        \- |
| Ontario |               4 | DFO         | Fall   | Late2010s    |        \- |        \- |        \- |        \- |        \- |  667.2884 |

Lake Ontario

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-6.png)<!-- -->

| Lake  | Period\_Numeric | Group       | Season      | Period\_Name | 1997-2005 | 2006-2015 | 2005-2015 |  2016-2019 |
| :---- | --------------: | :---------- | :---------- | :----------- | --------: | --------: | --------: | ---------: |
| Huron |               2 | GLNPO\_Zoop | Spring      | Early2000s   |  102.1974 |        \- |        \- |         \- |
| Huron |               2 | GLNPO\_Zoop | Summer      | Early2000s   |  138.5930 |        \- |        \- |         \- |
| Huron |               3 | GLNPO\_Mys  | Spring      | Early2010s   |        \- |  37.98273 |        \- |         \- |
| Huron |               3 | GLNPO\_Zoop | Spring      | Early2010s   |        \- |  16.80587 |        \- |         \- |
| Huron |               3 | GLNPO\_Mys  | Summer      | Early2010s   |        \- |  92.12437 |        \- |         \- |
| Huron |               3 | GLNPO\_Zoop | Summer      | Early2010s   |        \- |  35.83608 |        \- |         \- |
| Huron |               3 | USGS        | Late Summer | Early2010s   |        \- |        \- |  157.5899 |         \- |
| Huron |               4 | GLNPO\_Mys  | Spring      | Late2010s    |        \- |        \- |        \- |  15.375909 |
| Huron |               4 | GLNPO\_Zoop | Spring      | Late2010s    |        \- |        \- |        \- |   4.607222 |
| Huron |               4 | GLNPO\_Mys  | Summer      | Late2010s    |        \- |        \- |        \- |  51.073056 |
| Huron |               4 | GLNPO\_Zoop | Summer      | Late2010s    |        \- |        \- |        \- |   9.250357 |
| Huron |               4 | USGS        | Late Summer | Late2010s    |        \- |        \- |        \- | 146.607807 |

Lake Huron

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-7.png)<!-- -->

| Lake     | Period\_Numeric | Group       | Season | Period\_Name | 1997-2005 | 2006-2015 | 2016-2019 |
| :------- | --------------: | :---------- | :----- | :----------- | --------: | --------: | --------: |
| Superior |               2 | GLNPO\_Zoop | Spring | Early2000s   |  145.9254 |        \- |        \- |
| Superior |               2 | GLNPO\_Zoop | Summer | Early2000s   |  153.3683 |        \- |        \- |
| Superior |               3 | GLNPO\_Mys  | Spring | Early2010s   |        \- | 274.40107 |        \- |
| Superior |               3 | GLNPO\_Zoop | Spring | Early2010s   |        \- |  90.90661 |        \- |
| Superior |               3 | GLNPO\_Mys  | Summer | Early2010s   |        \- | 441.83203 |        \- |
| Superior |               3 | GLNPO\_Zoop | Summer | Early2010s   |        \- | 184.22680 |        \- |
| Superior |               4 | GLNPO\_Mys  | Spring | Late2010s    |        \- |        \- | 251.70821 |
| Superior |               4 | GLNPO\_Zoop | Spring | Late2010s    |        \- |        \- |  65.15856 |
| Superior |               4 | GLNPO\_Mys  | Summer | Late2010s    |        \- |        \- | 402.64368 |
| Superior |               4 | GLNPO\_Zoop | Summer | Late2010s    |        \- |        \- | 145.04308 |

Lake Superior

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-8.png)<!-- -->

    ## [1] "Density for All Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-9.png)<!-- -->

    ## [1] "Biomass for All Lakes and Seasons on One Plot:"

![](GLNPO_Long_term_2019_files/figure-gfm/Compare%20values%20among%20lakes%20during%20periods%20by%20group-10.png)<!-- -->

| Period\_Numeric | Period\_Name | Period    | Lake     | Group       | N\_Years | dens\_Spring | se2\_dens\_Spring | biom\_Spring | se2\_biom\_Spring | dens\_Summer | se2\_dens\_Summer | biom\_Summer | se2\_biom\_Summer | dens\_Fall | se2\_dens\_Fall | biom\_Fall | se2\_biom\_Fall |
| --------------: | :----------- | :-------- | :------- | :---------- | -------: | -----------: | ----------------: | -----------: | ----------------: | -----------: | ----------------: | -----------: | ----------------: | ---------: | --------------: | ---------: | --------------: |
|               1 | Early1990s   | 1990-1995 | Ontario  | DFO         |        3 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      438.9 |       101.48762 |  2022.2617 |       915.12314 |
|               2 | Early2000s   | 1997-2005 | Superior | GLNPO\_Zoop |        7 |        107.6 |        37.8978003 |   145.925430 |         69.082645 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Superior | GLNPO\_Zoop |        9 |           \- |                \- |           \- |                \- |        112.3 |         25.786474 |  153.3682840 |        49.8479995 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Michigan | GLNPO\_Zoop |        7 |        181.6 |        51.6103243 |   418.206222 |        223.800080 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Michigan | GLNPO\_Zoop |        9 |           \- |                \- |           \- |                \- |        389.2 |        122.029616 |  843.8804233 |       320.0537500 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1995-2002 | Michigan | NOAA        |        6 |        155.6 |        43.3567834 |   315.514202 |        106.137311 |        247.5 |         81.514089 |  629.6174626 |       210.9832904 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1995-2002 | Michigan | NOAA        |        4 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      132.8 |        65.48068 |   444.1379 |       193.57174 |
|               2 | Early2000s   | 1997-2005 | Huron    | GLNPO\_Zoop |        7 |         53.3 |        33.2729548 |   102.197444 |         88.659525 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Huron    | GLNPO\_Zoop |        9 |           \- |                \- |           \- |                \- |        137.2 |         50.282135 |  138.5930208 |        22.4732772 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Erie     | GLNPO\_Zoop |        1 |          0.0 |                \- |     0.000000 |                \- |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Erie     | GLNPO\_Zoop |        7 |           \- |                \- |           \- |                \- |          2.5 |          2.209072 |    1.8289286 |         2.8806815 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Ontario  | GLNPO\_Zoop |        7 |         85.0 |        20.5584615 |   126.966407 |         56.749319 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 1997-2005 | Ontario  | GLNPO\_Zoop |        8 |           \- |                \- |           \- |                \- |        171.9 |         89.259705 |  462.1577222 |       264.4707284 |         \- |              \- |         \- |              \- |
|               2 | Early2000s   | 2002-2005 | Ontario  | DFO         |        4 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      254.3 |        57.34358 |   918.3259 |       211.55235 |
|               3 | Early2010s   | 2006-2015 | Superior | GLNPO\_Mys  |        9 |        112.6 |        36.4160526 |   274.401071 |         71.430281 |        218.2 |         46.659386 |  441.8320312 |       100.7866786 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Superior | GLNPO\_Zoop |       10 |         85.7 |        21.1001169 |    90.906610 |         19.457494 |        161.4 |         38.987547 |  184.2267965 |        71.3009875 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Michigan | GLNPO\_Mys  |       10 |        120.6 |        21.9002953 |   257.144730 |         61.731262 |        170.1 |         47.066067 |  393.5820230 |       176.5047008 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Michigan | GLNPO\_Zoop |       10 |         84.3 |        28.7685167 |    99.436025 |         62.042505 |        171.2 |         41.448410 |  178.7610000 |        96.6828763 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2005-2015 | Michigan | USGS        |       11 |           \- |                \- |           \- |                \- |        205.1 |         52.630555 |  532.4298477 |       193.5537403 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2007-2015 | Michigan | NOAA        |        9 |         48.0 |        20.6909744 |   127.815740 |         53.548782 |         98.2 |         30.070838 |  283.1267414 |        96.2165867 |       79.4 |        20.87544 |   282.7255 |        72.91184 |
|               3 | Early2010s   | 2006-2015 | Huron    | GLNPO\_Mys  |       10 |         14.7 |         5.1251060 |    37.982726 |         12.946224 |         46.7 |          8.983988 |   92.1243738 |        21.5738390 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Huron    | GLNPO\_Zoop |       10 |          9.6 |         4.2576154 |    16.805869 |         15.116121 |         32.6 |         10.784260 |   35.8360833 |        21.9120507 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2005-2015 | Huron    | USGS        |       11 |           \- |                \- |           \- |                \- |         57.2 |         12.596229 |  157.5898875 |        50.3152367 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Erie     | GLNPO\_Mys  |        5 |          0.2 |         0.2529822 |     1.683310 |          2.087222 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Erie     | GLNPO\_Mys  |        7 |           \- |                \- |           \- |                \- |          2.3 |          2.925236 |    1.0038069 |         1.0969912 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Erie     | GLNPO\_Zoop |        6 |          0.0 |         0.0000000 |     0.000000 |          0.000000 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Erie     | GLNPO\_Zoop |        8 |           \- |                \- |           \- |                \- |          1.4 |          1.161857 |    0.5397083 |         0.8014194 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Ontario  | GLNPO\_Mys  |       10 |        130.7 |        57.1322458 |   283.592715 |        122.812966 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Ontario  | GLNPO\_Mys  |        9 |           \- |                \- |           \- |                \- |        361.0 |        200.923266 |  746.9020016 |       266.1135518 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Ontario  | GLNPO\_Zoop |       10 |        169.3 |        76.8242522 |   269.174850 |        267.058160 |        254.3 |        112.119226 |  527.8136905 |       436.2629221 |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Ontario  | DFO         |        1 |           \- |                \- |           \- |                \- |        332.6 |                \- |  815.2259259 |                \- |         \- |              \- |         \- |              \- |
|               3 | Early2010s   | 2006-2015 | Ontario  | DFO         |        9 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      198.1 |        44.51159 |   723.0504 |       129.15255 |
|               4 | Late2010s    | 2016-2019 | Superior | GLNPO\_Mys  |        4 |        119.1 |        21.9821898 |   251.708208 |         28.448805 |        206.1 |         21.914227 |  402.6436834 |        62.0698381 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Superior | GLNPO\_Zoop |        3 |         58.4 |         9.8332768 |    65.158556 |         20.097887 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Superior | GLNPO\_Zoop |        4 |           \- |                \- |           \- |                \- |        144.7 |         28.482334 |  145.0430849 |        38.9780809 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Michigan | GLNPO\_Mys  |        4 |         51.8 |        31.7802218 |   131.645276 |         76.465785 |        119.4 |         70.032992 |  240.7322718 |       135.0204489 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Michigan | GLNPO\_Zoop |        3 |         53.5 |        54.1078347 |    37.810667 |         35.783057 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Michigan | GLNPO\_Zoop |        4 |           \- |                \- |           \- |                \- |         93.9 |         41.593139 |   71.3044444 |        31.9492210 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Michigan | USGS        |        4 |           \- |                \- |           \- |                \- |         91.9 |         58.982286 |  219.9535920 |       148.9214617 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Michigan | NOAA        |        4 |         33.6 |        30.3236294 |   106.576937 |         81.379788 |         43.0 |         36.435914 |  114.5521445 |        80.6980816 |       44.3 |        36.18300 |   151.7199 |       126.07551 |
|               4 | Late2010s    | 2016-2019 | Huron    | GLNPO\_Mys  |        4 |          6.6 |         2.7110883 |    15.375909 |          3.386767 |         24.9 |          6.863126 |   51.0730561 |        19.5808273 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Huron    | GLNPO\_Zoop |        3 |          7.3 |         5.3304159 |     4.607222 |          4.410829 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Huron    | GLNPO\_Zoop |        4 |           \- |                \- |           \- |                \- |         13.7 |          6.883313 |    9.2503571 |         7.6265670 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Huron    | USGS        |        4 |           \- |                \- |           \- |                \- |         49.1 |         15.054899 |  146.6078070 |        68.9114827 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Erie     | GLNPO\_Mys  |        3 |          0.9 |         1.5762121 |     6.018971 |         10.617432 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Erie     | GLNPO\_Mys  |        2 |           \- |                \- |           \- |                \- |          7.6 |         11.300000 |    1.6010773 |         2.5417753 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Erie     | GLNPO\_Zoop |        2 |          0.0 |         0.0000000 |     0.000000 |          0.000000 |          6.5 |          5.400000 |    2.9516667 |         4.0233333 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Ontario  | GLNPO\_Mys  |        4 |        200.7 |        25.7982396 |   390.206618 |         75.816349 |        240.7 |         79.100037 |  579.5253353 |        47.8604462 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Ontario  | GLNPO\_Zoop |        3 |        125.0 |         7.0354657 |   126.013333 |         96.335045 |           \- |                \- |           \- |                \- |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2019 | Ontario  | GLNPO\_Zoop |        4 |           \- |                \- |           \- |                \- |        175.2 |         71.691422 |  246.0587500 |        90.6275645 |         \- |              \- |         \- |              \- |
|               4 | Late2010s    | 2016-2017 | Ontario  | DFO         |        2 |           \- |                \- |           \- |                \- |           \- |                \- |           \- |                \- |      151.4 |        21.00000 |   667.2884 |        61.07681 |

Whole Table

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
    ## (Intercept)      3.05756    0.07894  38.731  < 2e-16 ***
    ## SeasonSummer     0.60984    0.06817   8.946  < 2e-16 ***
    ## SeasonFall       0.33082    0.13456   2.459   0.0142 *  
    ## GroupGLNPO_Zoop -0.19451    0.09101  -2.137   0.0330 *  
    ## GroupNOAA       -0.44639    0.09983  -4.472 9.34e-06 ***
    ## GroupUSGS       -0.10184    0.09890  -1.030   0.3036    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 7.898   8.65 31.73  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.413   Deviance explained = 42.6%
    ## -REML = 637.67  Scale est. = 0.46622   n = 593

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
    ## (Intercept)      3.66704    0.23508  15.599  < 2e-16 ***
    ## SeasonSummer     0.65274    0.11785   5.539 4.41e-08 ***
    ## SeasonFall       0.03201    0.23985   0.133   0.8939    
    ## GroupGLNPO_Mys  -0.20601    0.23426  -0.879   0.3795    
    ## GroupGLNPO_Zoop -0.53779    0.22564  -2.383   0.0174 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 2.963  3.674 17.57 1.7e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.155   Deviance explained = 16.4%
    ## -REML =  837.3  Scale est. = 0.70501   n = 666

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
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.79766    0.09366  19.194  < 2e-16 ***
    ## SeasonSummer       0.84199    0.09523   8.842  < 2e-16 ***
    ## SeasonLate Summer  0.90373    0.11883   7.605 3.01e-13 ***
    ## GroupGLNPO_Zoop   -0.42854    0.10684  -4.011 7.49e-05 ***
    ## GroupUSGS          0.00000    0.00000      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F  p-value    
    ## s(Year) 4.777  5.842 12.32 2.78e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Rank: 13/14
    ## R-sq.(adj) =  0.378   Deviance explained = 39.2%
    ## -REML = 383.59  Scale est. = 0.53258   n = 338

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
    ## (Intercept)      3.24325    0.06311  51.387  < 2e-16 ***
    ## SeasonSummer     0.41541    0.05923   7.013 8.04e-12 ***
    ## GroupGLNPO_Zoop -0.36876    0.06905  -5.341 1.44e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value
    ## s(Year) 3.127  3.884 1.752   0.194
    ## 
    ## R-sq.(adj) =  0.166   Deviance explained = 17.5%
    ## -REML = 479.39  Scale est. = 0.41223   n = 482

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
    ## (Intercept)       0.3470     0.1498   2.316 0.022734 *  
    ## SeasonSummer      0.5302     0.1307   4.057 0.000102 ***
    ## GroupGLNPO_Zoop  -0.3437     0.1466  -2.344 0.021143 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value  
    ## s(Year) 2.245  2.796 3.838  0.0231 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.241   Deviance explained = 27.3%
    ## -REML = 98.107  Scale est. = 0.37787   n = 100

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
    ## (Intercept)      3.72956    0.10083  36.987  < 2e-16 ***
    ## SeasonSummer     0.75242    0.08918   8.437 3.44e-16 ***
    ## SeasonFall       0.67718    0.17304   3.913 0.000103 ***
    ## GroupGLNPO_Zoop -0.89800    0.11821  -7.596 1.49e-13 ***
    ## GroupNOAA       -0.37427    0.12750  -2.935 0.003482 ** 
    ## GroupUSGS        0.11661    0.14002   0.833 0.405331    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##         edf Ref.df    F p-value    
    ## s(Year) 8.4  8.885 37.1  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.45   Deviance explained = 46.5%
    ## -REML = 688.02  Scale est. = 0.75483   n = 520

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
    ## (Intercept)       4.4599     0.3134  14.231  < 2e-16 ***
    ## SeasonSummer      1.1008     0.1602   6.871  1.5e-11 ***
    ## SeasonFall        0.6877     0.3197   2.151 0.031848 *  
    ## GroupGLNPO_Mys   -0.2594     0.3112  -0.834 0.404798    
    ## GroupGLNPO_Zoop  -1.0055     0.3008  -3.343 0.000876 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value    
    ## s(Year) 3.309  4.093 34.33  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.369   Deviance explained = 37.6%
    ## -REML = 1004.2  Scale est. = 1.2281    n = 655

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
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         2.2507     0.1062  21.187  < 2e-16 ***
    ## SeasonSummer        0.8411     0.1100   7.647 2.89e-13 ***
    ## SeasonLate Summer   0.0000     0.0000      NA       NA    
    ## GroupGLNPO_Zoop    -1.0340     0.1225  -8.442 1.40e-15 ***
    ## GroupUSGS           1.3376     0.1446   9.250  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df    F p-value    
    ## s(Year) 7.163  8.134 14.9  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Rank: 13/14
    ## R-sq.(adj) =    0.5   Deviance explained = 51.7%
    ## -REML = 393.75  Scale est. = 0.68519   n = 308

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
    ## (Intercept)      4.03217    0.07133  56.526  < 2e-16 ***
    ## SeasonSummer     0.46249    0.06776   6.825 2.75e-11 ***
    ## GroupGLNPO_Zoop -1.10971    0.07866 -14.108  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value  
    ## s(Year) 4.807  5.862 2.885  0.0106 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.368   Deviance explained = 37.7%
    ## -REML = 531.48  Scale est. = 0.52841   n = 473

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
    ## (Intercept)       0.5003     0.1330   3.762 0.000291 ***
    ## SeasonSummer      0.2756     0.1159   2.378 0.019405 *  
    ## GroupGLNPO_Zoop  -0.4323     0.1301  -3.322 0.001270 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##           edf Ref.df     F p-value  
    ## s(Year) 2.054   2.56 2.576  0.0878 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.192   Deviance explained = 22.5%
    ## -REML = 86.513  Scale est. = 0.29807   n = 100

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

    ## # A tibble: 15,934 x 2
    ##    Visit   Length_mm
    ##    <chr>       <dbl>
    ##  1 M032G00       7.9
    ##  2 M032G00      10.2
    ##  3 M032G00       7.9
    ##  4 M032G00      10.3
    ##  5 M032G00       5.7
    ##  6 M032G00       5.3
    ##  7 M032G00       9.6
    ##  8 M032G00       9.2
    ##  9 M032G00      13.1
    ## 10 M032G00      18.8
    ## # ... with 15,924 more rows

    ## # A tibble: 35,750 x 5
    ##    Visit   Length_mm Lake      Year Season
    ##    <chr>       <dbl> <fct>    <int> <fct> 
    ##  1 M032G00       7.9 Michigan  2000 Summer
    ##  2 M032G00      10.2 Michigan  2000 Summer
    ##  3 M032G00       7.9 Michigan  2000 Summer
    ##  4 M032G00      10.3 Michigan  2000 Summer
    ##  5 M032G00       5.7 Michigan  2000 Summer
    ##  6 M032G00       5.3 Michigan  2000 Summer
    ##  7 M032G00       9.6 Michigan  2000 Summer
    ##  8 M032G00       9.2 Michigan  2000 Summer
    ##  9 M032G00      13.1 Michigan  2000 Summer
    ## 10 M032G00      18.8 Michigan  2000 Summer
    ## # ... with 35,740 more rows

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
    ##  1 06GB50M5    2.7
    ##  2 06GB50M5    2.1
    ##  3 06GB50M5    2.9
    ##  4 06GB50M5    2.1
    ##  5 06GB50M5    2.4
    ##  6 06GB50M5    2.6
    ##  7 06GB50M5    2.5
    ##  8 06GB50M5    2.4
    ##  9 06GB50M5    2.6
    ## 10 06GB50M5    2.7
    ## # ... with 220,621 more rows

    ## # A tibble: 215,785 x 5
    ##    Visit    Length Lake   Year Season
    ##    <chr>     <dbl> <fct> <int> <fct> 
    ##  1 06GB50M5    2.7 Huron  2006 Summer
    ##  2 06GB50M5    2.1 Huron  2006 Summer
    ##  3 06GB50M5    2.9 Huron  2006 Summer
    ##  4 06GB50M5    2.1 Huron  2006 Summer
    ##  5 06GB50M5    2.4 Huron  2006 Summer
    ##  6 06GB50M5    2.6 Huron  2006 Summer
    ##  7 06GB50M5    2.5 Huron  2006 Summer
    ##  8 06GB50M5    2.4 Huron  2006 Summer
    ##  9 06GB50M5    2.6 Huron  2006 Summer
    ## 10 06GB50M5    2.7 Huron  2006 Summer
    ## # ... with 215,775 more rows

#### Wrangle USGS and NOAA Individual Data

<br>

## Calculate summary values from life history data

### Brooding Females Data Plotting and Analysis

#### Brooding Females - all groups and seasons

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-2.png)<!-- -->

    ## # A tibble: 13 x 8
    ## # Groups:   Lake, Season [11]
    ##    Lake     Season Group     Brood_Count_Mean Brood_Count_SE BF_Length_Mean
    ##    <chr>    <fct>  <chr>                <dbl>          <dbl>          <dbl>
    ##  1 Erie     Spring GLNPO_Mys             26.2          1.93            15.4
    ##  2 Michigan Fall   NOAA                  21.7          0.261           17.1
    ##  3 Ontario  Spring GLNPO_Mys             15.9          0.331           14.0
    ##  4 Michigan Spring GLNPO_Mys             15.9          0.234           14.7
    ##  5 Michigan Summer NOAA                  18.1          0.382           16.7
    ##  6 Huron    Summer GLNPO_Mys             15.3          0.763           14.9
    ##  7 Michigan Winter NOAA                  15.0          0.299           14.9
    ##  8 Michigan Summer GLNPO_Mys             15.5          0.358           15.7
    ##  9 Michigan Spring NOAA                  14.6          0.214           15.0
    ## 10 Huron    Spring GLNPO_Mys             13.9          0.290           14.7
    ## 11 Ontario  Summer GLNPO_Mys             14.8          1.30            15.8
    ## 12 Superior Summer GLNPO_Mys             13.0          0.409           15.6
    ## 13 Superior Spring GLNPO_Mys             12.4          0.165           14.9
    ##    BF_Length_SE Brood_per_mm
    ##           <dbl>        <dbl>
    ##  1       0.148         1.69 
    ##  2       0.0417        1.27 
    ##  3       0.0580        1.14 
    ##  4       0.0671        1.08 
    ##  5       0.0734        1.08 
    ##  6       0.240         1.03 
    ##  7       0.0828        1.00 
    ##  8       0.111         0.987
    ##  9       0.0388        0.978
    ## 10       0.0673        0.947
    ## 11       0.669         0.936
    ## 12       0.211         0.833
    ## 13       0.0552        0.833

    ## Warning: Ignoring unknown aesthetics: linestyle

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20across%20all%20lakes,%20seasons,%20and%20groups-3.png)<!-- -->

#### Brooding females - GLNPO Spring across lakes

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-2.png)<!-- -->

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-3.png)<!-- -->

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning in qt((1 - level)/2, df): NaNs produced
    
    ## Warning in qt((1 - level)/2, df): NaNs produced

    ## Warning: Removed 2 rows containing missing values (geom_point).

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf
    
    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20brooding%20females%20all%20lakes,%20in%20spring%20by%20GLNPO-4.png)<!-- -->

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake     Season Group     Brood_Count_Mean Brood_Count_SE BF_Length_Mean
    ##   <chr>    <fct>  <chr>                <dbl>          <dbl>          <dbl>
    ## 1 Erie     Spring GLNPO_Mys             26.2          1.93            15.4
    ## 2 Ontario  Spring GLNPO_Mys             15.9          0.331           14.0
    ## 3 Michigan Spring GLNPO_Mys             15.9          0.234           14.7
    ## 4 Huron    Spring GLNPO_Mys             13.9          0.290           14.7
    ## 5 Superior Spring GLNPO_Mys             12.4          0.165           14.9
    ##   BF_Length_SE Brood_per_mm
    ##          <dbl>        <dbl>
    ## 1       0.148         1.69 
    ## 2       0.0580        1.14 
    ## 3       0.0671        1.08 
    ## 4       0.0673        0.947
    ## 5       0.0552        0.833

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake  Season Group Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <chr> <fct>  <chr>            <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Erie  Spring GLNP~             26.2          1.93            15.4       0.148 
    ## 2 Onta~ Spring GLNP~             15.9          0.331           14.0       0.0580
    ## 3 Mich~ Spring GLNP~             15.9          0.234           14.7       0.0671
    ## 4 Huron Spring GLNP~             13.9          0.290           14.7       0.0673
    ## 5 Supe~ Spring GLNP~             12.4          0.165           14.9       0.0552
    ## # ... with 1 more variable: Brood_per_mm <dbl>

#### Brooding females - NOAA Spring-Fall in Michigan

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-2.png)<!-- -->

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/plot%20tibble%20of%20NOAA%20brooding%20females%20Michigan%20in%20all%20seasons-3.png)<!-- -->

    ## # A tibble: 6 x 7
    ## # Groups:   Group [2]
    ##   Group     Season Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <chr>     <fct>             <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 GLNPO_Mys Spring             15.1         0.0903           14.6       0.0160
    ## 2 GLNPO_Mys Summer             14.5         0.137            15.5       0.0574
    ## 3 NOAA      Fall               21.7         0.261            17.1       0.0417
    ## 4 NOAA      Summer             18.1         0.382            16.7       0.0734
    ## 5 NOAA      Winter             15.0         0.299            14.9       0.0828
    ## 6 NOAA      Spring             14.6         0.214            15.0       0.0388
    ##   Brood_per_mm
    ##          <dbl>
    ## 1        1.04 
    ## 2        0.937
    ## 3        1.27 
    ## 4        1.08 
    ## 5        1.00 
    ## 6        0.978

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
    ## 2 observations deleted due to missingness

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908  42.061 <2e-16 ***
    ## Length         1   6704    6704 310.543 <2e-16 ***
    ## Lake:Length    4     13       3   0.155  0.961    
    ## Residuals   1085  23422      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 2 observations deleted due to missingness

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
    ## LakeHuron           -20.5549    64.1306  -0.321    0.749
    ## LakeMichigan        -18.0450    63.9048  -0.282    0.778
    ## LakeOntario         -14.4146    63.9111  -0.226    0.822
    ## LakeSuperior        -20.1932    63.8865  -0.316    0.752
    ## Length                1.0370     4.1070   0.252    0.801
    ## LakeHuron:Length      0.7621     4.1282   0.185    0.854
    ## LakeMichigan:Length   0.6739     4.1107   0.164    0.870
    ## LakeOntario:Length    0.5085     4.1115   0.124    0.902
    ## LakeSuperior:Length   0.5693     4.1094   0.139    0.890
    ## 
    ## Residual standard error: 4.646 on 1085 degrees of freedom
    ##   (2 observations deleted due to missingness)
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
    ## 2 observations deleted due to missingness

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
    ## 2 observations deleted due to missingness

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908   42.19 <2e-16 ***
    ## Length         1   6704    6704  311.51 <2e-16 ***
    ## Residuals   1089  23436      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 2 observations deleted due to missingness

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
    ## LakeHuron     -8.93372    2.14886  -4.157 3.47e-05 ***
    ## LakeMichigan  -7.63451    2.09768  -3.640 0.000286 ***
    ## LakeOntario   -6.40320    2.09711  -3.053 0.002318 ** 
    ## LakeSuperior -11.33516    2.08526  -5.436 6.73e-08 ***
    ## Length         1.63056    0.09238  17.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.639 on 1089 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.3029 
    ## F-statistic: 96.06 on 5 and 1089 DF,  p-value: < 2.2e-16

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Analyses-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Analyses-2.png)<!-- -->

    ## Warning in replications(paste("~", xx), data = mf): non-factors ignored: Length

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Embryos ~ Lake + Length, data = Mysids_BroodingFemales %>% filter(Season == "Spring", Group == "GLNPO_Mys"))
    ## 
    ## $Lake
    ##                          diff         lwr        upr     p adj
    ## Huron-Erie        -11.0666667 -16.9288241 -5.2045092 0.0000029
    ## Michigan-Erie      -8.8788136 -14.6071812 -3.1504459 0.0002397
    ## Ontario-Erie       -8.5180812 -14.2387691 -2.7973932 0.0004863
    ## Superior-Erie     -12.1436399 -17.8399387 -6.4473411 0.0000001
    ## Michigan-Huron      2.1878531   0.4813131  3.8943931 0.0043573
    ## Ontario-Huron       2.5485855   0.8680042  4.2291668 0.0003547
    ## Superior-Huron     -1.0769733  -2.6725619  0.5186154 0.3486140
    ## Ontario-Michigan    0.3607324  -0.7678329  1.4892977 0.9066716
    ## Superior-Michigan  -3.2648264  -4.2624281 -2.2672246 0.0000000
    ## Superior-Ontario   -3.6255587  -4.5780734 -2.6730441 0.0000000

    ## Warning in replications(paste("~", xx), data = mf): non-factors ignored: Length

    ##                        diff         lwr        upr        p adj
    ## Ontario-Erie     -8.5180812 -14.2387691 -2.7973932 0.0004862566
    ## Ontario-Michigan  0.3607324  -0.7678329  1.4892977 0.9066716483
    ## Michigan-Huron    2.1878531   0.4813131  3.8943931 0.0043573198
    ## Superior-Huron   -1.0769733  -2.6725619  0.5186154 0.3486140038

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](GLNPO_Long_term_2019_files/figure-gfm/Brooding%20Female%20Analyses-3.png)<!-- -->

#### Key plots and analyses of Brooding Females data

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females-1.png)<!-- -->![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females-2.png)<!-- -->

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females-3.png)<!-- -->

    ##               Df Sum Sq Mean Sq F value Pr(>F)    
    ## Lake           4   3632     908   42.19 <2e-16 ***
    ## Length         1   6704    6704  311.51 <2e-16 ***
    ## Residuals   1089  23436      22                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 2 observations deleted due to missingness

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
    ## LakeHuron     -8.93372    2.14886  -4.157 3.47e-05 ***
    ## LakeMichigan  -7.63451    2.09768  -3.640 0.000286 ***
    ## LakeOntario   -6.40320    2.09711  -3.053 0.002318 ** 
    ## LakeSuperior -11.33516    2.08526  -5.436 6.73e-08 ***
    ## Length         1.63056    0.09238  17.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.639 on 1089 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.3029 
    ## F-statistic: 96.06 on 5 and 1089 DF,  p-value: < 2.2e-16

![](GLNPO_Long_term_2019_files/figure-gfm/Key%20plots%20analyses%20brooding%20females-4.png)<!-- -->

    ## # A tibble: 5 x 8
    ## # Groups:   Lake, Season [5]
    ##   Lake  Season Group Brood_Count_Mean Brood_Count_SE BF_Length_Mean BF_Length_SE
    ##   <chr> <fct>  <chr>            <dbl>          <dbl>          <dbl>        <dbl>
    ## 1 Erie  Spring GLNP~             26.2          1.93            15.4       0.148 
    ## 2 Onta~ Spring GLNP~             15.9          0.331           14.0       0.0580
    ## 3 Mich~ Spring GLNP~             15.9          0.234           14.7       0.0671
    ## 4 Huron Spring GLNP~             13.9          0.290           14.7       0.0673
    ## 5 Supe~ Spring GLNP~             12.4          0.165           14.9       0.0552
    ## # ... with 1 more variable: Brood_per_mm <dbl>

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
      mutate(Group = "USGS"),
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

# Mysid_Mclust_Fits[[5]]
# Mysid_Mclust_Fits[[50]]
# Mysid_Mclust_Fits[[150]]
# 
# Extract_Mclust_values(
#   Mysid_Mclust_Fits[[50]]
# )
# 
# Extract_Mclust_values(
#   Mysid_Mclust_Fits[[150]]
# )
# 
# class(Mysid_Mclust_Fits[[5]])
# class(Mysid_Mclust_Fits[[50]])
# class(Mysid_Mclust_Fits[[150]])

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
      
      summarize(MeanDens = mean(Dens))
    
    ) %>% 
  
  mutate(Dens1 = Prop_1 * MeanDens,
         
         Dens2 = Prop_2 * MeanDens)


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
  
  
  filter(Season != "Late Summer" | 
           Group != "USGS" |
           Year != 2012
           ) %>% 
  
  
  filter(Season != "Fall" | 
           Group != "NOAA" |
           !(Year %in% c(1996, 2013, 2018, 2019))
           )
```

#### Size Structure plots

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 1: Lake = "Huron", Year = 1997, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 2: Lake = "Huron", Year = 2000, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 3: Lake = "Huron", Year = 2001, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 4: Lake = "Huron", Year = 2003, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 5: Lake = "Huron", Year = 2003, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 6: Lake = "Huron", Year = 2004, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 7: Lake = "Huron", Year = 2004, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 8: Lake = "Huron", Year = 2005, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 9: Lake = "Huron", Year = 2005, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 11: Lake = "Huron", Year = 2006, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 12: Lake = "Huron", Year = 2006, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 13: Lake = "Huron", Year = 2007, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 14: Lake = "Huron", Year = 2007, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 15: Lake = "Huron", Year = 2007, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 17: Lake = "Huron", Year = 2008, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 18: Lake = "Huron", Year = 2008, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 19: Lake = "Huron", Year = 2008, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 20: Lake = "Huron", Year = 2008, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 22: Lake = "Huron", Year = 2009, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 23: Lake = "Huron", Year = 2009, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 25: Lake = "Huron", Year = 2010, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 26: Lake = "Huron", Year = 2010, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 27: Lake = "Huron", Year = 2010, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 29: Lake = "Huron", Year = 2011, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 30: Lake = "Huron", Year = 2011, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 31: Lake = "Huron", Year = 2011, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 32: Lake = "Huron", Year = 2012, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 33: Lake = "Huron", Year = 2012, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 34: Lake = "Huron", Year = 2012, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 36: Lake = "Huron", Year = 2013, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 37: Lake = "Huron", Year = 2013, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 39: Lake = "Huron", Year = 2014, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 40: Lake = "Huron", Year = 2014, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 42: Lake = "Huron", Year = 2015, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 43: Lake = "Huron", Year = 2015, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 44: Lake = "Huron", Year = 2015, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 46: Lake = "Huron", Year = 2016, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 47: Lake = "Huron", Year = 2016, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 48: Lake = "Huron", Year = 2016, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 50: Lake = "Huron", Year = 2017, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 51: Lake = "Huron", Year = 2017, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 52: Lake = "Huron", Year = 2017, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 54: Lake = "Huron", Year = 2018, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 55: Lake = "Huron", Year = 2018, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 57: Lake = "Huron", Year = 2019, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 58: Lake = "Huron", Year = 2019, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 59: Lake = "Michigan", Year = 1995, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 60: Lake = "Michigan", Year = 1995, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 61: Lake = "Michigan", Year = 1996, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 62: Lake = "Michigan", Year = 1996, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 63: Lake = "Michigan", Year = 1998, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 64: Lake = "Michigan", Year = 1998, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 65: Lake = "Michigan", Year = 1998, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 66: Lake = "Michigan", Year = 1998, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 67: Lake = "Michigan", Year = 1998, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 68: Lake = "Michigan", Year = 1999, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 69: Lake = "Michigan", Year = 1999, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 70: Lake = "Michigan", Year = 1999, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 71: Lake = "Michigan", Year = 1999, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 72: Lake = "Michigan", Year = 2000, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 73: Lake = "Michigan", Year = 2000, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 74: Lake = "Michigan", Year = 2000, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 75: Lake = "Michigan", Year = 2000, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 76: Lake = "Michigan", Year = 2001, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 77: Lake = "Michigan", Year = 2001, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 78: Lake = "Michigan", Year = 2002, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 79: Lake = "Michigan", Year = 2002, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 80: Lake = "Michigan", Year = 2003, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 81: Lake = "Michigan", Year = 2004, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 82: Lake = "Michigan", Year = 2005, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 83: Lake = "Michigan", Year = 2006, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 84: Lake = "Michigan", Year = 2006, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 85: Lake = "Michigan", Year = 2006, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 86: Lake = "Michigan", Year = 2007, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 87: Lake = "Michigan", Year = 2007, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 88: Lake = "Michigan", Year = 2007, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 89: Lake = "Michigan", Year = 2007, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 90: Lake = "Michigan", Year = 2007, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 91: Lake = "Michigan", Year = 2007, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 92: Lake = "Michigan", Year = 2007, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 93: Lake = "Michigan", Year = 2007, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 94: Lake = "Michigan", Year = 2008, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 95: Lake = "Michigan", Year = 2008, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 96: Lake = "Michigan", Year = 2008, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 97: Lake = "Michigan", Year = 2008, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 98: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 99: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 100: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 101: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 102: Lake = "Michigan", Year = 2009, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 103: Lake = "Michigan", Year = 2009, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 104: Lake = "Michigan", Year = 2009, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 105: Lake = "Michigan", Year = 2009, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 106: Lake = "Michigan", Year = 2009, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 107: Lake = "Michigan", Year = 2009, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 108: Lake = "Michigan", Year = 2009, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 109: Lake = "Michigan", Year = 2010, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 110: Lake = "Michigan", Year = 2010, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 111: Lake = "Michigan", Year = 2010, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 112: Lake = "Michigan", Year = 2010, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 113: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 114: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 115: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 116: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 117: Lake = "Michigan", Year = 2010, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 118: Lake = "Michigan", Year = 2011, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 119: Lake = "Michigan", Year = 2011, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 120: Lake = "Michigan", Year = 2011, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 121: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 122: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 123: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 124: Lake = "Michigan", Year = 2011, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 125: Lake = "Michigan", Year = 2012, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 126: Lake = "Michigan", Year = 2012, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 127: Lake = "Michigan", Year = 2012, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 128: Lake = "Michigan", Year = 2012, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 129: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 130: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 131: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 132: Lake = "Michigan", Year = 2012, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 133: Lake = "Michigan", Year = 2013, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 134: Lake = "Michigan", Year = 2013, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 135: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 136: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 137: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 138: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 139: Lake = "Michigan", Year = 2013, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 140: Lake = "Michigan", Year = 2014, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 141: Lake = "Michigan", Year = 2014, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 142: Lake = "Michigan", Year = 2014, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 143: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 144: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 145: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 146: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 147: Lake = "Michigan", Year = 2015, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 148: Lake = "Michigan", Year = 2015, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 149: Lake = "Michigan", Year = 2015, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 150: Lake = "Michigan", Year = 2015, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 151: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 152: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 153: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 154: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 155: Lake = "Michigan", Year = 2016, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 156: Lake = "Michigan", Year = 2016, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 157: Lake = "Michigan", Year = 2016, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 158: Lake = "Michigan", Year = 2016, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 159: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 160: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 161: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 162: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 163: Lake = "Michigan", Year = 2016, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 164: Lake = "Michigan", Year = 2017, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 165: Lake = "Michigan", Year = 2017, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 166: Lake = "Michigan", Year = 2017, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 167: Lake = "Michigan", Year = 2017, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 168: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 169: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 170: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 171: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 172: Lake = "Michigan", Year = 2017, Season = "Winter", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 173: Lake = "Michigan", Year = 2018, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 174: Lake = "Michigan", Year = 2018, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 175: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 176: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 177: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 178: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 179: Lake = "Michigan", Year = 2019, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 180: Lake = "Michigan", Year = 2019, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 181: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 182: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 183: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 184: Lake = "Ontario", Year = 2001, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 185: Lake = "Ontario", Year = 2001, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 186: Lake = "Ontario", Year = 2002, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 187: Lake = "Ontario", Year = 2003, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 188: Lake = "Ontario", Year = 2004, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 189: Lake = "Ontario", Year = 2004, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 190: Lake = "Ontario", Year = 2005, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 191: Lake = "Ontario", Year = 2005, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 192: Lake = "Ontario", Year = 2006, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 193: Lake = "Ontario", Year = 2006, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 194: Lake = "Ontario", Year = 2006, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 195: Lake = "Ontario", Year = 2007, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 196: Lake = "Ontario", Year = 2007, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 197: Lake = "Ontario", Year = 2007, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 198: Lake = "Ontario", Year = 2007, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 199: Lake = "Ontario", Year = 2008, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 200: Lake = "Ontario", Year = 2008, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 201: Lake = "Ontario", Year = 2008, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 202: Lake = "Ontario", Year = 2008, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 203: Lake = "Ontario", Year = 2009, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 204: Lake = "Ontario", Year = 2009, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 205: Lake = "Ontario", Year = 2009, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 206: Lake = "Ontario", Year = 2009, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 207: Lake = "Ontario", Year = 2010, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 208: Lake = "Ontario", Year = 2010, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 209: Lake = "Ontario", Year = 2010, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 210: Lake = "Ontario", Year = 2010, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 211: Lake = "Ontario", Year = 2011, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 212: Lake = "Ontario", Year = 2011, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 213: Lake = "Ontario", Year = 2011, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 214: Lake = "Ontario", Year = 2012, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 215: Lake = "Ontario", Year = 2012, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 216: Lake = "Ontario", Year = 2012, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 217: Lake = "Ontario", Year = 2013, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 218: Lake = "Ontario", Year = 2013, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 219: Lake = "Ontario", Year = 2013, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 220: Lake = "Ontario", Year = 2013, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 221: Lake = "Ontario", Year = 2014, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 222: Lake = "Ontario", Year = 2014, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 223: Lake = "Ontario", Year = 2014, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 224: Lake = "Ontario", Year = 2014, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 225: Lake = "Ontario", Year = 2015, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 226: Lake = "Ontario", Year = 2015, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 227: Lake = "Ontario", Year = 2015, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 228: Lake = "Ontario", Year = 2015, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 229: Lake = "Ontario", Year = 2016, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 230: Lake = "Ontario", Year = 2016, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 231: Lake = "Ontario", Year = 2016, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 232: Lake = "Ontario", Year = 2016, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 233: Lake = "Ontario", Year = 2017, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 234: Lake = "Ontario", Year = 2017, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 235: Lake = "Ontario", Year = 2017, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 236: Lake = "Ontario", Year = 2017, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 237: Lake = "Ontario", Year = 2018, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 238: Lake = "Ontario", Year = 2018, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 239: Lake = "Ontario", Year = 2018, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 240: Lake = "Ontario", Year = 2019, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 241: Lake = "Ontario", Year = 2019, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 242: Lake = "Superior", Year = 1997, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 243: Lake = "Superior", Year = 1998, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 244: Lake = "Superior", Year = 1998, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 245: Lake = "Superior", Year = 2000, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 246: Lake = "Superior", Year = 2001, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 247: Lake = "Superior", Year = 2002, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 248: Lake = "Superior", Year = 2002, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 249: Lake = "Superior", Year = 2003, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 250: Lake = "Superior", Year = 2003, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 251: Lake = "Superior", Year = 2004, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 252: Lake = "Superior", Year = 2004, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 253: Lake = "Superior", Year = 2005, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 254: Lake = "Superior", Year = 2005, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 255: Lake = "Superior", Year = 2006, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 256: Lake = "Superior", Year = 2006, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 257: Lake = "Superior", Year = 2007, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 258: Lake = "Superior", Year = 2007, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 259: Lake = "Superior", Year = 2007, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 260: Lake = "Superior", Year = 2007, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 261: Lake = "Superior", Year = 2008, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 262: Lake = "Superior", Year = 2008, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 263: Lake = "Superior", Year = 2008, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 264: Lake = "Superior", Year = 2008, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 265: Lake = "Superior", Year = 2009, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 266: Lake = "Superior", Year = 2009, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 267: Lake = "Superior", Year = 2009, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 268: Lake = "Superior", Year = 2009, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 269: Lake = "Superior", Year = 2010, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 270: Lake = "Superior", Year = 2010, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 271: Lake = "Superior", Year = 2010, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 272: Lake = "Superior", Year = 2010, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 273: Lake = "Superior", Year = 2011, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 274: Lake = "Superior", Year = 2011, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 275: Lake = "Superior", Year = 2011, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 276: Lake = "Superior", Year = 2011, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 277: Lake = "Superior", Year = 2012, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 278: Lake = "Superior", Year = 2012, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 279: Lake = "Superior", Year = 2012, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 280: Lake = "Superior", Year = 2012, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 281: Lake = "Superior", Year = 2013, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 282: Lake = "Superior", Year = 2013, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 283: Lake = "Superior", Year = 2013, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 284: Lake = "Superior", Year = 2013, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 285: Lake = "Superior", Year = 2014, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 286: Lake = "Superior", Year = 2014, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 287: Lake = "Superior", Year = 2014, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 288: Lake = "Superior", Year = 2014, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 289: Lake = "Superior", Year = 2015, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 290: Lake = "Superior", Year = 2015, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 291: Lake = "Superior", Year = 2015, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 292: Lake = "Superior", Year = 2015, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 293: Lake = "Superior", Year = 2016, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 294: Lake = "Superior", Year = 2016, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 295: Lake = "Superior", Year = 2016, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 296: Lake = "Superior", Year = 2016, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 297: Lake = "Superior", Year = 2017, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 298: Lake = "Superior", Year = 2017, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 299: Lake = "Superior", Year = 2017, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 300: Lake = "Superior", Year = 2018, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 301: Lake = "Superior", Year = 2018, Season = "Spring", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 302: Lake = "Superior", Year = 2018, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 303: Lake = "Superior", Year = 2018, Season = "Summer", Group = "GLNPO_Zoop".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 304: Lake = "Superior", Year = 2019, Season = "Spring", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Season`.
    ## i Unknown levels in `f`: Late Summer
    ## i Input `Season` is `factor(...)`.
    ## i The error occurred in group 305: Lake = "Superior", Year = 2019, Season = "Summer", Group = "GLNPO_Mys".

    ## Warning: Unknown levels in `f`: Late Summer

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 3: Lake = "Huron", Year = 2006, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 8: Lake = "Huron", Year = 2008, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 11: Lake = "Huron", Year = 2009, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 14: Lake = "Huron", Year = 2010, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 17: Lake = "Huron", Year = 2011, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 22: Lake = "Huron", Year = 2013, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 25: Lake = "Huron", Year = 2014, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 28: Lake = "Huron", Year = 2015, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 31: Lake = "Huron", Year = 2016, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 34: Lake = "Huron", Year = 2017, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 37: Lake = "Huron", Year = 2018, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 40: Lake = "Huron", Year = 2019, Season = "Fall", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 41: Lake = "Michigan", Year = 1995, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 42: Lake = "Michigan", Year = 1995, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 43: Lake = "Michigan", Year = 1996, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 44: Lake = "Michigan", Year = 1996, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 45: Lake = "Michigan", Year = 1998, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 46: Lake = "Michigan", Year = 1998, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 47: Lake = "Michigan", Year = 1998, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 48: Lake = "Michigan", Year = 1999, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 49: Lake = "Michigan", Year = 1999, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 50: Lake = "Michigan", Year = 1999, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 51: Lake = "Michigan", Year = 2000, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 52: Lake = "Michigan", Year = 2000, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 53: Lake = "Michigan", Year = 2000, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 54: Lake = "Michigan", Year = 2002, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 58: Lake = "Michigan", Year = 2007, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 60: Lake = "Michigan", Year = 2007, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 61: Lake = "Michigan", Year = 2007, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 63: Lake = "Michigan", Year = 2008, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 65: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 66: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 67: Lake = "Michigan", Year = 2008, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 69: Lake = "Michigan", Year = 2009, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 71: Lake = "Michigan", Year = 2009, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 72: Lake = "Michigan", Year = 2009, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 74: Lake = "Michigan", Year = 2010, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 76: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 77: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 78: Lake = "Michigan", Year = 2010, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 80: Lake = "Michigan", Year = 2011, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 82: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 83: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 84: Lake = "Michigan", Year = 2011, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 86: Lake = "Michigan", Year = 2012, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 88: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 89: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 90: Lake = "Michigan", Year = 2012, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 92: Lake = "Michigan", Year = 2013, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 94: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 95: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 97: Lake = "Michigan", Year = 2014, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 99: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 100: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 101: Lake = "Michigan", Year = 2014, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 103: Lake = "Michigan", Year = 2015, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 105: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 106: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 107: Lake = "Michigan", Year = 2015, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 109: Lake = "Michigan", Year = 2016, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 111: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 112: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 113: Lake = "Michigan", Year = 2016, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 115: Lake = "Michigan", Year = 2017, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 117: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 118: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 119: Lake = "Michigan", Year = 2017, Season = "Fall", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 121: Lake = "Michigan", Year = 2018, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 123: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 124: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 126: Lake = "Michigan", Year = 2019, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 128: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 129: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Removed 8 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 8 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 8 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-1.png)<!-- -->

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 1: Lake = "Michigan", Year = 1995, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 2: Lake = "Michigan", Year = 1995, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 3: Lake = "Michigan", Year = 1996, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 4: Lake = "Michigan", Year = 1996, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 5: Lake = "Michigan", Year = 1998, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 6: Lake = "Michigan", Year = 1998, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 7: Lake = "Michigan", Year = 1999, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 8: Lake = "Michigan", Year = 1999, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 9: Lake = "Michigan", Year = 2000, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 10: Lake = "Michigan", Year = 2000, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 11: Lake = "Michigan", Year = 2002, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 15: Lake = "Michigan", Year = 2007, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 17: Lake = "Michigan", Year = 2007, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 19: Lake = "Michigan", Year = 2008, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 21: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 22: Lake = "Michigan", Year = 2008, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 24: Lake = "Michigan", Year = 2009, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 26: Lake = "Michigan", Year = 2009, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 28: Lake = "Michigan", Year = 2010, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 30: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 31: Lake = "Michigan", Year = 2010, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 33: Lake = "Michigan", Year = 2011, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 35: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 36: Lake = "Michigan", Year = 2011, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 38: Lake = "Michigan", Year = 2012, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 40: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 41: Lake = "Michigan", Year = 2012, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 43: Lake = "Michigan", Year = 2013, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 45: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 46: Lake = "Michigan", Year = 2013, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 48: Lake = "Michigan", Year = 2014, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 50: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 51: Lake = "Michigan", Year = 2014, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 53: Lake = "Michigan", Year = 2015, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 55: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 56: Lake = "Michigan", Year = 2015, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 58: Lake = "Michigan", Year = 2016, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 60: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 61: Lake = "Michigan", Year = 2016, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 63: Lake = "Michigan", Year = 2017, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 65: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 66: Lake = "Michigan", Year = 2017, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 68: Lake = "Michigan", Year = 2018, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 70: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 71: Lake = "Michigan", Year = 2018, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 73: Lake = "Michigan", Year = 2019, Season = "Spring", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 75: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "NOAA".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Problem with `mutate()` input `Group`.
    ## i Unknown levels in `f`: GLNPO_Mys
    ## i Input `Group` is `fct_recode(Group, GLNPO = "GLNPO_Mys")`.
    ## i The error occurred in group 76: Lake = "Michigan", Year = 2019, Season = "Summer", Group = "USGS".

    ## Warning: Unknown levels in `f`: GLNPO_Mys

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).
    
    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-2.png)<!-- -->

    ## Warning: Removed 8 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 8 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 8 rows containing missing values (geom_point).

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-3.png)<!-- -->

    ## # A tibble: 4 x 7
    ##   Lake  N_Years Mean_Length_1 Mean_Length_2 Total_Growth Overall_Rate_pe~
    ##   <chr>   <int>         <dbl>         <dbl>        <dbl>            <dbl>
    ## 1 Huron      14          5.28          12.5         9.54            0.681
    ## 2 Mich~      14          5.45          13.1        10.1             0.724
    ## 3 Onta~      13          6.46          14.4        11.4             0.815
    ## 4 Supe~      13          5.01          12.1         9.11            0.651
    ## # ... with 1 more variable: Measured_Rate_per_month <dbl>

    ## Warning: Removed 4 rows containing non-finite values (stat_bin).

![](GLNPO_Long_term_2019_files/figure-gfm/Plot%20Size%20Structure%20Results-4.png)<!-- -->

    ## # A tibble: 134 x 10
    ##     Visit   Sex      Stage  Count Length Embryos  Year Lake  Season Age    
    ##     <chr>   <chr>    <chr>  <dbl>  <dbl>   <dbl> <dbl> <fct> <fct>  <fct>  
    ##   1 E15MM12 Female   Early      1  15         30  2012 Erie  Spring (14,20]
    ##   2 E15MG13 Juvenile <NA>       1   4.63      NA  2013 Erie  Summer (0,14] 
    ##   3 E063M14 Female   Mid        1  16.5       30  2014 Erie  Spring (14,20]
    ##   4 E15MM14 Female   Early      1  15.3       28  2014 Erie  Spring (14,20]
    ##   5 E15MG07 Juvenile <NA>       1   4.5       NA  2007 Erie  Summer (0,14] 
    ##   6 E15MG07 Juvenile <NA>       1   4         NA  2007 Erie  Summer (0,14] 
    ##   7 E15MG07 Juvenile <NA>       1   4.5       NA  2007 Erie  Summer (0,14] 
    ##   8 E15MG07 Juvenile <NA>       1   5         NA  2007 Erie  Summer (0,14] 
    ##   9 E009G08 Juvenile <NA>       1   4.2       NA  2008 Erie  Summer (0,14] 
    ##  10 E009G08 Juvenile <NA>       1   4         NA  2008 Erie  Summer (0,14] 
    ##  11 E009G08 Juvenile <NA>       1   6.6       NA  2008 Erie  Summer (0,14] 
    ##  12 E009G08 Juvenile <NA>       1   5         NA  2008 Erie  Summer (0,14] 
    ##  13 E009G08 Juvenile <NA>       1   4         NA  2008 Erie  Summer (0,14] 
    ##  14 E009G08 Juvenile <NA>       1   4         NA  2008 Erie  Summer (0,14] 
    ##  15 E009G08 Juvenile <NA>       1   4         NA  2008 Erie  Summer (0,14] 
    ##  16 E15MG08 Female   <NA>       1  15.5       NA  2008 Erie  Summer (14,20]
    ##  17 E15MG08 Juvenile <NA>       1   5         NA  2008 Erie  Summer (0,14] 
    ##  18 E15MG08 Juvenile <NA>       1   4.5       NA  2008 Erie  Summer (0,14] 
    ##  19 E010G10 Juvenile <NA>       1   4.7       NA  2010 Erie  Summer (0,14] 
    ##  20 E93bG10 Juvenile <NA>       1   4         NA  2010 Erie  Summer (0,14] 
    ##  21 E009G15 Juvenile <NA>       1   4.86      NA  2015 Erie  Summer (0,14] 
    ##  22 E009G15 Juvenile <NA>       1   3.70      NA  2015 Erie  Summer (0,14] 
    ##  23 E009G15 Juvenile <NA>       1   5.88      NA  2015 Erie  Summer (0,14] 
    ##  24 E009G15 Juvenile <NA>       1   7.32      NA  2015 Erie  Summer (0,14] 
    ##  25 E009G15 Juvenile <NA>       1   3.68      NA  2015 Erie  Summer (0,14] 
    ##  26 E009G15 Juvenile <NA>       1   4.89      NA  2015 Erie  Summer (0,14] 
    ##  27 E009G15 Juvenile <NA>       1   3.81      NA  2015 Erie  Summer (0,14] 
    ##  28 E009G15 Juvenile <NA>       1   5.11      NA  2015 Erie  Summer (0,14] 
    ##  29 E009G15 Juvenile <NA>       1   3.78      NA  2015 Erie  Summer (0,14] 
    ##  30 E009G15 Juvenile <NA>       1   3.25      NA  2015 Erie  Summer (0,14] 
    ##  31 E009G15 Juvenile <NA>       1   4.75      NA  2015 Erie  Summer (0,14] 
    ##  32 E009G15 Juvenile <NA>       1   4.51      NA  2015 Erie  Summer (0,14] 
    ##  33 E009G15 Juvenile <NA>       1   3.93      NA  2015 Erie  Summer (0,14] 
    ##  34 E009G15 Juvenile <NA>       1   4.10      NA  2015 Erie  Summer (0,14] 
    ##  35 E009G15 Juvenile <NA>       1   5.22      NA  2015 Erie  Summer (0,14] 
    ##  36 E009G15 Juvenile <NA>       1   4.26      NA  2015 Erie  Summer (0,14] 
    ##  37 E009G15 Juvenile <NA>       1   3.84      NA  2015 Erie  Summer (0,14] 
    ##  38 E009M18 Female   Early      1  15.3       22  2018 Erie  Spring (14,20]
    ##  39 E009M18 Male     Mature     1  12.6       NA  2018 Erie  Spring (0,14] 
    ##  40 E009M18 Female   Early      1  15.6       17  2018 Erie  Spring (14,20]
    ##  41 E009M18 Male     Mature     1  13.3       NA  2018 Erie  Spring (0,14] 
    ##  42 E009G18 Juvenile <NA>       1   3.19      NA  2018 Erie  Summer (0,14] 
    ##  43 E009G18 Juvenile <NA>       1   3.60      NA  2018 Erie  Summer (0,14] 
    ##  44 E009G18 Juvenile <NA>       1   3.26      NA  2018 Erie  Summer (0,14] 
    ##  45 E009G18 Juvenile <NA>       1   4.27      NA  2018 Erie  Summer (0,14] 
    ##  46 E009G18 Juvenile <NA>       1   3.40      NA  2018 Erie  Summer (0,14] 
    ##  47 E009G18 Juvenile <NA>       1   3.02      NA  2018 Erie  Summer (0,14] 
    ##  48 E009G18 Juvenile <NA>       1   2.87      NA  2018 Erie  Summer (0,14] 
    ##  49 E009G18 Juvenile <NA>       1   4.66      NA  2018 Erie  Summer (0,14] 
    ##  50 E009G18 Juvenile <NA>       1   3.37      NA  2018 Erie  Summer (0,14] 
    ##  51 E009G18 Juvenile <NA>       1   3.64      NA  2018 Erie  Summer (0,14] 
    ##  52 E009G18 Juvenile <NA>       1   3.90      NA  2018 Erie  Summer (0,14] 
    ##  53 E009G18 Juvenile <NA>       1   3.84      NA  2018 Erie  Summer (0,14] 
    ##  54 E009G18 Juvenile <NA>       1   4.65      NA  2018 Erie  Summer (0,14] 
    ##  55 E009G18 Juvenile <NA>       1   3.52      NA  2018 Erie  Summer (0,14] 
    ##  56 E009G18 Juvenile <NA>       1   3.69      NA  2018 Erie  Summer (0,14] 
    ##  57 E009G18 Juvenile <NA>       1   3.17      NA  2018 Erie  Summer (0,14] 
    ##  58 E009G18 Juvenile <NA>       1   4.12      NA  2018 Erie  Summer (0,14] 
    ##  59 E009G18 Juvenile <NA>       1   3.51      NA  2018 Erie  Summer (0,14] 
    ##  60 E009G18 Juvenile <NA>       1   3.38      NA  2018 Erie  Summer (0,14] 
    ##  61 E009G18 Juvenile <NA>       1   2.90      NA  2018 Erie  Summer (0,14] 
    ##  62 E009G18 Juvenile <NA>       1   4.79      NA  2018 Erie  Summer (0,14] 
    ##  63 E009G18 Juvenile <NA>       1   3.55      NA  2018 Erie  Summer (0,14] 
    ##  64 E009G18 Juvenile <NA>       1   7.91      NA  2018 Erie  Summer (0,14] 
    ##  65 E009G18 Juvenile <NA>       1   3.69      NA  2018 Erie  Summer (0,14] 
    ##  66 E009G18 Juvenile <NA>       1   4.06      NA  2018 Erie  Summer (0,14] 
    ##  67 E009G18 Juvenile <NA>       1   4.68      NA  2018 Erie  Summer (0,14] 
    ##  68 E009G18 Juvenile <NA>       1   3.75      NA  2018 Erie  Summer (0,14] 
    ##  69 E009G18 Juvenile <NA>       1   3.75      NA  2018 Erie  Summer (0,14] 
    ##  70 E009G18 Juvenile <NA>       1   4.19      NA  2018 Erie  Summer (0,14] 
    ##  71 E009G18 Juvenile <NA>       1   3.78      NA  2018 Erie  Summer (0,14] 
    ##  72 E009G18 Juvenile <NA>       1   3.96      NA  2018 Erie  Summer (0,14] 
    ##  73 E15MG18 Juvenile <NA>       1   3.97      NA  2018 Erie  Summer (0,14] 
    ##  74 E15MG18 Juvenile <NA>       1   3.39      NA  2018 Erie  Summer (0,14] 
    ##  75 E15MG18 Juvenile <NA>       1   2.82      NA  2018 Erie  Summer (0,14] 
    ##  76 E15MG18 Juvenile <NA>       1   2.87      NA  2018 Erie  Summer (0,14] 
    ##  77 E15MG18 Juvenile <NA>       1   3.26      NA  2018 Erie  Summer (0,14] 
    ##  78 E15MG18 Juvenile <NA>       1   2.65      NA  2018 Erie  Summer (0,14] 
    ##  79 E15MG18 Juvenile <NA>       1   5.58      NA  2018 Erie  Summer (0,14] 
    ##  80 E15MG18 Juvenile <NA>       1   3.65      NA  2018 Erie  Summer (0,14] 
    ##  81 E15MG18 Juvenile <NA>       1   3.16      NA  2018 Erie  Summer (0,14] 
    ##  82 E15MG18 Juvenile <NA>       1   3.19      NA  2018 Erie  Summer (0,14] 
    ##  83 E15MG18 Juvenile <NA>       1   3.03      NA  2018 Erie  Summer (0,14] 
    ##  84 E15MG18 Juvenile <NA>       1   3.34      NA  2018 Erie  Summer (0,14] 
    ##  85 E15MG18 Juvenile <NA>       1   3.68      NA  2018 Erie  Summer (0,14] 
    ##  86 E15MG18 Juvenile <NA>       1   3.66      NA  2018 Erie  Summer (0,14] 
    ##  87 E15MG18 Juvenile <NA>       1   3.76      NA  2018 Erie  Summer (0,14] 
    ##  88 E15MG18 Juvenile <NA>       1   3.64      NA  2018 Erie  Summer (0,14] 
    ##  89 E15MG18 Juvenile <NA>       1   3.18      NA  2018 Erie  Summer (0,14] 
    ##  90 E15MG18 Juvenile <NA>       1   3.52      NA  2018 Erie  Summer (0,14] 
    ##  91 E15MG18 Juvenile <NA>       1   2.91      NA  2018 Erie  Summer (0,14] 
    ##  92 E15MG18 Juvenile <NA>       1   3.26      NA  2018 Erie  Summer (0,14] 
    ##  93 E15MG18 Juvenile <NA>       1   2.87      NA  2018 Erie  Summer (0,14] 
    ##  94 E15MG18 Juvenile <NA>       1   3.26      NA  2018 Erie  Summer (0,14] 
    ##  95 E15MG18 Juvenile <NA>       1   3.04      NA  2018 Erie  Summer (0,14] 
    ##  96 E063G18 Juvenile <NA>       1   4.89      NA  2018 Erie  Summer (0,14] 
    ##  97 E063G18 Juvenile <NA>       1   3.41      NA  2018 Erie  Summer (0,14] 
    ##  98 E063G18 Juvenile <NA>       1   3.66      NA  2018 Erie  Summer (0,14] 
    ##  99 E063G18 Juvenile <NA>       1   3.4       NA  2018 Erie  Summer (0,14] 
    ## 100 E063G18 Juvenile <NA>       1   3.84      NA  2018 Erie  Summer (0,14] 
    ## 101 E063G18 Juvenile <NA>       1   3.79      NA  2018 Erie  Summer (0,14] 
    ## 102 E063G18 Juvenile <NA>       1   3.33      NA  2018 Erie  Summer (0,14] 
    ## 103 E063G18 Juvenile <NA>       1   5.13      NA  2018 Erie  Summer (0,14] 
    ## 104 E063G18 Juvenile <NA>       1   3.31      NA  2018 Erie  Summer (0,14] 
    ## 105 E063G18 Juvenile <NA>       1   4.33      NA  2018 Erie  Summer (0,14] 
    ## 106 E063G18 Juvenile <NA>       1   3.44      NA  2018 Erie  Summer (0,14] 
    ## 107 E063G18 Juvenile <NA>       1   6.90      NA  2018 Erie  Summer (0,14] 
    ## 108 E009G19 Juvenile <NA>       1   3.31      NA  2019 Erie  Summer (0,14] 
    ## 109 E009G19 Juvenile <NA>       1   3.19      NA  2019 Erie  Summer (0,14] 
    ## 110 E009G19 Juvenile <NA>       1   3.58      NA  2019 Erie  Summer (0,14] 
    ## 111 E009G19 Juvenile <NA>       1   3.99      NA  2019 Erie  Summer (0,14] 
    ## 112 E009G19 Juvenile <NA>       1   3.79      NA  2019 Erie  Summer (0,14] 
    ## 113 E009G19 Juvenile <NA>       1   2.65      NA  2019 Erie  Summer (0,14] 
    ## 114 E009G19 Juvenile <NA>       1   3.64      NA  2019 Erie  Summer (0,14] 
    ## 115 E009G19 Juvenile <NA>       1   3.61      NA  2019 Erie  Summer (0,14] 
    ## 116 E009G19 Juvenile <NA>       1   3.46      NA  2019 Erie  Summer (0,14] 
    ## 117 E009G19 Juvenile <NA>       1   2.37      NA  2019 Erie  Summer (0,14] 
    ## 118 E009G19 Juvenile <NA>       1   4.37      NA  2019 Erie  Summer (0,14] 
    ## 119 E009G19 Juvenile <NA>       1   3.35      NA  2019 Erie  Summer (0,14] 
    ## 120 E009G19 Juvenile <NA>       1   3.36      NA  2019 Erie  Summer (0,14] 
    ## 121 E009G19 Juvenile <NA>       1   3.28      NA  2019 Erie  Summer (0,14] 
    ## 122 E009G19 Juvenile <NA>       1   3.65      NA  2019 Erie  Summer (0,14] 
    ## 123 E009G19 Juvenile <NA>       1   3.82      NA  2019 Erie  Summer (0,14] 
    ## 124 E009G19 Juvenile <NA>       1   3.75      NA  2019 Erie  Summer (0,14] 
    ## 125 E009G19 Juvenile <NA>       1   3.23      NA  2019 Erie  Summer (0,14] 
    ## 126 E009G19 Juvenile <NA>       1   3.45      NA  2019 Erie  Summer (0,14] 
    ## 127 E009G19 Juvenile <NA>       1   3.52      NA  2019 Erie  Summer (0,14] 
    ## 128 E009G19 Juvenile <NA>       1   3.79      NA  2019 Erie  Summer (0,14] 
    ## 129 E93bG19 Juvenile <NA>       1   4.39      NA  2019 Erie  Summer (0,14] 
    ## 130 E93bG19 Juvenile <NA>       1   2.98      NA  2019 Erie  Summer (0,14] 
    ## 131 E93bG19 Juvenile <NA>       1   2.82      NA  2019 Erie  Summer (0,14] 
    ## 132 E93bG19 Juvenile <NA>       1   3.66      NA  2019 Erie  Summer (0,14] 
    ## 133 E93bG19 Juvenile <NA>       1   3.04      NA  2019 Erie  Summer (0,14] 
    ## 134 E010A19 Male     Mature     1  12.5       NA  2019 Erie  Spring (0,14]

    ## # A tibble: 4 x 7
    ##   Season Age    Mean_Length Sd_Length N_Length Rate_per_month_3 Rate_per_month_~
    ##   <fct>  <fct>        <dbl>     <dbl>    <int>            <dbl>            <dbl>
    ## 1 Spring (0,14]       12.8      0.444        3             1.23             1.12
    ## 2 Spring (14,2~       15.5      0.566        5             1.57             1.46
    ## 3 Summer (0,14]        3.89     0.890      125            NA               NA   
    ## 4 Summer (14,2~       15.5     NA            1            NA               NA

    ## [1] 99.1

<br> <br>

-----

### Survival estimates

### Table of life history values between lakes

# End of Script.

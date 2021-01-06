Size Structure Fit Plots Markdown File
================

``` r
Mysids_Lengths_plot <- read_csv("Lengths_Data_Rmd_Output.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Visit = col_character(),
    ##   Length = col_double(),
    ##   Lake = col_character(),
    ##   Year = col_double(),
    ##   Season = col_character(),
    ##   Group = col_character(),
    ##   VisPool = col_character()
    ## )

``` r
Mysid_Mclust_Fits_plot <- 
  
  Mysids_Lengths_plot %>% 
  
  mutate(VisPool = str_replace(VisPool, pattern = "Late Summer", replacement = "Late_Summer")) %>% 

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
```

``` r
MaxLen = ceiling(max(Mysids_Lengths_plot$Length))

PlotFits <- function(List){
  
  par(mfrow = c(2,1), mar = c(3, 2, 1, 1) + 0.1)
  
  for(i in seq_along(List)){
    
    Obj = NULL
    Name = NULL
    
    Obj = List[[i]]
    Name = names(List)[i]
    
    if(class(Obj) == "Mclust"){
      
      print(paste0("Mclust fit of ", Name, ":"))
      
      if(Obj$G == 1){
        
        Obj$data %>% 
          hist(breaks = seq(0, MaxLen, 0.5), right = FALSE, col = 2,
               main = Name)
        
      } else {
        
        Obj$data %>% 
          hist(breaks = seq(0, MaxLen, 0.5), right = FALSE, col = 4,
               main = Name)
      
        Obj$data %>% 
          split(Obj$classification) %>% 
          .$`1` %>% 
          hist(breaks = seq(0, MaxLen, 0.5), right = FALSE, add = T, col = 2)
        
      }
      
      plot(Obj, what = "density", xlim = c(0, MaxLen))
      
      
    } else {
      
      print(paste0("Raw data for ", Name))
      
      hist(Obj, breaks = seq(0, MaxLen, 0.5), right = FALSE, 
           main = Name)
      
      hist(Obj, breaks = seq(0, MaxLen, 0.5), right = FALSE, freq = FALSE)
      
    }
    
  }
  
}
```

# Summer Fits

## Mysid nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    !grepl("Late_Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("GLNPO_Mysid", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

## Zooplankton net:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("GLNPO_Zoop", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Raw data for Erie_2001_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Erie_2003_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Erie_2004_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-3.png)<!-- -->

    ## [1] "Raw data for Erie_2005_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Erie_2008_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-5.png)<!-- -->

    ## [1] "Raw data for Erie_2010_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-6.png)<!-- -->

    ## [1] "Raw data for Erie_2015_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-7.png)<!-- -->

    ## [1] "Raw data for Erie_2018_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Huron_1997_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-9.png)<!-- -->

    ## [1] "Raw data for Huron_1998_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Huron_2000_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Huron_2001_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Huron_2002_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-13.png)<!-- -->

    ## [1] "Mclust fit of Huron_2003_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-14.png)<!-- -->

    ## [1] "Mclust fit of Huron_2004_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-15.png)<!-- -->

    ## [1] "Mclust fit of Huron_2005_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-16.png)<!-- -->

    ## [1] "Mclust fit of Huron_2006_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-17.png)<!-- -->

    ## [1] "Mclust fit of Huron_2007_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-18.png)<!-- -->

    ## [1] "Mclust fit of Huron_2008_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-19.png)<!-- -->

    ## [1] "Mclust fit of Huron_2009_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-20.png)<!-- -->

    ## [1] "Mclust fit of Huron_2010_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-21.png)<!-- -->

    ## [1] "Mclust fit of Huron_2011_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-22.png)<!-- -->

    ## [1] "Mclust fit of Huron_2012_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-23.png)<!-- -->

    ## [1] "Raw data for Huron_2013_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-24.png)<!-- -->

    ## [1] "Mclust fit of Huron_2014_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-25.png)<!-- -->

    ## [1] "Mclust fit of Huron_2015_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-26.png)<!-- -->

    ## [1] "Mclust fit of Huron_2016_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-27.png)<!-- -->

    ## [1] "Raw data for Huron_2017_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-28.png)<!-- -->

    ## [1] "Raw data for Huron_2018_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-29.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1997_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-30.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1998_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-31.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1999_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-32.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2000_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-33.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2001_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-34.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2002_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-35.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2003_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-36.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2004_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-37.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2005_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-38.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2006_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-39.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-40.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-41.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-42.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-43.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-44.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-45.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-46.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-47.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-48.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-49.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-50.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-51.png)<!-- -->

    ## [1] "Raw data for Ontario_1998_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-52.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2000_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-53.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2001_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-54.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2002_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-55.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2003_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-56.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2004_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-57.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2005_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-58.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2006_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-59.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2007_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-60.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2008_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-61.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2009_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-62.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2010_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-63.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2011_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-64.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2012_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-65.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2013_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-66.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2014_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-67.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2015_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-68.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2016_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-69.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2017_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-70.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2018_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-71.png)<!-- -->

    ## [1] "Mclust fit of Superior_1997_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-72.png)<!-- -->

    ## [1] "Mclust fit of Superior_1998_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-73.png)<!-- -->

    ## [1] "Raw data for Superior_1999_Summer_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-74.png)<!-- -->

    ## [1] "Mclust fit of Superior_2000_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-75.png)<!-- -->

    ## [1] "Mclust fit of Superior_2001_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-76.png)<!-- -->

    ## [1] "Mclust fit of Superior_2002_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-77.png)<!-- -->

    ## [1] "Mclust fit of Superior_2003_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-78.png)<!-- -->

    ## [1] "Mclust fit of Superior_2004_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-79.png)<!-- -->

    ## [1] "Mclust fit of Superior_2005_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-80.png)<!-- -->

    ## [1] "Mclust fit of Superior_2006_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-81.png)<!-- -->

    ## [1] "Mclust fit of Superior_2007_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-82.png)<!-- -->

    ## [1] "Mclust fit of Superior_2008_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-83.png)<!-- -->

    ## [1] "Mclust fit of Superior_2009_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-84.png)<!-- -->

    ## [1] "Mclust fit of Superior_2010_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-85.png)<!-- -->

    ## [1] "Mclust fit of Superior_2011_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-86.png)<!-- -->

    ## [1] "Mclust fit of Superior_2012_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-87.png)<!-- -->

    ## [1] "Mclust fit of Superior_2013_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-88.png)<!-- -->

    ## [1] "Mclust fit of Superior_2014_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-89.png)<!-- -->

    ## [1] "Mclust fit of Superior_2015_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-90.png)<!-- -->

    ## [1] "Mclust fit of Superior_2016_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-91.png)<!-- -->

    ## [1] "Mclust fit of Superior_2017_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-92.png)<!-- -->

    ## [1] "Mclust fit of Superior_2018_Summer_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20zooplankton%20fits-93.png)<!-- -->

## NOAA nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    !grepl("Late_Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("NOAA", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Mclust fit of Michigan_1995_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1996_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1998_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1999_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2000_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2002_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-13.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-14.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-15.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-16.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-17.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-18.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Summer_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20NOAA%20mysid%20fits-19.png)<!-- -->

## USGS nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    !grepl("Late_Summer", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("USGS", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Mclust fit of Michigan_2008_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20summer%20USGS%20mysid%20fits-11.png)<!-- -->

# Spring Fits

## Mysid nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Spring", names(Mysid_Mclust_Fits_plot)) & 
    
    !grepl("GLNPO_Zoop", names(Mysid_Mclust_Fits_plot))
  
  ] %>% 
  
  PlotFits
```

    ## [1] "Raw data for Erie_2012_Spring_GLNPO_Mys"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-1.png)<!-- -->

    ## [1] "Raw data for Erie_2014_Spring_GLNPO_Mys"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-2.png)<!-- -->

    ## [1] "Raw data for Erie_2018_Spring_GLNPO_Mys"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-3.png)<!-- -->

    ## [1] "Raw data for Erie_2019_Spring_GLNPO_Mys"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Huron_2006_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Huron_2007_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Huron_2008_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Huron_2009_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Huron_2010_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Huron_2011_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Huron_2012_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Huron_2013_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Huron_2014_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-13.png)<!-- -->

    ## [1] "Mclust fit of Huron_2015_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-14.png)<!-- -->

    ## [1] "Mclust fit of Huron_2016_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-15.png)<!-- -->

    ## [1] "Mclust fit of Huron_2017_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-16.png)<!-- -->

    ## [1] "Mclust fit of Huron_2018_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-17.png)<!-- -->

    ## [1] "Mclust fit of Huron_2019_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-18.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1995_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-19.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1996_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-20.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1998_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-21.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1999_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-22.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2000_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-23.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2002_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-24.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2006_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-25.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-26.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-27.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-28.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-29.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-30.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-31.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-32.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-33.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-34.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-35.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-36.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-37.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-38.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-39.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-40.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-41.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-42.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-43.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-44.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-45.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-46.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-47.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-48.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-49.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-50.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-51.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2006_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-52.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2007_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-53.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2008_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-54.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2009_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-55.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2010_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-56.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2011_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-57.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2012_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-58.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2013_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-59.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2014_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-60.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2015_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-61.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2016_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-62.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2017_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-63.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2018_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-64.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2019_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-65.png)<!-- -->

    ## [1] "Mclust fit of Superior_2007_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-66.png)<!-- -->

    ## [1] "Mclust fit of Superior_2008_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-67.png)<!-- -->

    ## [1] "Mclust fit of Superior_2009_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-68.png)<!-- -->

    ## [1] "Mclust fit of Superior_2010_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-69.png)<!-- -->

    ## [1] "Mclust fit of Superior_2011_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-70.png)<!-- -->

    ## [1] "Mclust fit of Superior_2012_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-71.png)<!-- -->

    ## [1] "Mclust fit of Superior_2013_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-72.png)<!-- -->

    ## [1] "Mclust fit of Superior_2014_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-73.png)<!-- -->

    ## [1] "Mclust fit of Superior_2015_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-74.png)<!-- -->

    ## [1] "Mclust fit of Superior_2016_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-75.png)<!-- -->

    ## [1] "Mclust fit of Superior_2017_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-76.png)<!-- -->

    ## [1] "Mclust fit of Superior_2018_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-77.png)<!-- -->

    ## [1] "Mclust fit of Superior_2019_Spring_GLNPO_Mys:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20mysid%20fits-78.png)<!-- -->

## Zooplankton net:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Spring", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("GLNPO_Zoop", names(Mysid_Mclust_Fits_plot))
  
  ] %>% 
  
  PlotFits
```

    ## [1] "Raw data for Huron_1998_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-1.png)<!-- -->

    ## [1] "Raw data for Huron_1999_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Huron_2001_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Huron_2002_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Huron_2003_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Huron_2004_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Huron_2005_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Huron_2006_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Huron_2007_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Huron_2008_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Huron_2009_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-11.png)<!-- -->

    ## [1] "Raw data for Huron_2010_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Huron_2011_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-13.png)<!-- -->

    ## [1] "Raw data for Huron_2012_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-14.png)<!-- -->

    ## [1] "Raw data for Huron_2013_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-15.png)<!-- -->

    ## [1] "Raw data for Huron_2014_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-16.png)<!-- -->

    ## [1] "Raw data for Huron_2015_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-17.png)<!-- -->

    ## [1] "Raw data for Huron_2016_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-18.png)<!-- -->

    ## [1] "Mclust fit of Huron_2017_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-19.png)<!-- -->

    ## [1] "Raw data for Huron_2018_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-20.png)<!-- -->

    ## [1] "Raw data for Michigan_1998_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-21.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2001_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-22.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2002_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-23.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2003_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-24.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2004_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-25.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2005_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-26.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2006_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-27.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-28.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-29.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-30.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-31.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-32.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-33.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-34.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-35.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-36.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-37.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-38.png)<!-- -->

    ## [1] "Raw data for Michigan_2018_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-39.png)<!-- -->

    ## [1] "Raw data for Ontario_1998_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-40.png)<!-- -->

    ## [1] "Raw data for Ontario_1999_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-41.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2001_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-42.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2002_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-43.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2003_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-44.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2004_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-45.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2005_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-46.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2006_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-47.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2007_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-48.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2008_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-49.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2009_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-50.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2010_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-51.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2011_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-52.png)<!-- -->

    ## [1] "Raw data for Ontario_2012_Spring_GLNPO_Zoop"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-53.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2013_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-54.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2014_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-55.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2015_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-56.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2016_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-57.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2017_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-58.png)<!-- -->

    ## [1] "Mclust fit of Ontario_2018_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-59.png)<!-- -->

    ## [1] "Mclust fit of Superior_1998_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-60.png)<!-- -->

    ## [1] "Mclust fit of Superior_1999_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-61.png)<!-- -->

    ## [1] "Mclust fit of Superior_2001_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-62.png)<!-- -->

    ## [1] "Mclust fit of Superior_2002_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-63.png)<!-- -->

    ## [1] "Mclust fit of Superior_2003_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-64.png)<!-- -->

    ## [1] "Mclust fit of Superior_2004_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-65.png)<!-- -->

    ## [1] "Mclust fit of Superior_2005_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-66.png)<!-- -->

    ## [1] "Mclust fit of Superior_2006_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-67.png)<!-- -->

    ## [1] "Mclust fit of Superior_2007_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-68.png)<!-- -->

    ## [1] "Mclust fit of Superior_2008_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-69.png)<!-- -->

    ## [1] "Mclust fit of Superior_2009_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-70.png)<!-- -->

    ## [1] "Mclust fit of Superior_2010_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-71.png)<!-- -->

    ## [1] "Mclust fit of Superior_2011_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-72.png)<!-- -->

    ## [1] "Mclust fit of Superior_2012_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-73.png)<!-- -->

    ## [1] "Mclust fit of Superior_2013_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-74.png)<!-- -->

    ## [1] "Mclust fit of Superior_2014_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-75.png)<!-- -->

    ## [1] "Mclust fit of Superior_2015_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-76.png)<!-- -->

    ## [1] "Mclust fit of Superior_2016_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-77.png)<!-- -->

    ## [1] "Mclust fit of Superior_2017_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-78.png)<!-- -->

    ## [1] "Mclust fit of Superior_2018_Spring_GLNPO_Zoop:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20zooplankton%20fits-79.png)<!-- -->

## NOAA nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Spring", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("NOAA", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Mclust fit of Michigan_1995_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1996_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1998_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1999_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2000_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2002_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-13.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-14.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-15.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-16.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-17.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-18.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Spring_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20spring%20NOAA%20mysid%20fits-19.png)<!-- -->

# Late Summer / Fall Fits

## NOAA nets:

``` r
Mysid_Mclust_Fits_plot[
  
  grepl("Fall", names(Mysid_Mclust_Fits_plot)) & 
    
    grepl("NOAA", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Mclust fit of Michigan_1996_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1998_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Michigan_1999_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2000_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2007_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2008_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2009_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2010_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2011_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2012_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2013_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2014_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2015_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-13.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2016_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-14.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2017_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-15.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2018_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-16.png)<!-- -->

    ## [1] "Mclust fit of Michigan_2019_Fall_NOAA:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20Fall%20NOAA%20mysid%20fits-17.png)<!-- -->

## USGS nets:

``` r
Mysid_Mclust_Fits_plot[
  
  (grepl("Late_Summer", names(Mysid_Mclust_Fits_plot)) | 
    
    grepl("Fall", names(Mysid_Mclust_Fits_plot))) & 
    
    grepl("USGS", names(Mysid_Mclust_Fits_plot))
  
  ] %>%
  
  PlotFits
```

    ## [1] "Mclust fit of Huron_2006_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-1.png)<!-- -->

    ## [1] "Mclust fit of Huron_2008_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-2.png)<!-- -->

    ## [1] "Mclust fit of Huron_2009_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-3.png)<!-- -->

    ## [1] "Mclust fit of Huron_2010_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-4.png)<!-- -->

    ## [1] "Mclust fit of Huron_2011_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-5.png)<!-- -->

    ## [1] "Mclust fit of Huron_2012_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-6.png)<!-- -->

    ## [1] "Mclust fit of Huron_2013_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-7.png)<!-- -->

    ## [1] "Mclust fit of Huron_2014_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-8.png)<!-- -->

    ## [1] "Mclust fit of Huron_2015_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-9.png)<!-- -->

    ## [1] "Mclust fit of Huron_2016_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-10.png)<!-- -->

    ## [1] "Mclust fit of Huron_2017_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-11.png)<!-- -->

    ## [1] "Mclust fit of Huron_2018_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-12.png)<!-- -->

    ## [1] "Mclust fit of Huron_2019_Late_Summer_USGS:"

![](Size_Structure_Fit_Diagnose_files/figure-gfm/Plot%20late%20summer%20or%20fall%20fits-13.png)<!-- -->

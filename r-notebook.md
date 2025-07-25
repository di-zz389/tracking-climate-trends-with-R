R Notebook for Climate Change Trends
================

Loading the libraries requied to analyze the datasets

``` r
#loading the required packages
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(corrplot)
```

    ## corrplot 0.95 loaded

``` r
library(VIM)
```

    ## Loading required package: colorspace
    ## Loading required package: grid
    ## VIM is ready to use.
    ## 
    ## Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues
    ## 
    ## Attaching package: 'VIM'
    ## 
    ## The following object is masked from 'package:datasets':
    ## 
    ##     sleep

``` r
library(Hmisc)
```

    ## 
    ## Attaching package: 'Hmisc'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following object is masked from 'package:Hmisc':
    ## 
    ##     describe
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(ggplot2)
library(dplyr)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:Hmisc':
    ## 
    ##     subplot
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(GGally)
library(mice)
```

    ## 
    ## Attaching package: 'mice'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

``` r
library(readr)
library(patchwork)
library(ggcorrplot)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(RColorBrewer)
```

Loading the datasets

``` r
#loading datasets
df1 <- read_csv("global-temp-annual.csv")
```

    ## Rows: 136 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): Year, Land, Land and Ocean, N Hem, S Hem, Band 1, Band 2, Band 3
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df2 <- read_csv("owid-co2-data.csv")
```

    ## Rows: 50191 Columns: 79
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): country, iso_code
    ## dbl (77): year, population, gdp, cement_co2, cement_co2_per_capita, co2, co2...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Summary of dataset

``` r
#summary of datasets
cat("Dataset 1 Dimensions:", dim(df1), "\n")
```

    ## Dataset 1 Dimensions: 136 8

``` r
cat("Dataset 2 Dimensions:", dim(df2), "\n")
```

    ## Dataset 2 Dimensions: 50191 79

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
cat("Summary of Dataset 1: \n")
```

    ## Summary of Dataset 1:

``` r
summary(df1)
```

    ##       Year           Land          Land and Ocean         N Hem        
    ##  Min.   :1880   Min.   :-0.64000   Min.   :-0.47000   Min.   :-0.5200  
    ##  1st Qu.:1914   1st Qu.:-0.28250   1st Qu.:-0.21000   1st Qu.:-0.2125  
    ##  Median :1948   Median :-0.08000   Median :-0.07500   Median :-0.0150  
    ##  Mean   :1948   Mean   : 0.01243   Mean   : 0.01809   Mean   : 0.0375  
    ##  3rd Qu.:1981   3rd Qu.: 0.22000   3rd Qu.: 0.18250   3rd Qu.: 0.1600  
    ##  Max.   :2015   Max.   : 0.99000   Max.   : 0.86000   Max.   : 1.1200  
    ##                                                                        
    ##      S Hem               Band 1            Band 2             Band 3          
    ##  Min.   :-0.480000   Min.   :-0.5700   Min.   :-0.61000   Min.   :-0.4800000  
    ##  1st Qu.:-0.230000   1st Qu.:-0.1400   1st Qu.:-0.21250   1st Qu.:-0.2625000  
    ##  Median :-0.085000   Median : 0.0700   Median : 0.01500   Median :-0.0750000  
    ##  Mean   :-0.001765   Mean   : 0.1279   Mean   : 0.05216   Mean   :-0.0000862  
    ##  3rd Qu.: 0.252500   3rd Qu.: 0.3425   3rd Qu.: 0.29250   3rd Qu.: 0.2525000  
    ##  Max.   : 0.600000   Max.   : 1.2600   Max.   : 0.91000   Max.   : 0.5800000  
    ##                      NA's   :20        NA's   :20         NA's   :20

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
cat("Summary of Dataset 2: \n")
```

    ## Summary of Dataset 2:

``` r
summary(df2)
```

    ##    country               year        iso_code           population       
    ##  Length:50191       Min.   :1750   Length:50191       Min.   :2.150e+02  
    ##  Class :character   1st Qu.:1875   Class :character   1st Qu.:3.273e+05  
    ##  Mode  :character   Median :1924   Mode  :character   Median :2.290e+06  
    ##                     Mean   :1920                      Mean   :5.686e+07  
    ##                     3rd Qu.:1974                      3rd Qu.:9.862e+06  
    ##                     Max.   :2023                      Max.   :8.092e+09  
    ##                                                       NA's   :9172       
    ##       gdp              cement_co2       cement_co2_per_capita
    ##  Min.   :4.998e+07   Min.   :   0.000   Min.   :0.0000       
    ##  1st Qu.:7.874e+09   1st Qu.:   0.000   1st Qu.:0.0000       
    ##  Median :2.744e+10   Median :   0.000   Median :0.0010       
    ##  Mean   :3.300e+11   Mean   :   7.768   Mean   :0.0590       
    ##  3rd Qu.:1.213e+11   3rd Qu.:   0.486   3rd Qu.:0.0757       
    ##  Max.   :1.301e+14   Max.   :1696.308   Max.   :2.4840       
    ##  NA's   :34940       NA's   :21328      NA's   :24833        
    ##       co2            co2_growth_abs      co2_growth_prct     
    ##  Min.   :    0.000   Min.   :-1977.750   Min.   :  -100.000  
    ##  1st Qu.:    0.374   1st Qu.:   -0.005   1st Qu.:    -1.102  
    ##  Median :    4.990   Median :    0.044   Median :     3.804  
    ##  Mean   :  415.698   Mean   :    6.209   Mean   :    43.104  
    ##  3rd Qu.:   53.273   3rd Qu.:    1.002   3rd Qu.:    10.891  
    ##  Max.   :37791.570   Max.   : 1865.208   Max.   :180870.000  
    ##  NA's   :21054       NA's   :23210       NA's   :24189       
    ##  co2_including_luc   co2_including_luc_growth_abs co2_including_luc_growth_prct
    ##  Min.   :  -99.693   Min.   :-2325.500            Min.   :-16151.107           
    ##  1st Qu.:    6.418   1st Qu.:   -0.908            1st Qu.:    -4.412           
    ##  Median :   27.691   Median :    0.078            Median :     0.976           
    ##  Mean   :  535.581   Mean   :    7.215            Mean   :     3.539           
    ##  3rd Qu.:  123.959   3rd Qu.:    2.620            3rd Qu.:     6.297           
    ##  Max.   :41416.480   Max.   : 2340.184            Max.   : 16646.543           
    ##  NA's   :26606       NA's   :26906                NA's   :26906                
    ##  co2_including_luc_per_capita co2_including_luc_per_gdp
    ##  Min.   :-13.907              Min.   : -6.469          
    ##  1st Qu.:  1.974              1st Qu.:  0.392          
    ##  Median :  4.501              Median :  0.869          
    ##  Mean   :  7.223              Mean   :  2.404          
    ##  3rd Qu.:  9.296              3rd Qu.:  2.104          
    ##  Max.   :364.706              Max.   :291.947          
    ##  NA's   :26696                NA's   :33401            
    ##  co2_including_luc_per_unit_energy co2_per_capita     co2_per_gdp    
    ##  Min.   : -8.476                   Min.   :  0.000   Min.   : 0.000  
    ##  1st Qu.:  0.200                   1st Qu.:  0.169   1st Qu.: 0.131  
    ##  Median :  0.277                   Median :  1.013   Median : 0.264  
    ##  Mean   :  1.074                   Mean   :  3.815   Mean   : 0.398  
    ##  3rd Qu.:  0.707                   3rd Qu.:  4.297   3rd Qu.: 0.508  
    ##  Max.   :351.719                   Max.   :782.682   Max.   :82.576  
    ##  NA's   :40490                     NA's   :24009     NA's   :32663   
    ##  co2_per_unit_energy    coal_co2         coal_co2_per_capita consumption_co2   
    ##  Min.   : 0.000      Min.   :    0.000   Min.   : 0.000      Min.   :    0.00  
    ##  1st Qu.: 0.175      1st Qu.:    0.066   1st Qu.: 0.017      1st Qu.:   11.60  
    ##  Median : 0.216      Median :    2.088   Median : 0.224      Median :   71.39  
    ##  Mean   : 0.240      Mean   :  184.120   Mean   : 1.290      Mean   : 1319.19  
    ##  3rd Qu.: 0.256      3rd Qu.:   27.870   3rd Qu.: 1.595      3rd Qu.:  447.41  
    ##  Max.   :10.686      Max.   :15401.220   Max.   :34.260      Max.   :37791.57  
    ##  NA's   :39841       NA's   :28436       NA's   :29141       NA's   :45325     
    ##  consumption_co2_per_capita consumption_co2_per_gdp cumulative_cement_co2
    ##  Min.   : 0.000             Min.   :0.000           Min.   :    0.00     
    ##  1st Qu.: 1.136             1st Qu.:0.206           1st Qu.:    0.00     
    ##  Median : 4.158             Median :0.299           Median :    0.00     
    ##  Mean   : 6.401             Mean   :0.350           Mean   :  188.43     
    ##  3rd Qu.: 9.764             3rd Qu.:0.423           3rd Qu.:    8.94     
    ##  Max.   :63.465             Max.   :3.746           Max.   :48366.29     
    ##  NA's   :45689              NA's   :45747           NA's   :21350        
    ##  cumulative_co2      cumulative_co2_including_luc cumulative_coal_co2
    ##  Min.   :0.000e+00   Min.   :   -532.5            Min.   :     0.00  
    ##  1st Qu.:4.130e+00   1st Qu.:    179.3            1st Qu.:     1.52  
    ##  Median :7.859e+01   Median :   1046.5            Median :    55.89  
    ##  Mean   :1.229e+04   Mean   :  27731.1            Mean   :  9206.57  
    ##  3rd Qu.:1.142e+03   3rd Qu.:   5168.4            3rd Qu.:   905.00  
    ##  Max.   :1.812e+06   Max.   :2627266.5            Max.   :835489.38  
    ##  NA's   :22863       NA's   :26606                NA's   :28436      
    ##  cumulative_flaring_co2 cumulative_gas_co2  cumulative_luc_co2 
    ##  Min.   :    0.000      Min.   :0.000e+00   Min.   : -4470.21  
    ##  1st Qu.:    0.000      1st Qu.:0.000e+00   1st Qu.:    12.67  
    ##  Median :    0.000      Median :1.580e-01   Median :   277.37  
    ##  Mean   :  173.969      Mean   :1.822e+03   Mean   : 10064.59  
    ##  3rd Qu.:    5.971      3rd Qu.:8.329e+01   3rd Qu.:  1810.79  
    ##  Max.   :19795.938      Max.   :2.701e+05   Max.   :819483.25  
    ##  NA's   :34300          NA's   :32177       NA's   :12955      
    ##  cumulative_oil_co2  cumulative_other_co2 energy_per_capita energy_per_gdp  
    ##  Min.   :     0.00   Min.   :    0.000    Min.   :     0    Min.   : 0.078  
    ##  1st Qu.:     0.87   1st Qu.:    0.157    1st Qu.:  3057    1st Qu.: 0.856  
    ##  Median :    23.34   Median :   20.264    Median : 13071    Median : 1.294  
    ##  Mean   :  3708.48   Mean   :  312.842    Mean   : 24784    Mean   : 1.768  
    ##  3rd Qu.:   304.52   3rd Qu.:  124.768    3rd Qu.: 35908    3rd Qu.: 2.139  
    ##  Max.   :628324.25   Max.   :10303.908    Max.   :317577    Max.   :25.253  
    ##  NA's   :24974       NA's   :46989        NA's   :40082     NA's   :42495   
    ##   flaring_co2      flaring_co2_per_capita    gas_co2         gas_co2_per_capita
    ##  Min.   :  0.000   Min.   :  0.000        Min.   :   0.000   Min.   : 0.000    
    ##  1st Qu.:  0.000   1st Qu.:  0.000        1st Qu.:   0.000   1st Qu.: 0.000    
    ##  Median :  0.000   Median :  0.000        Median :   0.015   Median : 0.003    
    ##  Mean   :  5.823   Mean   :  0.274        Mean   :  73.621   Mean   : 0.787    
    ##  3rd Qu.:  0.332   3rd Qu.:  0.031        3rd Qu.:   6.275   3rd Qu.: 0.432    
    ##  Max.   :435.263   Max.   :114.514        Max.   :7922.980   Max.   :53.476    
    ##  NA's   :34239     NA's   :35497          NA's   :32177      NA's   :32899     
    ##  ghg_excluding_lucf_per_capita ghg_per_capita    land_use_change_co2
    ##  Min.   :  0.026               Min.   :-27.601   Min.   :-312.169   
    ##  1st Qu.:  0.291               1st Qu.:  2.048   1st Qu.:   0.028   
    ##  Median :  0.691               Median :  4.291   Median :   3.422   
    ##  Mean   :  3.164               Mean   :  7.730   Mean   : 121.937   
    ##  3rd Qu.:  2.928               3rd Qu.:  9.048   3rd Qu.:  23.295   
    ##  Max.   :369.526               Max.   :369.584   Max.   :7743.690   
    ##  NA's   :14552                 NA's   :14378     NA's   :12955      
    ##  land_use_change_co2_per_capita    methane          methane_per_capita
    ##  Min.   :-28.222                Min.   :    0.000   Min.   :  0.032   
    ##  1st Qu.:  0.130                1st Qu.:    0.465   1st Qu.:  0.655   
    ##  Median :  1.270                Median :    3.720   Median :  1.054   
    ##  Mean   :  4.096                Mean   :  101.812   Mean   :  1.859   
    ##  3rd Qu.:  4.380                3rd Qu.:   20.169   3rd Qu.:  1.777   
    ##  Max.   :238.143                Max.   :10529.812   Max.   :120.192   
    ##  NA's   :13757                  NA's   :12781       NA's   :14378     
    ##  nitrous_oxide      nitrous_oxide_per_capita    oil_co2         
    ##  Min.   :   0.000   Min.   : 0.0000          Min.   :    0.000  
    ##  1st Qu.:   0.072   1st Qu.: 0.1090          1st Qu.:    0.084  
    ##  Median :   0.721   Median : 0.2300          Median :    1.365  
    ##  Mean   :  25.679   Mean   : 0.4669          Mean   :  114.659  
    ##  3rd Qu.:   5.100   3rd Qu.: 0.4750          3rd Qu.:   14.158  
    ##  Max.   :2996.286   Max.   :20.4080          Max.   :12411.968  
    ##  NA's   :11911      NA's   :13871            NA's   :24973      
    ##  oil_co2_per_capita other_co2_per_capita other_industry_co2
    ##  Min.   :  0.0000   Min.   :0.000        Min.   :  0.000   
    ##  1st Qu.:  0.0458   1st Qu.:0.028        1st Qu.:  0.008   
    ##  Median :  0.3990   Median :0.067        Median :  1.352   
    ##  Mean   :  2.1862   Mean   :0.075        Mean   : 14.616   
    ##  3rd Qu.:  1.9952   3rd Qu.:0.104        3rd Qu.:  6.392   
    ##  Max.   :782.6820   Max.   :0.356        Max.   :305.381   
    ##  NA's   :25755      NA's   :47717        NA's   :46989     
    ##  primary_energy_consumption share_global_cement_co2 share_global_co2 
    ##  Min.   :0.000e+00          Min.   :  0.000         Min.   :  0.000  
    ##  1st Qu.:7.586e+00          1st Qu.:  0.000         1st Qu.:  0.004  
    ##  Median :6.764e+01          Median :  0.027         Median :  0.053  
    ##  Mean   :2.493e+03          Mean   :  3.168         Mean   :  5.400  
    ##  3rd Qu.:4.685e+02          3rd Qu.:  0.368         3rd Qu.:  0.578  
    ##  Max.   :1.721e+05          Max.   :100.001         Max.   :100.000  
    ##  NA's   :40040              NA's   :28231           NA's   :22863    
    ##  share_global_co2_including_luc share_global_coal_co2
    ##  Min.   : -0.601                Min.   :  0.000      
    ##  1st Qu.:  0.041                1st Qu.:  0.002      
    ##  Median :  0.179                Median :  0.059      
    ##  Mean   :  3.587                Mean   :  6.825      
    ##  3rd Qu.:  0.870                3rd Qu.:  0.923      
    ##  Max.   :100.000                Max.   :100.000      
    ##  NA's   :26606                  NA's   :28436        
    ##  share_global_cumulative_cement_co2 share_global_cumulative_co2
    ##  Min.   :  0.000                    Min.   :  0.000            
    ##  1st Qu.:  0.000                    1st Qu.:  0.002            
    ##  Median :  0.023                    Median :  0.029            
    ##  Mean   :  3.199                    Mean   :  5.531            
    ##  3rd Qu.:  0.386                    3rd Qu.:  0.403            
    ##  Max.   :100.000                    Max.   :100.000            
    ##  NA's   :28231                      NA's   :22863              
    ##  share_global_cumulative_co2_including_luc share_global_cumulative_coal_co2
    ##  Min.   : -0.120                           Min.   :  0.000                 
    ##  1st Qu.:  0.027                           1st Qu.:  0.001                 
    ##  Median :  0.151                           Median :  0.040                 
    ##  Mean   :  3.469                           Mean   :  7.006                 
    ##  3rd Qu.:  0.721                           3rd Qu.:  0.676                 
    ##  Max.   :100.000                           Max.   :100.000                 
    ##  NA's   :26606                             NA's   :28436                   
    ##  share_global_cumulative_flaring_co2 share_global_cumulative_gas_co2
    ##  Min.   :  0.000                     Min.   :  0.000                
    ##  1st Qu.:  0.000                     1st Qu.:  0.000                
    ##  Median :  0.005                     Median :  0.014                
    ##  Mean   :  3.061                     Mean   :  4.107                
    ##  3rd Qu.:  0.485                     3rd Qu.:  0.314                
    ##  Max.   :100.000                     Max.   :100.000                
    ##  NA's   :39322                       NA's   :35157                  
    ##  share_global_cumulative_luc_co2 share_global_cumulative_oil_co2
    ##  Min.   : -0.5940                Min.   :  0.000                
    ##  1st Qu.:  0.0090                1st Qu.:  0.004                
    ##  Median :  0.1110                Median :  0.058                
    ##  Mean   :  2.7313                Mean   :  3.366                
    ##  3rd Qu.:  0.5572                3rd Qu.:  0.511                
    ##  Max.   :100.0000                Max.   :100.000                
    ##  NA's   :12955                   NA's   :26599                  
    ##  share_global_cumulative_other_co2 share_global_flaring_co2
    ##  Min.   :  0.000                   Min.   :  0.000         
    ##  1st Qu.:  0.159                   1st Qu.:  0.000         
    ##  Median :  0.543                   Median :  0.001         
    ##  Mean   :  7.229                   Mean   :  3.142         
    ##  3rd Qu.:  2.289                   3rd Qu.:  0.674         
    ##  Max.   :100.000                   Max.   :100.000         
    ##  NA's   :48083                     NA's   :39322           
    ##  share_global_gas_co2 share_global_luc_co2 share_global_oil_co2
    ##  Min.   :  0.000      Min.   : -8.182      Min.   :  0.000     
    ##  1st Qu.:  0.000      1st Qu.:  0.001      1st Qu.:  0.006     
    ##  Median :  0.025      Median :  0.076      Median :  0.068     
    ##  Mean   :  4.199      Mean   :  2.605      Mean   :  3.348     
    ##  3rd Qu.:  0.488      3rd Qu.:  0.523      3rd Qu.:  0.546     
    ##  Max.   :100.000      Max.   :100.000      Max.   :100.000     
    ##  NA's   :35157        NA's   :12955        NA's   :26599       
    ##  share_global_other_co2 share_of_temperature_change_from_ghg
    ##  Min.   :  0.000        Min.   : -0.810                     
    ##  1st Qu.:  0.205        1st Qu.:  0.004                     
    ##  Median :  0.838        Median :  0.078                     
    ##  Mean   :  7.513        Mean   :  2.269                     
    ##  3rd Qu.:  3.211        3rd Qu.:  0.359                     
    ##  Max.   :100.000        Max.   :100.000                     
    ##  NA's   :48083          NA's   :9190                        
    ##  temperature_change_from_ch4 temperature_change_from_co2
    ##  Min.   :-0.001              Min.   :0.0000             
    ##  1st Qu.: 0.000              1st Qu.:0.0000             
    ##  Median : 0.000              Median :0.0000             
    ##  Mean   : 0.003              Mean   :0.0077             
    ##  3rd Qu.: 0.001              3rd Qu.:0.0010             
    ##  Max.   : 0.422              Max.   :1.1610             
    ##  NA's   :12131               NA's   :9190               
    ##  temperature_change_from_ghg temperature_change_from_n2o   total_ghg        
    ##  Min.   :-0.001              Min.   :0.0000              Min.   :  -14.961  
    ##  1st Qu.: 0.000              1st Qu.:0.0000              1st Qu.:    1.835  
    ##  Median : 0.000              Median :0.0000              Median :   15.008  
    ##  Mean   : 0.011              Mean   :0.0005              Mean   :  488.542  
    ##  3rd Qu.: 0.001              3rd Qu.:0.0000              3rd Qu.:   78.243  
    ##  Max.   : 1.668              Max.   :0.0850              Max.   :53816.852  
    ##  NA's   :9190                NA's   :12131               NA's   :12781      
    ##  total_ghg_excluding_lucf   trade_co2         trade_co2_share  
    ##  Min.   :    0.000        Min.   :-2195.952   Min.   :-98.849  
    ##  1st Qu.:    0.235        1st Qu.:   -3.180   1st Qu.: -6.168  
    ##  Median :    2.371        Median :    1.518   Median :  8.701  
    ##  Mean   :  316.134        Mean   :   -7.232   Mean   : 20.524  
    ##  3rd Qu.:   29.337        3rd Qu.:    9.153   3rd Qu.: 32.666  
    ##  Max.   :44114.785        Max.   : 1798.999   Max.   :568.635  
    ##  NA's   :12955            NA's   :45656       NA's   :45656

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

Examining Datasets

``` r
cat("Dataset 1 - First 6 rows:\n")
```

    ## Dataset 1 - First 6 rows:

``` r
head(df1)
```

    ## # A tibble: 6 × 8
    ##    Year  Land `Land and Ocean` `N Hem` `S Hem` `Band 1` `Band 2` `Band 3`
    ##   <dbl> <dbl>            <dbl>   <dbl>   <dbl>    <dbl>    <dbl>    <dbl>
    ## 1  1880 -0.41            -0.2    -0.34   -0.05       NA       NA       NA
    ## 2  1881 -0.39            -0.11   -0.2    -0.02       NA       NA       NA
    ## 3  1882 -0.3             -0.09   -0.18   -0.01       NA       NA       NA
    ## 4  1883 -0.32            -0.2    -0.31   -0.08       NA       NA       NA
    ## 5  1884 -0.6             -0.27   -0.42   -0.12       NA       NA       NA
    ## 6  1885 -0.46            -0.31   -0.41   -0.21       NA       NA       NA

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
cat("Dataset 2 - First 6 rows:\n")
```

    ## Dataset 2 - First 6 rows:

``` r
head(df2)
```

    ## # A tibble: 6 × 79
    ##   country  year iso_code population   gdp cement_co2 cement_co2_per_capita   co2
    ##   <chr>   <dbl> <chr>         <dbl> <dbl>      <dbl>                 <dbl> <dbl>
    ## 1 Afghan…  1750 AFG         2802560    NA          0                     0    NA
    ## 2 Afghan…  1751 AFG              NA    NA          0                    NA    NA
    ## 3 Afghan…  1752 AFG              NA    NA          0                    NA    NA
    ## 4 Afghan…  1753 AFG              NA    NA          0                    NA    NA
    ## 5 Afghan…  1754 AFG              NA    NA          0                    NA    NA
    ## 6 Afghan…  1755 AFG              NA    NA          0                    NA    NA
    ## # ℹ 71 more variables: co2_growth_abs <dbl>, co2_growth_prct <dbl>,
    ## #   co2_including_luc <dbl>, co2_including_luc_growth_abs <dbl>,
    ## #   co2_including_luc_growth_prct <dbl>, co2_including_luc_per_capita <dbl>,
    ## #   co2_including_luc_per_gdp <dbl>, co2_including_luc_per_unit_energy <dbl>,
    ## #   co2_per_capita <dbl>, co2_per_gdp <dbl>, co2_per_unit_energy <dbl>,
    ## #   coal_co2 <dbl>, coal_co2_per_capita <dbl>, consumption_co2 <dbl>,
    ## #   consumption_co2_per_capita <dbl>, consumption_co2_per_gdp <dbl>, …

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
cat("Dataset 1 data types:\n")
```

    ## Dataset 1 data types:

``` r
sapply(df1, class)
```

    ##           Year           Land Land and Ocean          N Hem          S Hem 
    ##      "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
    ##         Band 1         Band 2         Band 3 
    ##      "numeric"      "numeric"      "numeric"

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
cat("Dataset 2 data types:\n")
```

    ## Dataset 2 data types:

``` r
sapply(df2, class)
```

    ##                                   country 
    ##                               "character" 
    ##                                      year 
    ##                                 "numeric" 
    ##                                  iso_code 
    ##                               "character" 
    ##                                population 
    ##                                 "numeric" 
    ##                                       gdp 
    ##                                 "numeric" 
    ##                                cement_co2 
    ##                                 "numeric" 
    ##                     cement_co2_per_capita 
    ##                                 "numeric" 
    ##                                       co2 
    ##                                 "numeric" 
    ##                            co2_growth_abs 
    ##                                 "numeric" 
    ##                           co2_growth_prct 
    ##                                 "numeric" 
    ##                         co2_including_luc 
    ##                                 "numeric" 
    ##              co2_including_luc_growth_abs 
    ##                                 "numeric" 
    ##             co2_including_luc_growth_prct 
    ##                                 "numeric" 
    ##              co2_including_luc_per_capita 
    ##                                 "numeric" 
    ##                 co2_including_luc_per_gdp 
    ##                                 "numeric" 
    ##         co2_including_luc_per_unit_energy 
    ##                                 "numeric" 
    ##                            co2_per_capita 
    ##                                 "numeric" 
    ##                               co2_per_gdp 
    ##                                 "numeric" 
    ##                       co2_per_unit_energy 
    ##                                 "numeric" 
    ##                                  coal_co2 
    ##                                 "numeric" 
    ##                       coal_co2_per_capita 
    ##                                 "numeric" 
    ##                           consumption_co2 
    ##                                 "numeric" 
    ##                consumption_co2_per_capita 
    ##                                 "numeric" 
    ##                   consumption_co2_per_gdp 
    ##                                 "numeric" 
    ##                     cumulative_cement_co2 
    ##                                 "numeric" 
    ##                            cumulative_co2 
    ##                                 "numeric" 
    ##              cumulative_co2_including_luc 
    ##                                 "numeric" 
    ##                       cumulative_coal_co2 
    ##                                 "numeric" 
    ##                    cumulative_flaring_co2 
    ##                                 "numeric" 
    ##                        cumulative_gas_co2 
    ##                                 "numeric" 
    ##                        cumulative_luc_co2 
    ##                                 "numeric" 
    ##                        cumulative_oil_co2 
    ##                                 "numeric" 
    ##                      cumulative_other_co2 
    ##                                 "numeric" 
    ##                         energy_per_capita 
    ##                                 "numeric" 
    ##                            energy_per_gdp 
    ##                                 "numeric" 
    ##                               flaring_co2 
    ##                                 "numeric" 
    ##                    flaring_co2_per_capita 
    ##                                 "numeric" 
    ##                                   gas_co2 
    ##                                 "numeric" 
    ##                        gas_co2_per_capita 
    ##                                 "numeric" 
    ##             ghg_excluding_lucf_per_capita 
    ##                                 "numeric" 
    ##                            ghg_per_capita 
    ##                                 "numeric" 
    ##                       land_use_change_co2 
    ##                                 "numeric" 
    ##            land_use_change_co2_per_capita 
    ##                                 "numeric" 
    ##                                   methane 
    ##                                 "numeric" 
    ##                        methane_per_capita 
    ##                                 "numeric" 
    ##                             nitrous_oxide 
    ##                                 "numeric" 
    ##                  nitrous_oxide_per_capita 
    ##                                 "numeric" 
    ##                                   oil_co2 
    ##                                 "numeric" 
    ##                        oil_co2_per_capita 
    ##                                 "numeric" 
    ##                      other_co2_per_capita 
    ##                                 "numeric" 
    ##                        other_industry_co2 
    ##                                 "numeric" 
    ##                primary_energy_consumption 
    ##                                 "numeric" 
    ##                   share_global_cement_co2 
    ##                                 "numeric" 
    ##                          share_global_co2 
    ##                                 "numeric" 
    ##            share_global_co2_including_luc 
    ##                                 "numeric" 
    ##                     share_global_coal_co2 
    ##                                 "numeric" 
    ##        share_global_cumulative_cement_co2 
    ##                                 "numeric" 
    ##               share_global_cumulative_co2 
    ##                                 "numeric" 
    ## share_global_cumulative_co2_including_luc 
    ##                                 "numeric" 
    ##          share_global_cumulative_coal_co2 
    ##                                 "numeric" 
    ##       share_global_cumulative_flaring_co2 
    ##                                 "numeric" 
    ##           share_global_cumulative_gas_co2 
    ##                                 "numeric" 
    ##           share_global_cumulative_luc_co2 
    ##                                 "numeric" 
    ##           share_global_cumulative_oil_co2 
    ##                                 "numeric" 
    ##         share_global_cumulative_other_co2 
    ##                                 "numeric" 
    ##                  share_global_flaring_co2 
    ##                                 "numeric" 
    ##                      share_global_gas_co2 
    ##                                 "numeric" 
    ##                      share_global_luc_co2 
    ##                                 "numeric" 
    ##                      share_global_oil_co2 
    ##                                 "numeric" 
    ##                    share_global_other_co2 
    ##                                 "numeric" 
    ##      share_of_temperature_change_from_ghg 
    ##                                 "numeric" 
    ##               temperature_change_from_ch4 
    ##                                 "numeric" 
    ##               temperature_change_from_co2 
    ##                                 "numeric" 
    ##               temperature_change_from_ghg 
    ##                                 "numeric" 
    ##               temperature_change_from_n2o 
    ##                                 "numeric" 
    ##                                 total_ghg 
    ##                                 "numeric" 
    ##                  total_ghg_excluding_lucf 
    ##                                 "numeric" 
    ##                                 trade_co2 
    ##                                 "numeric" 
    ##                           trade_co2_share 
    ##                                 "numeric"

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

Checking for Missing Values

``` r
#analysing missing values
missing_df1 <- df1 %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(
    Missing_Percentage = (Missing_Count / nrow(df1)) * 100,
    Present_Count = nrow(df1) - Missing_Count,
    Present_Percentage = 100 - Missing_Percentage
  ) %>%
  arrange(desc(Missing_Count))

print("Dataset 1 - Missing Values:")
```

    ## [1] "Dataset 1 - Missing Values:"

``` r
print(missing_df1)
```

    ## # A tibble: 8 × 5
    ##   Variable     Missing_Count Missing_Percentage Present_Count Present_Percentage
    ##   <chr>                <int>              <dbl>         <int>              <dbl>
    ## 1 Band 1                  20               14.7           116               85.3
    ## 2 Band 2                  20               14.7           116               85.3
    ## 3 Band 3                  20               14.7           116               85.3
    ## 4 Year                     0                0             136              100  
    ## 5 Land                     0                0             136              100  
    ## 6 Land and Oc…             0                0             136              100  
    ## 7 N Hem                    0                0             136              100  
    ## 8 S Hem                    0                0             136              100

``` r
missing_df2 <- df2 %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(
    Missing_Percentage = (Missing_Count / nrow(df2)) * 100,
    Present_Count = nrow(df2) - Missing_Count,
    Present_Percentage = 100 - Missing_Percentage
  ) %>%
  arrange(desc(Missing_Count))

print("Dataset 2 - Missing Values:")
```

    ## [1] "Dataset 2 - Missing Values:"

``` r
print(missing_df2)
```

    ## # A tibble: 79 × 5
    ##    Variable    Missing_Count Missing_Percentage Present_Count Present_Percentage
    ##    <chr>               <int>              <dbl>         <int>              <dbl>
    ##  1 share_glob…         48083               95.8          2108               4.20
    ##  2 share_glob…         48083               95.8          2108               4.20
    ##  3 other_co2_…         47717               95.1          2474               4.93
    ##  4 cumulative…         46989               93.6          3202               6.38
    ##  5 other_indu…         46989               93.6          3202               6.38
    ##  6 consumptio…         45747               91.1          4444               8.85
    ##  7 consumptio…         45689               91.0          4502               8.97
    ##  8 trade_co2           45656               91.0          4535               9.04
    ##  9 trade_co2_…         45656               91.0          4535               9.04
    ## 10 consumptio…         45325               90.3          4866               9.69
    ## # ℹ 69 more rows

``` r
#visualising missing data patterns
p1 <- ggplot(missing_df1, aes(x = reorder(Variable, Missing_Percentage))) +
  geom_col(aes(y = Present_Percentage), fill = "#17becf", alpha = 0.8) +
  geom_col(aes(y = Missing_Percentage), fill = "#ff7f0e", alpha = 0.9) +
  coord_flip() +
  labs(
    title = "Missing Data Pattern - Dataset 1",
    subtitle = "Orange = Missing, Blue = Present",
    x = "Variables",
    y = "Percentage",
    caption = paste("Total observations:", nrow(df1))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

print(p1)
```

![](r-notebook_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
p2<- ggplot(missing_df2, aes(x = reorder(Variable, Missing_Percentage))) +
  geom_col(aes(y = Present_Percentage, text = paste("Variable:", Variable, "<br>Present:", round(Present_Percentage, 1), "%")), 
           fill = "#17becf", alpha = 0.8) +
  geom_col(aes(y = Missing_Percentage, text = paste("Variable:", Variable, "<br>Missing:", round(Missing_Percentage, 1), "%")), 
           fill = "#ff7f0e", alpha = 0.9) +
  coord_flip() +
  labs(title = "Missing Data Pattern - Dataset 2",
       x = "Variables", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.y = element_blank())
```

    ## Warning in geom_col(aes(y = Present_Percentage, text = paste("Variable:", :
    ## Ignoring unknown aesthetics: text

    ## Warning in geom_col(aes(y = Missing_Percentage, text = paste("Variable:", :
    ## Ignoring unknown aesthetics: text

``` r
ggplotly(p2, tooltip = "text")
```

    ## file:////tmp/Rtmpsui8Sr/file14a1b391ca54e/widget14a1b6e7a42b0.html screenshot completed

![](r-notebook_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

Identifying duplicates and unique values in Data

``` r
cat("Dataset 1 duplicates:", sum(duplicated(df1)), "\n")
```

    ## Dataset 1 duplicates: 0

``` r
cat("Dataset 2 duplicates:", sum(duplicated(df2)), "\n")
```

    ## Dataset 2 duplicates: 0

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

``` r
check_categorical <- function(df, dataset_name) {
  char_cols <- sapply(df, function(x) is.character(x) | is.factor(x))
  char_col_names <- names(df)[char_cols]
  
  for (col in char_col_names) {
    unique_count <- length(unique(df[[col]]))
    cat(sprintf("%s - %s: %d unique values\n", dataset_name, col, unique_count))
    
    if (unique_count <= 20) {
      print(table(df[[col]], useNA = "ifany"))
      cat("\n")
    }
  }
}

check_categorical(df1, "Dataset 1")
check_categorical(df2, "Dataset 2")
```

    ## Dataset 2 - country: 255 unique values
    ## Dataset 2 - iso_code: 219 unique values

``` r
cat(strrep("-", 50), "\n")
```

    ## --------------------------------------------------

Handling Missing Values

``` r
handle_missing_values <- function(df, strategy = "auto") {
  df_clean <- df

  missing_pct <- df %>%
    summarise_all(~sum(is.na(.)) / length(.) * 100)
  
  #remove columns with >50% missing values
  high_missing_cols <- names(missing_pct)[missing_pct > 50]
  if (length(high_missing_cols) > 0) {
    cat("Removing columns with >50% missing values:", paste(high_missing_cols, collapse = ", "), "\n")
    df_clean <- df_clean %>% select(-all_of(high_missing_cols))
  }
  cat(strrep("-", 60), "\n")
  retained_cols <- names(df_clean)
  cat("Retained", length(retained_cols), "columns:", paste(retained_cols, collapse = ", "), "\n")
  
  #handling remaining missing values
  for (col in names(df_clean)) {
    if (sum(is.na(df_clean[[col]])) > 0) {
      if (is.numeric(df_clean[[col]])) {
        #using median for numeric columns
        df_clean[[col]][is.na(df_clean[[col]])] <- median(df_clean[[col]], na.rm = TRUE)
      } else {
        #using mode for categorical columns
        mode_val <- names(sort(table(df_clean[[col]]), decreasing = TRUE))[1]
        df_clean[[col]][is.na(df_clean[[col]])] <- mode_val
      }
    }
  }
  
  return(df_clean)
}

df1_clean <- handle_missing_values(df1)
```

    ## ------------------------------------------------------------ 
    ## Retained 8 columns: Year, Land, Land and Ocean, N Hem, S Hem, Band 1, Band 2, Band 3

``` r
cat(strrep("-", 60), "\n")
```

    ## ------------------------------------------------------------

``` r
cat("Dataset 1: Columns", ncol(df1), "->", ncol(df1_clean), "| Rows", nrow(df1), "->", nrow(df1_clean), "after cleaning\n")
```

    ## Dataset 1: Columns 8 -> 8 | Rows 136 -> 136 after cleaning

``` r
cat(strrep("-", 60), "\n")
```

    ## ------------------------------------------------------------

``` r
df2_clean <- handle_missing_values(df2)
```

    ## Removing columns with >50% missing values: gdp, co2_including_luc, co2_including_luc_growth_abs, co2_including_luc_growth_prct, co2_including_luc_per_capita, co2_including_luc_per_gdp, co2_including_luc_per_unit_energy, co2_per_gdp, co2_per_unit_energy, coal_co2, coal_co2_per_capita, consumption_co2, consumption_co2_per_capita, consumption_co2_per_gdp, cumulative_co2_including_luc, cumulative_coal_co2, cumulative_flaring_co2, cumulative_gas_co2, cumulative_other_co2, energy_per_capita, energy_per_gdp, flaring_co2, flaring_co2_per_capita, gas_co2, gas_co2_per_capita, oil_co2_per_capita, other_co2_per_capita, other_industry_co2, primary_energy_consumption, share_global_cement_co2, share_global_co2_including_luc, share_global_coal_co2, share_global_cumulative_cement_co2, share_global_cumulative_co2_including_luc, share_global_cumulative_coal_co2, share_global_cumulative_flaring_co2, share_global_cumulative_gas_co2, share_global_cumulative_oil_co2, share_global_cumulative_other_co2, share_global_flaring_co2, share_global_gas_co2, share_global_oil_co2, share_global_other_co2, trade_co2, trade_co2_share 
    ## ------------------------------------------------------------ 
    ## Retained 34 columns: country, year, iso_code, population, cement_co2, cement_co2_per_capita, co2, co2_growth_abs, co2_growth_prct, co2_per_capita, cumulative_cement_co2, cumulative_co2, cumulative_luc_co2, cumulative_oil_co2, ghg_excluding_lucf_per_capita, ghg_per_capita, land_use_change_co2, land_use_change_co2_per_capita, methane, methane_per_capita, nitrous_oxide, nitrous_oxide_per_capita, oil_co2, share_global_co2, share_global_cumulative_co2, share_global_cumulative_luc_co2, share_global_luc_co2, share_of_temperature_change_from_ghg, temperature_change_from_ch4, temperature_change_from_co2, temperature_change_from_ghg, temperature_change_from_n2o, total_ghg, total_ghg_excluding_lucf

``` r
cat(strrep("-", 60), "\n")
```

    ## ------------------------------------------------------------

``` r
cat("Dataset 2: Columns", ncol(df2), "->", ncol(df2_clean), "| Rows", nrow(df2), "->", nrow(df2_clean), "after cleaning\n")
```

    ## Dataset 2: Columns 79 -> 34 | Rows 50191 -> 50191 after cleaning

``` r
cat(strrep("-", 60), "\n")
```

    ## ------------------------------------------------------------

Univariate Analysis

For numeric columns, we will use shapiro.test() to test if the data
follows normal distribution. Upon calculating the p-value:

- If p \> 0.05: Data is likely normally distributed
- If p ≤ 0.05: Data significantly deviates from normal distribution

``` r
create_univariate_plots <- function(df, dataset_name) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  categorical_cols <- names(df)[sapply(df, is.factor)]

  for (col in numeric_cols) {
    p1 <- ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
      ggtitle(paste("Distribution of", col)) +
      theme_minimal()
    
    p2 <- ggplot(df, aes(y = .data[[col]])) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      ggtitle(paste("Boxplot of", col)) +
      theme_minimal()

    print(p1)
    print(p2)

    shapiro_test <- shapiro.test(sample(df[[col]], min(5000, length(df[[col]]))))
    cat(sprintf("%s - Shapiro-Wilk normality test p-value: %.6f\n", col, shapiro_test$p.value))
  }

  for (col in categorical_cols) {
    p <- ggplot(df, aes(x = .data[[col]])) +
      geom_bar(fill = "coral", alpha = 0.7) +
      ggtitle(paste("Distribution of", col)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}

create_univariate_plots(df1_clean, "Dataset 1")
```

![](r-notebook_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

    ## Year - Shapiro-Wilk normality test p-value: 0.000183

![](r-notebook_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

    ## Land - Shapiro-Wilk normality test p-value: 0.000005

![](r-notebook_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

    ## Land and Ocean - Shapiro-Wilk normality test p-value: 0.000003

![](r-notebook_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

    ## N Hem - Shapiro-Wilk normality test p-value: 0.000004

![](r-notebook_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->

    ## S Hem - Shapiro-Wilk normality test p-value: 0.000002

![](r-notebook_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->

    ## Band 1 - Shapiro-Wilk normality test p-value: 0.000001

![](r-notebook_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->

    ## Band 2 - Shapiro-Wilk normality test p-value: 0.032602

![](r-notebook_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->

    ## Band 3 - Shapiro-Wilk normality test p-value: 0.000054

``` r
create_univariate_plots(df2_clean, "Dataset 2")
```

![](r-notebook_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->

    ## year - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->

    ## population - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->

    ## cement_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->

    ## cement_co2_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->

    ## co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->

    ## co2_growth_abs - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-29.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-30.png)<!-- -->

    ## co2_growth_prct - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-31.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-32.png)<!-- -->

    ## co2_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-33.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-34.png)<!-- -->

    ## cumulative_cement_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-35.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-36.png)<!-- -->

    ## cumulative_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-37.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-38.png)<!-- -->

    ## cumulative_luc_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-39.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-40.png)<!-- -->

    ## cumulative_oil_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-41.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-42.png)<!-- -->

    ## ghg_excluding_lucf_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-43.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-44.png)<!-- -->

    ## ghg_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-45.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-46.png)<!-- -->

    ## land_use_change_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-47.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-48.png)<!-- -->

    ## land_use_change_co2_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-49.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-50.png)<!-- -->

    ## methane - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-51.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-52.png)<!-- -->

    ## methane_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-53.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-54.png)<!-- -->

    ## nitrous_oxide - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-55.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-56.png)<!-- -->

    ## nitrous_oxide_per_capita - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-57.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-58.png)<!-- -->

    ## oil_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-59.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-60.png)<!-- -->

    ## share_global_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-61.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-62.png)<!-- -->

    ## share_global_cumulative_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-63.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-64.png)<!-- -->

    ## share_global_cumulative_luc_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-65.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-66.png)<!-- -->

    ## share_global_luc_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-67.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-68.png)<!-- -->

    ## share_of_temperature_change_from_ghg - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-69.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-70.png)<!-- -->

    ## temperature_change_from_ch4 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-71.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-72.png)<!-- -->

    ## temperature_change_from_co2 - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-73.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-74.png)<!-- -->

    ## temperature_change_from_ghg - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-75.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-76.png)<!-- -->

    ## temperature_change_from_n2o - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-77.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-78.png)<!-- -->

    ## total_ghg - Shapiro-Wilk normality test p-value: 0.000000

![](r-notebook_files/figure-gfm/unnamed-chunk-8-79.png)<!-- -->![](r-notebook_files/figure-gfm/unnamed-chunk-8-80.png)<!-- -->

    ## total_ghg_excluding_lucf - Shapiro-Wilk normality test p-value: 0.000000

Correlation analysis between numeric variables in a dataset

``` r
analyze_correlations <- function(df, dataset_name) {
  numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
  
  if (ncol(numeric_df) < 2) {
    cat("Not enough numeric variables for correlation analysis in", dataset_name, "\n")
    return()
  }
  
  cor_matrix <- cor(numeric_df, use = "complete.obs")
  cat("=== CORRELATION MATRIX FOR", dataset_name, "===\n")
  print(round(cor_matrix, 3))
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           title = paste("Correlation Matrix -", dataset_name))

  ggcorrplot::ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",
                        lab = TRUE, lab_size = 3, method = "circle",
                        colors = c("red", "white", "blue"),
                        title = paste("Correlation Heatmap -", dataset_name))

  strong_correlations <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
  
  if (nrow(strong_correlations) > 0) {
    cat("\nStrong correlations (|r| > 0.7) in", dataset_name, ":\n")
    for (i in 1:nrow(strong_correlations)) {
      row_idx <- strong_correlations[i, 1]
      col_idx <- strong_correlations[i, 2]
      var1 <- rownames(cor_matrix)[row_idx]
      var2 <- colnames(cor_matrix)[col_idx]
      correlation <- cor_matrix[row_idx, col_idx]
      cat(sprintf("%s - %s: %.3f\n", var1, var2, correlation))

      p <- ggplot(df, aes(x = !!sym(var1), y = !!sym(var2))) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(method = "lm", color = "red", se = TRUE) +
        ggtitle(paste(var1, "vs", var2, sprintf("(r = %.3f)", correlation))) +
        theme_minimal()
      print(p)
    }
  }
  
  return(cor_matrix)
}

cor_matrix1 <- analyze_correlations(df1_clean, "Dataset 1")
```

    ## === CORRELATION MATRIX FOR Dataset 1 ===
    ##                 Year  Land Land and Ocean N Hem S Hem Band 1 Band 2 Band 3
    ## Year           1.000 0.906          0.866 0.843 0.831  0.665  0.678  0.776
    ## Land           0.906 1.000          0.975 0.967 0.914  0.847  0.832  0.833
    ## Land and Ocean 0.866 0.975          1.000 0.975 0.960  0.892  0.907  0.888
    ## N Hem          0.843 0.967          0.975 1.000 0.873  0.920  0.852  0.783
    ## S Hem          0.831 0.914          0.960 0.873 1.000  0.792  0.912  0.955
    ## Band 1         0.665 0.847          0.892 0.920 0.792  1.000  0.806  0.772
    ## Band 2         0.678 0.832          0.907 0.852 0.912  0.806  1.000  0.826
    ## Band 3         0.776 0.833          0.888 0.783 0.955  0.772  0.826  1.000

![](r-notebook_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## 
    ## Strong correlations (|r| > 0.7) in Dataset 1 :
    ## Land - Year: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

    ## Land and Ocean - Year: 0.866

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

    ## N Hem - Year: 0.843

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

    ## S Hem - Year: 0.831

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

    ## Band 3 - Year: 0.776

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->

    ## Year - Land: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->

    ## Land and Ocean - Land: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->

    ## N Hem - Land: 0.967

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->

    ## S Hem - Land: 0.914

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->

    ## Band 1 - Land: 0.847

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->

    ## Band 2 - Land: 0.832

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-12.png)<!-- -->

    ## Band 3 - Land: 0.833

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-13.png)<!-- -->

    ## Year - Land and Ocean: 0.866

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-14.png)<!-- -->

    ## Land - Land and Ocean: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-15.png)<!-- -->

    ## N Hem - Land and Ocean: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-16.png)<!-- -->

    ## S Hem - Land and Ocean: 0.960

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-17.png)<!-- -->

    ## Band 1 - Land and Ocean: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-18.png)<!-- -->

    ## Band 2 - Land and Ocean: 0.907

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-19.png)<!-- -->

    ## Band 3 - Land and Ocean: 0.888

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-20.png)<!-- -->

    ## Year - N Hem: 0.843

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-21.png)<!-- -->

    ## Land - N Hem: 0.967

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-22.png)<!-- -->

    ## Land and Ocean - N Hem: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-23.png)<!-- -->

    ## S Hem - N Hem: 0.873

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-24.png)<!-- -->

    ## Band 1 - N Hem: 0.920

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-25.png)<!-- -->

    ## Band 2 - N Hem: 0.852

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-26.png)<!-- -->

    ## Band 3 - N Hem: 0.783

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-27.png)<!-- -->

    ## Year - S Hem: 0.831

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-28.png)<!-- -->

    ## Land - S Hem: 0.914

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-29.png)<!-- -->

    ## Land and Ocean - S Hem: 0.960

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-30.png)<!-- -->

    ## N Hem - S Hem: 0.873

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-31.png)<!-- -->

    ## Band 1 - S Hem: 0.792

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-32.png)<!-- -->

    ## Band 2 - S Hem: 0.912

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-33.png)<!-- -->

    ## Band 3 - S Hem: 0.955

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-34.png)<!-- -->

    ## Land - Band 1: 0.847

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-35.png)<!-- -->

    ## Land and Ocean - Band 1: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-36.png)<!-- -->

    ## N Hem - Band 1: 0.920

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-37.png)<!-- -->

    ## S Hem - Band 1: 0.792

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-38.png)<!-- -->

    ## Band 2 - Band 1: 0.806

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-39.png)<!-- -->

    ## Band 3 - Band 1: 0.772

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-40.png)<!-- -->

    ## Land - Band 2: 0.832

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-41.png)<!-- -->

    ## Land and Ocean - Band 2: 0.907

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-42.png)<!-- -->

    ## N Hem - Band 2: 0.852

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-43.png)<!-- -->

    ## S Hem - Band 2: 0.912

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-44.png)<!-- -->

    ## Band 1 - Band 2: 0.806

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-45.png)<!-- -->

    ## Band 3 - Band 2: 0.826

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-46.png)<!-- -->

    ## Year - Band 3: 0.776

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-47.png)<!-- -->

    ## Land - Band 3: 0.833

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-48.png)<!-- -->

    ## Land and Ocean - Band 3: 0.888

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-49.png)<!-- -->

    ## N Hem - Band 3: 0.783

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-50.png)<!-- -->

    ## S Hem - Band 3: 0.955

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-51.png)<!-- -->

    ## Band 1 - Band 3: 0.772

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-52.png)<!-- -->

    ## Band 2 - Band 3: 0.826

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-53.png)<!-- -->

``` r
cor_matrix2 <- analyze_correlations(df2_clean, "Dataset 2")
```

    ## === CORRELATION MATRIX FOR Dataset 2 ===
    ##                                        year population cement_co2
    ## year                                  1.000      0.100      0.114
    ## population                            0.100      1.000      0.824
    ## cement_co2                            0.114      0.824      1.000
    ## cement_co2_per_capita                 0.356      0.104      0.179
    ## co2                                   0.160      0.703      0.751
    ## co2_growth_abs                        0.059      0.508      0.495
    ## co2_growth_prct                       0.003     -0.002     -0.002
    ## co2_per_capita                        0.142      0.011      0.031
    ## cumulative_cement_co2                 0.125      0.779      0.941
    ## cumulative_co2                        0.133      0.716      0.764
    ## cumulative_luc_co2                    0.145      0.832      0.708
    ## cumulative_oil_co2                    0.130      0.707      0.753
    ## ghg_excluding_lucf_per_capita         0.249      0.036      0.056
    ## ghg_per_capita                        0.132     -0.006      0.015
    ## land_use_change_co2                   0.068      0.646      0.393
    ## land_use_change_co2_per_capita       -0.028     -0.033     -0.029
    ## methane                               0.133      0.898      0.793
    ## methane_per_capita                    0.115     -0.013     -0.002
    ## nitrous_oxide                         0.148      0.857      0.788
    ## nitrous_oxide_per_capita              0.146     -0.012      0.005
    ## oil_co2                               0.136      0.794      0.790
    ## share_global_co2                     -0.131      0.341      0.271
    ## share_global_cumulative_co2          -0.124      0.304      0.236
    ## share_global_cumulative_luc_co2       0.052      0.602      0.426
    ## share_global_luc_co2                  0.042      0.603      0.376
    ## share_of_temperature_change_from_ghg  0.053      0.547      0.412
    ## temperature_change_from_ch4           0.150      0.888      0.830
    ## temperature_change_from_co2           0.147      0.750      0.731
    ## temperature_change_from_ghg           0.150      0.796      0.767
    ## temperature_change_from_n2o           0.148      0.807      0.780
    ## total_ghg                             0.136      0.847      0.812
    ## total_ghg_excluding_lucf              0.140      0.807      0.843
    ##                                      cement_co2_per_capita    co2
    ## year                                                 0.356  0.160
    ## population                                           0.104  0.703
    ## cement_co2                                           0.179  0.751
    ## cement_co2_per_capita                                1.000  0.140
    ## co2                                                  0.140  1.000
    ## co2_growth_abs                                       0.090  0.411
    ## co2_growth_prct                                     -0.005 -0.003
    ## co2_per_capita                                       0.218  0.045
    ## cumulative_cement_co2                                0.179  0.773
    ## cumulative_co2                                       0.169  0.797
    ## cumulative_luc_co2                                   0.132  0.737
    ## cumulative_oil_co2                                   0.156  0.771
    ## ghg_excluding_lucf_per_capita                        0.454  0.077
    ## ghg_per_capita                                       0.252  0.028
    ## land_use_change_co2                                  0.026  0.434
    ## land_use_change_co2_per_capita                      -0.093 -0.040
    ## methane                                              0.128  0.755
    ## methane_per_capita                                   0.179 -0.002
    ## nitrous_oxide                                        0.143  0.766
    ## nitrous_oxide_per_capita                             0.124  0.008
    ## oil_co2                                              0.179  0.818
    ## share_global_co2                                     0.023  0.331
    ## share_global_cumulative_co2                          0.024  0.325
    ## share_global_cumulative_luc_co2                      0.055  0.503
    ## share_global_luc_co2                                 0.016  0.406
    ## share_of_temperature_change_from_ghg                 0.058  0.508
    ## temperature_change_from_ch4                          0.143  0.784
    ## temperature_change_from_co2                          0.143  0.761
    ## temperature_change_from_ghg                          0.144  0.775
    ## temperature_change_from_n2o                          0.138  0.766
    ## total_ghg                                            0.146  0.783
    ## total_ghg_excluding_lucf                             0.168  0.799
    ##                                      co2_growth_abs co2_growth_prct
    ## year                                          0.059           0.003
    ## population                                    0.508          -0.002
    ## cement_co2                                    0.495          -0.002
    ## cement_co2_per_capita                         0.090          -0.005
    ## co2                                           0.411          -0.003
    ## co2_growth_abs                                1.000           0.000
    ## co2_growth_prct                               0.000           1.000
    ## co2_per_capita                                0.026           0.004
    ## cumulative_cement_co2                         0.384          -0.002
    ## cumulative_co2                                0.342          -0.002
    ## cumulative_luc_co2                            0.475          -0.003
    ## cumulative_oil_co2                            0.307          -0.002
    ## ghg_excluding_lucf_per_capita                 0.050           0.008
    ## ghg_per_capita                                0.024           0.015
    ## land_use_change_co2                           0.376          -0.003
    ## land_use_change_co2_per_capita               -0.011           0.017
    ## methane                                       0.507          -0.003
    ## methane_per_capita                            0.003           0.001
    ## nitrous_oxide                                 0.486          -0.003
    ## nitrous_oxide_per_capita                      0.005          -0.001
    ## oil_co2                                       0.444          -0.002
    ## share_global_co2                              0.232          -0.003
    ## share_global_cumulative_co2                   0.198          -0.003
    ## share_global_cumulative_luc_co2               0.358          -0.003
    ## share_global_luc_co2                          0.339          -0.003
    ## share_of_temperature_change_from_ghg          0.333          -0.003
    ## temperature_change_from_ch4                   0.481          -0.003
    ## temperature_change_from_co2                   0.388          -0.003
    ## temperature_change_from_ghg                   0.417          -0.003
    ## temperature_change_from_n2o                   0.402          -0.003
    ## total_ghg                                     0.492          -0.003
    ## total_ghg_excluding_lucf                      0.471          -0.003
    ##                                      co2_per_capita cumulative_cement_co2
    ## year                                          0.142                 0.125
    ## population                                    0.011                 0.779
    ## cement_co2                                    0.031                 0.941
    ## cement_co2_per_capita                         0.218                 0.179
    ## co2                                           0.045                 0.773
    ## co2_growth_abs                                0.026                 0.384
    ## co2_growth_prct                               0.004                -0.002
    ## co2_per_capita                                1.000                 0.041
    ## cumulative_cement_co2                         0.041                 1.000
    ## cumulative_co2                                0.065                 0.892
    ## cumulative_luc_co2                            0.046                 0.711
    ## cumulative_oil_co2                            0.058                 0.893
    ## ghg_excluding_lucf_per_capita                 0.386                 0.070
    ## ghg_per_capita                                0.250                 0.023
    ## land_use_change_co2                           0.003                 0.345
    ## land_use_change_co2_per_capita               -0.037                -0.033
    ## methane                                       0.032                 0.775
    ## methane_per_capita                            0.193                -0.001
    ## nitrous_oxide                                 0.039                 0.789
    ## nitrous_oxide_per_capita                      0.057                 0.011
    ## oil_co2                                       0.063                 0.848
    ## share_global_co2                              0.019                 0.267
    ## share_global_cumulative_co2                   0.023                 0.256
    ## share_global_cumulative_luc_co2               0.027                 0.417
    ## share_global_luc_co2                          0.000                 0.337
    ## share_of_temperature_change_from_ghg          0.031                 0.424
    ## temperature_change_from_ch4                   0.039                 0.837
    ## temperature_change_from_co2                   0.053                 0.809
    ## temperature_change_from_ghg                   0.050                 0.827
    ## temperature_change_from_n2o                   0.044                 0.848
    ## total_ghg                                     0.044                 0.816
    ## total_ghg_excluding_lucf                      0.054                 0.862
    ##                                      cumulative_co2 cumulative_luc_co2
    ## year                                          0.133              0.145
    ## population                                    0.716              0.832
    ## cement_co2                                    0.764              0.708
    ## cement_co2_per_capita                         0.169              0.132
    ## co2                                           0.797              0.737
    ## co2_growth_abs                                0.342              0.475
    ## co2_growth_prct                              -0.002             -0.003
    ## co2_per_capita                                0.065              0.046
    ## cumulative_cement_co2                         0.892              0.711
    ## cumulative_co2                                1.000              0.803
    ## cumulative_luc_co2                            0.803              1.000
    ## cumulative_oil_co2                            0.979              0.755
    ## ghg_excluding_lucf_per_capita                 0.109              0.084
    ## ghg_per_capita                                0.050              0.050
    ## land_use_change_co2                           0.418              0.802
    ## land_use_change_co2_per_capita               -0.036             -0.002
    ## methane                                       0.800              0.925
    ## methane_per_capita                            0.007              0.006
    ## nitrous_oxide                                 0.834              0.892
    ## nitrous_oxide_per_capita                      0.025              0.019
    ## oil_co2                                       0.941              0.850
    ## share_global_co2                              0.358              0.452
    ## share_global_cumulative_co2                   0.379              0.433
    ## share_global_cumulative_luc_co2               0.533              0.810
    ## share_global_luc_co2                          0.394              0.743
    ## share_of_temperature_change_from_ghg          0.563              0.744
    ## temperature_change_from_ch4                   0.855              0.908
    ## temperature_change_from_co2                   0.908              0.860
    ## temperature_change_from_ghg                   0.904              0.881
    ## temperature_change_from_n2o                   0.901              0.844
    ## total_ghg                                     0.853              0.908
    ## total_ghg_excluding_lucf                      0.889              0.846
    ##                                      cumulative_oil_co2
    ## year                                              0.130
    ## population                                        0.707
    ## cement_co2                                        0.753
    ## cement_co2_per_capita                             0.156
    ## co2                                               0.771
    ## co2_growth_abs                                    0.307
    ## co2_growth_prct                                  -0.002
    ## co2_per_capita                                    0.058
    ## cumulative_cement_co2                             0.893
    ## cumulative_co2                                    0.979
    ## cumulative_luc_co2                                0.755
    ## cumulative_oil_co2                                1.000
    ## ghg_excluding_lucf_per_capita                     0.100
    ## ghg_per_capita                                    0.045
    ## land_use_change_co2                               0.362
    ## land_use_change_co2_per_capita                   -0.032
    ## methane                                           0.758
    ## methane_per_capita                                0.006
    ## nitrous_oxide                                     0.796
    ## nitrous_oxide_per_capita                          0.024
    ## oil_co2                                           0.928
    ## share_global_co2                                  0.290
    ## share_global_cumulative_co2                       0.306
    ## share_global_cumulative_luc_co2                   0.459
    ## share_global_luc_co2                              0.343
    ## share_of_temperature_change_from_ghg              0.480
    ## temperature_change_from_ch4                       0.830
    ## temperature_change_from_co2                       0.879
    ## temperature_change_from_ghg                       0.876
    ## temperature_change_from_n2o                       0.882
    ## total_ghg                                         0.813
    ## total_ghg_excluding_lucf                          0.857
    ##                                      ghg_excluding_lucf_per_capita
    ## year                                                         0.249
    ## population                                                   0.036
    ## cement_co2                                                   0.056
    ## cement_co2_per_capita                                        0.454
    ## co2                                                          0.077
    ## co2_growth_abs                                               0.050
    ## co2_growth_prct                                              0.008
    ## co2_per_capita                                               0.386
    ## cumulative_cement_co2                                        0.070
    ## cumulative_co2                                               0.109
    ## cumulative_luc_co2                                           0.084
    ## cumulative_oil_co2                                           0.100
    ## ghg_excluding_lucf_per_capita                                1.000
    ## ghg_per_capita                                               0.673
    ## land_use_change_co2                                          0.022
    ## land_use_change_co2_per_capita                              -0.043
    ## methane                                                      0.064
    ## methane_per_capita                                           0.752
    ## nitrous_oxide                                                0.071
    ## nitrous_oxide_per_capita                                     0.148
    ## oil_co2                                                      0.109
    ## share_global_co2                                             0.037
    ## share_global_cumulative_co2                                  0.042
    ## share_global_cumulative_luc_co2                              0.056
    ## share_global_luc_co2                                         0.017
    ## share_of_temperature_change_from_ghg                         0.061
    ## temperature_change_from_ch4                                  0.074
    ## temperature_change_from_co2                                  0.092
    ## temperature_change_from_ghg                                  0.087
    ## temperature_change_from_n2o                                  0.075
    ## total_ghg                                                    0.081
    ## total_ghg_excluding_lucf                                     0.095
    ##                                      ghg_per_capita land_use_change_co2
    ## year                                          0.132               0.068
    ## population                                   -0.006               0.646
    ## cement_co2                                    0.015               0.393
    ## cement_co2_per_capita                         0.252               0.026
    ## co2                                           0.028               0.434
    ## co2_growth_abs                                0.024               0.376
    ## co2_growth_prct                               0.015              -0.003
    ## co2_per_capita                                0.250               0.003
    ## cumulative_cement_co2                         0.023               0.345
    ## cumulative_co2                                0.050               0.418
    ## cumulative_luc_co2                            0.050               0.802
    ## cumulative_oil_co2                            0.045               0.362
    ## ghg_excluding_lucf_per_capita                 0.673               0.022
    ## ghg_per_capita                                1.000               0.045
    ## land_use_change_co2                           0.045               1.000
    ## land_use_change_co2_per_capita                0.570               0.068
    ## methane                                       0.021               0.735
    ## methane_per_capita                            0.691              -0.001
    ## nitrous_oxide                                 0.030               0.638
    ## nitrous_oxide_per_capita                      0.500               0.000
    ## oil_co2                                       0.051               0.475
    ## share_global_co2                              0.017               0.471
    ## share_global_cumulative_co2                   0.015               0.435
    ## share_global_cumulative_luc_co2               0.059               0.906
    ## share_global_luc_co2                          0.048               0.968
    ## share_of_temperature_change_from_ghg          0.051               0.793
    ## temperature_change_from_ch4                   0.029               0.633
    ## temperature_change_from_co2                   0.045               0.556
    ## temperature_change_from_ghg                   0.041               0.581
    ## temperature_change_from_n2o                   0.030               0.536
    ## total_ghg                                     0.040               0.674
    ## total_ghg_excluding_lucf                      0.040               0.521
    ##                                      land_use_change_co2_per_capita methane
    ## year                                                         -0.028   0.133
    ## population                                                   -0.033   0.898
    ## cement_co2                                                   -0.029   0.793
    ## cement_co2_per_capita                                        -0.093   0.128
    ## co2                                                          -0.040   0.755
    ## co2_growth_abs                                               -0.011   0.507
    ## co2_growth_prct                                               0.017  -0.003
    ## co2_per_capita                                               -0.037   0.032
    ## cumulative_cement_co2                                        -0.033   0.775
    ## cumulative_co2                                               -0.036   0.800
    ## cumulative_luc_co2                                           -0.002   0.925
    ## cumulative_oil_co2                                           -0.032   0.758
    ## ghg_excluding_lucf_per_capita                                -0.043   0.064
    ## ghg_per_capita                                                0.570   0.021
    ## land_use_change_co2                                           0.068   0.735
    ## land_use_change_co2_per_capita                                1.000  -0.029
    ## methane                                                      -0.029   1.000
    ## methane_per_capita                                            0.088   0.003
    ## nitrous_oxide                                                -0.029   0.978
    ## nitrous_oxide_per_capita                                      0.206   0.006
    ## oil_co2                                                      -0.034   0.861
    ## share_global_co2                                             -0.023   0.437
    ## share_global_cumulative_co2                                  -0.029   0.415
    ## share_global_cumulative_luc_co2                               0.051   0.742
    ## share_global_luc_co2                                          0.078   0.698
    ## share_of_temperature_change_from_ghg                          0.015   0.776
    ## temperature_change_from_ch4                                  -0.030   0.979
    ## temperature_change_from_co2                                  -0.027   0.911
    ## temperature_change_from_ghg                                  -0.028   0.939
    ## temperature_change_from_n2o                                  -0.033   0.923
    ## total_ghg                                                    -0.021   0.981
    ## total_ghg_excluding_lucf                                     -0.035   0.933
    ##                                      methane_per_capita nitrous_oxide
    ## year                                              0.115         0.148
    ## population                                       -0.013         0.857
    ## cement_co2                                       -0.002         0.788
    ## cement_co2_per_capita                             0.179         0.143
    ## co2                                              -0.002         0.766
    ## co2_growth_abs                                    0.003         0.486
    ## co2_growth_prct                                   0.001        -0.003
    ## co2_per_capita                                    0.193         0.039
    ## cumulative_cement_co2                            -0.001         0.789
    ## cumulative_co2                                    0.007         0.834
    ## cumulative_luc_co2                                0.006         0.892
    ## cumulative_oil_co2                                0.006         0.796
    ## ghg_excluding_lucf_per_capita                     0.752         0.071
    ## ghg_per_capita                                    0.691         0.030
    ## land_use_change_co2                              -0.001         0.638
    ## land_use_change_co2_per_capita                    0.088        -0.029
    ## methane                                           0.003         0.978
    ## methane_per_capita                                1.000         0.003
    ## nitrous_oxide                                     0.003         1.000
    ## nitrous_oxide_per_capita                          0.306         0.028
    ## oil_co2                                           0.009         0.896
    ## share_global_co2                                 -0.009         0.394
    ## share_global_cumulative_co2                      -0.009         0.379
    ## share_global_cumulative_luc_co2                   0.002         0.656
    ## share_global_luc_co2                             -0.002         0.595
    ## share_of_temperature_change_from_ghg              0.002         0.716
    ## temperature_change_from_ch4                       0.006         0.987
    ## temperature_change_from_co2                       0.004         0.944
    ## temperature_change_from_ghg                       0.005         0.966
    ## temperature_change_from_n2o                       0.002         0.960
    ## total_ghg                                         0.003         0.982
    ## total_ghg_excluding_lucf                          0.005         0.962
    ##                                      nitrous_oxide_per_capita oil_co2
    ## year                                                    0.146   0.136
    ## population                                             -0.012   0.794
    ## cement_co2                                              0.005   0.790
    ## cement_co2_per_capita                                   0.124   0.179
    ## co2                                                     0.008   0.818
    ## co2_growth_abs                                          0.005   0.444
    ## co2_growth_prct                                        -0.001  -0.002
    ## co2_per_capita                                          0.057   0.063
    ## cumulative_cement_co2                                   0.011   0.848
    ## cumulative_co2                                          0.025   0.941
    ## cumulative_luc_co2                                      0.019   0.850
    ## cumulative_oil_co2                                      0.024   0.928
    ## ghg_excluding_lucf_per_capita                           0.148   0.109
    ## ghg_per_capita                                          0.500   0.051
    ## land_use_change_co2                                     0.000   0.475
    ## land_use_change_co2_per_capita                          0.206  -0.034
    ## methane                                                 0.006   0.861
    ## methane_per_capita                                      0.306   0.009
    ## nitrous_oxide                                           0.028   0.896
    ## nitrous_oxide_per_capita                                1.000   0.028
    ## oil_co2                                                 0.028   1.000
    ## share_global_co2                                       -0.017   0.354
    ## share_global_cumulative_co2                            -0.016   0.357
    ## share_global_cumulative_luc_co2                         0.003   0.553
    ## share_global_luc_co2                                   -0.004   0.438
    ## share_of_temperature_change_from_ghg                    0.007   0.565
    ## temperature_change_from_ch4                             0.015   0.905
    ## temperature_change_from_co2                             0.020   0.894
    ## temperature_change_from_ghg                             0.019   0.906
    ## temperature_change_from_n2o                             0.023   0.895
    ## total_ghg                                               0.014   0.894
    ## total_ghg_excluding_lucf                                0.016   0.917
    ##                                      share_global_co2
    ## year                                           -0.131
    ## population                                      0.341
    ## cement_co2                                      0.271
    ## cement_co2_per_capita                           0.023
    ## co2                                             0.331
    ## co2_growth_abs                                  0.232
    ## co2_growth_prct                                -0.003
    ## co2_per_capita                                  0.019
    ## cumulative_cement_co2                           0.267
    ## cumulative_co2                                  0.358
    ## cumulative_luc_co2                              0.452
    ## cumulative_oil_co2                              0.290
    ## ghg_excluding_lucf_per_capita                   0.037
    ## ghg_per_capita                                  0.017
    ## land_use_change_co2                             0.471
    ## land_use_change_co2_per_capita                 -0.023
    ## methane                                         0.437
    ## methane_per_capita                             -0.009
    ## nitrous_oxide                                   0.394
    ## nitrous_oxide_per_capita                       -0.017
    ## oil_co2                                         0.354
    ## share_global_co2                                1.000
    ## share_global_cumulative_co2                     0.990
    ## share_global_cumulative_luc_co2                 0.571
    ## share_global_luc_co2                            0.502
    ## share_of_temperature_change_from_ghg            0.567
    ## temperature_change_from_ch4                     0.379
    ## temperature_change_from_co2                     0.383
    ## temperature_change_from_ghg                     0.384
    ## temperature_change_from_n2o                     0.350
    ## total_ghg                                       0.434
    ## total_ghg_excluding_lucf                        0.384
    ##                                      share_global_cumulative_co2
    ## year                                                      -0.124
    ## population                                                 0.304
    ## cement_co2                                                 0.236
    ## cement_co2_per_capita                                      0.024
    ## co2                                                        0.325
    ## co2_growth_abs                                             0.198
    ## co2_growth_prct                                           -0.003
    ## co2_per_capita                                             0.023
    ## cumulative_cement_co2                                      0.256
    ## cumulative_co2                                             0.379
    ## cumulative_luc_co2                                         0.433
    ## cumulative_oil_co2                                         0.306
    ## ghg_excluding_lucf_per_capita                              0.042
    ## ghg_per_capita                                             0.015
    ## land_use_change_co2                                        0.435
    ## land_use_change_co2_per_capita                            -0.029
    ## methane                                                    0.415
    ## methane_per_capita                                        -0.009
    ## nitrous_oxide                                              0.379
    ## nitrous_oxide_per_capita                                  -0.016
    ## oil_co2                                                    0.357
    ## share_global_co2                                           0.990
    ## share_global_cumulative_co2                                1.000
    ## share_global_cumulative_luc_co2                            0.550
    ## share_global_luc_co2                                       0.467
    ## share_of_temperature_change_from_ghg                       0.560
    ## temperature_change_from_ch4                                0.362
    ## temperature_change_from_co2                                0.387
    ## temperature_change_from_ghg                                0.383
    ## temperature_change_from_n2o                                0.349
    ## total_ghg                                                  0.418
    ## total_ghg_excluding_lucf                                   0.375
    ##                                      share_global_cumulative_luc_co2
    ## year                                                           0.052
    ## population                                                     0.602
    ## cement_co2                                                     0.426
    ## cement_co2_per_capita                                          0.055
    ## co2                                                            0.503
    ## co2_growth_abs                                                 0.358
    ## co2_growth_prct                                               -0.003
    ## co2_per_capita                                                 0.027
    ## cumulative_cement_co2                                          0.417
    ## cumulative_co2                                                 0.533
    ## cumulative_luc_co2                                             0.810
    ## cumulative_oil_co2                                             0.459
    ## ghg_excluding_lucf_per_capita                                  0.056
    ## ghg_per_capita                                                 0.059
    ## land_use_change_co2                                            0.906
    ## land_use_change_co2_per_capita                                 0.051
    ## methane                                                        0.742
    ## methane_per_capita                                             0.002
    ## nitrous_oxide                                                  0.656
    ## nitrous_oxide_per_capita                                       0.003
    ## oil_co2                                                        0.553
    ## share_global_co2                                               0.571
    ## share_global_cumulative_co2                                    0.550
    ## share_global_cumulative_luc_co2                                1.000
    ## share_global_luc_co2                                           0.944
    ## share_of_temperature_change_from_ghg                           0.905
    ## temperature_change_from_ch4                                    0.643
    ## temperature_change_from_co2                                    0.623
    ## temperature_change_from_ghg                                    0.632
    ## temperature_change_from_n2o                                    0.580
    ## total_ghg                                                      0.711
    ## total_ghg_excluding_lucf                                       0.592
    ##                                      share_global_luc_co2
    ## year                                                0.042
    ## population                                          0.603
    ## cement_co2                                          0.376
    ## cement_co2_per_capita                               0.016
    ## co2                                                 0.406
    ## co2_growth_abs                                      0.339
    ## co2_growth_prct                                    -0.003
    ## co2_per_capita                                      0.000
    ## cumulative_cement_co2                               0.337
    ## cumulative_co2                                      0.394
    ## cumulative_luc_co2                                  0.743
    ## cumulative_oil_co2                                  0.343
    ## ghg_excluding_lucf_per_capita                       0.017
    ## ghg_per_capita                                      0.048
    ## land_use_change_co2                                 0.968
    ## land_use_change_co2_per_capita                      0.078
    ## methane                                             0.698
    ## methane_per_capita                                 -0.002
    ## nitrous_oxide                                       0.595
    ## nitrous_oxide_per_capita                           -0.004
    ## oil_co2                                             0.438
    ## share_global_co2                                    0.502
    ## share_global_cumulative_co2                         0.467
    ## share_global_cumulative_luc_co2                     0.944
    ## share_global_luc_co2                                1.000
    ## share_of_temperature_change_from_ghg                0.826
    ## temperature_change_from_ch4                         0.587
    ## temperature_change_from_co2                         0.518
    ## temperature_change_from_ghg                         0.540
    ## temperature_change_from_n2o                         0.500
    ## total_ghg                                           0.639
    ## total_ghg_excluding_lucf                            0.489
    ##                                      share_of_temperature_change_from_ghg
    ## year                                                                0.053
    ## population                                                          0.547
    ## cement_co2                                                          0.412
    ## cement_co2_per_capita                                               0.058
    ## co2                                                                 0.508
    ## co2_growth_abs                                                      0.333
    ## co2_growth_prct                                                    -0.003
    ## co2_per_capita                                                      0.031
    ## cumulative_cement_co2                                               0.424
    ## cumulative_co2                                                      0.563
    ## cumulative_luc_co2                                                  0.744
    ## cumulative_oil_co2                                                  0.480
    ## ghg_excluding_lucf_per_capita                                       0.061
    ## ghg_per_capita                                                      0.051
    ## land_use_change_co2                                                 0.793
    ## land_use_change_co2_per_capita                                      0.015
    ## methane                                                             0.776
    ## methane_per_capita                                                  0.002
    ## nitrous_oxide                                                       0.716
    ## nitrous_oxide_per_capita                                            0.007
    ## oil_co2                                                             0.565
    ## share_global_co2                                                    0.567
    ## share_global_cumulative_co2                                         0.560
    ## share_global_cumulative_luc_co2                                     0.905
    ## share_global_luc_co2                                                0.826
    ## share_of_temperature_change_from_ghg                                1.000
    ## temperature_change_from_ch4                                         0.682
    ## temperature_change_from_co2                                         0.709
    ## temperature_change_from_ghg                                         0.707
    ## temperature_change_from_n2o                                         0.653
    ## total_ghg                                                           0.773
    ## total_ghg_excluding_lucf                                            0.675
    ##                                      temperature_change_from_ch4
    ## year                                                       0.150
    ## population                                                 0.888
    ## cement_co2                                                 0.830
    ## cement_co2_per_capita                                      0.143
    ## co2                                                        0.784
    ## co2_growth_abs                                             0.481
    ## co2_growth_prct                                           -0.003
    ## co2_per_capita                                             0.039
    ## cumulative_cement_co2                                      0.837
    ## cumulative_co2                                             0.855
    ## cumulative_luc_co2                                         0.908
    ## cumulative_oil_co2                                         0.830
    ## ghg_excluding_lucf_per_capita                              0.074
    ## ghg_per_capita                                             0.029
    ## land_use_change_co2                                        0.633
    ## land_use_change_co2_per_capita                            -0.030
    ## methane                                                    0.979
    ## methane_per_capita                                         0.006
    ## nitrous_oxide                                              0.987
    ## nitrous_oxide_per_capita                                   0.015
    ## oil_co2                                                    0.905
    ## share_global_co2                                           0.379
    ## share_global_cumulative_co2                                0.362
    ## share_global_cumulative_luc_co2                            0.643
    ## share_global_luc_co2                                       0.587
    ## share_of_temperature_change_from_ghg                       0.682
    ## temperature_change_from_ch4                                1.000
    ## temperature_change_from_co2                                0.948
    ## temperature_change_from_ghg                                0.972
    ## temperature_change_from_n2o                                0.966
    ## total_ghg                                                  0.982
    ## total_ghg_excluding_lucf                                   0.966
    ##                                      temperature_change_from_co2
    ## year                                                       0.147
    ## population                                                 0.750
    ## cement_co2                                                 0.731
    ## cement_co2_per_capita                                      0.143
    ## co2                                                        0.761
    ## co2_growth_abs                                             0.388
    ## co2_growth_prct                                           -0.003
    ## co2_per_capita                                             0.053
    ## cumulative_cement_co2                                      0.809
    ## cumulative_co2                                             0.908
    ## cumulative_luc_co2                                         0.860
    ## cumulative_oil_co2                                         0.879
    ## ghg_excluding_lucf_per_capita                              0.092
    ## ghg_per_capita                                             0.045
    ## land_use_change_co2                                        0.556
    ## land_use_change_co2_per_capita                            -0.027
    ## methane                                                    0.911
    ## methane_per_capita                                         0.004
    ## nitrous_oxide                                              0.944
    ## nitrous_oxide_per_capita                                   0.020
    ## oil_co2                                                    0.894
    ## share_global_co2                                           0.383
    ## share_global_cumulative_co2                                0.387
    ## share_global_cumulative_luc_co2                            0.623
    ## share_global_luc_co2                                       0.518
    ## share_of_temperature_change_from_ghg                       0.709
    ## temperature_change_from_ch4                                0.948
    ## temperature_change_from_co2                                1.000
    ## temperature_change_from_ghg                                0.996
    ## temperature_change_from_n2o                                0.981
    ## total_ghg                                                  0.959
    ## total_ghg_excluding_lucf                                   0.967
    ##                                      temperature_change_from_ghg
    ## year                                                       0.150
    ## population                                                 0.796
    ## cement_co2                                                 0.767
    ## cement_co2_per_capita                                      0.144
    ## co2                                                        0.775
    ## co2_growth_abs                                             0.417
    ## co2_growth_prct                                           -0.003
    ## co2_per_capita                                             0.050
    ## cumulative_cement_co2                                      0.827
    ## cumulative_co2                                             0.904
    ## cumulative_luc_co2                                         0.881
    ## cumulative_oil_co2                                         0.876
    ## ghg_excluding_lucf_per_capita                              0.087
    ## ghg_per_capita                                             0.041
    ## land_use_change_co2                                        0.581
    ## land_use_change_co2_per_capita                            -0.028
    ## methane                                                    0.939
    ## methane_per_capita                                         0.005
    ## nitrous_oxide                                              0.966
    ## nitrous_oxide_per_capita                                   0.019
    ## oil_co2                                                    0.906
    ## share_global_co2                                           0.384
    ## share_global_cumulative_co2                                0.383
    ## share_global_cumulative_luc_co2                            0.632
    ## share_global_luc_co2                                       0.540
    ## share_of_temperature_change_from_ghg                       0.707
    ## temperature_change_from_ch4                                0.972
    ## temperature_change_from_co2                                0.996
    ## temperature_change_from_ghg                                1.000
    ## temperature_change_from_n2o                                0.988
    ## total_ghg                                                  0.975
    ## total_ghg_excluding_lucf                                   0.977
    ##                                      temperature_change_from_n2o total_ghg
    ## year                                                       0.148     0.136
    ## population                                                 0.807     0.847
    ## cement_co2                                                 0.780     0.812
    ## cement_co2_per_capita                                      0.138     0.146
    ## co2                                                        0.766     0.783
    ## co2_growth_abs                                             0.402     0.492
    ## co2_growth_prct                                           -0.003    -0.003
    ## co2_per_capita                                             0.044     0.044
    ## cumulative_cement_co2                                      0.848     0.816
    ## cumulative_co2                                             0.901     0.853
    ## cumulative_luc_co2                                         0.844     0.908
    ## cumulative_oil_co2                                         0.882     0.813
    ## ghg_excluding_lucf_per_capita                              0.075     0.081
    ## ghg_per_capita                                             0.030     0.040
    ## land_use_change_co2                                        0.536     0.674
    ## land_use_change_co2_per_capita                            -0.033    -0.021
    ## methane                                                    0.923     0.981
    ## methane_per_capita                                         0.002     0.003
    ## nitrous_oxide                                              0.960     0.982
    ## nitrous_oxide_per_capita                                   0.023     0.014
    ## oil_co2                                                    0.895     0.894
    ## share_global_co2                                           0.350     0.434
    ## share_global_cumulative_co2                                0.349     0.418
    ## share_global_cumulative_luc_co2                            0.580     0.711
    ## share_global_luc_co2                                       0.500     0.639
    ## share_of_temperature_change_from_ghg                       0.653     0.773
    ## temperature_change_from_ch4                                0.966     0.982
    ## temperature_change_from_co2                                0.981     0.959
    ## temperature_change_from_ghg                                0.988     0.975
    ## temperature_change_from_n2o                                1.000     0.955
    ## total_ghg                                                  0.955     1.000
    ## total_ghg_excluding_lucf                                   0.963     0.978
    ##                                      total_ghg_excluding_lucf
    ## year                                                    0.140
    ## population                                              0.807
    ## cement_co2                                              0.843
    ## cement_co2_per_capita                                   0.168
    ## co2                                                     0.799
    ## co2_growth_abs                                          0.471
    ## co2_growth_prct                                        -0.003
    ## co2_per_capita                                          0.054
    ## cumulative_cement_co2                                   0.862
    ## cumulative_co2                                          0.889
    ## cumulative_luc_co2                                      0.846
    ## cumulative_oil_co2                                      0.857
    ## ghg_excluding_lucf_per_capita                           0.095
    ## ghg_per_capita                                          0.040
    ## land_use_change_co2                                     0.521
    ## land_use_change_co2_per_capita                         -0.035
    ## methane                                                 0.933
    ## methane_per_capita                                      0.005
    ## nitrous_oxide                                           0.962
    ## nitrous_oxide_per_capita                                0.016
    ## oil_co2                                                 0.917
    ## share_global_co2                                        0.384
    ## share_global_cumulative_co2                             0.375
    ## share_global_cumulative_luc_co2                         0.592
    ## share_global_luc_co2                                    0.489
    ## share_of_temperature_change_from_ghg                    0.675
    ## temperature_change_from_ch4                             0.966
    ## temperature_change_from_co2                             0.967
    ## temperature_change_from_ghg                             0.977
    ## temperature_change_from_n2o                             0.963
    ## total_ghg                                               0.978
    ## total_ghg_excluding_lucf                                1.000

![](r-notebook_files/figure-gfm/unnamed-chunk-9-54.png)<!-- -->

    ## 
    ## Strong correlations (|r| > 0.7) in Dataset 2 :
    ## cement_co2 - population: 0.824

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-55.png)<!-- -->

    ## co2 - population: 0.703

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-56.png)<!-- -->

    ## cumulative_cement_co2 - population: 0.779

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-57.png)<!-- -->

    ## cumulative_co2 - population: 0.716

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-58.png)<!-- -->

    ## cumulative_luc_co2 - population: 0.832

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-59.png)<!-- -->

    ## cumulative_oil_co2 - population: 0.707

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-60.png)<!-- -->

    ## methane - population: 0.898

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-61.png)<!-- -->

    ## nitrous_oxide - population: 0.857

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-62.png)<!-- -->

    ## oil_co2 - population: 0.794

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-63.png)<!-- -->

    ## temperature_change_from_ch4 - population: 0.888

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-64.png)<!-- -->

    ## temperature_change_from_co2 - population: 0.750

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-65.png)<!-- -->

    ## temperature_change_from_ghg - population: 0.796

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-66.png)<!-- -->

    ## temperature_change_from_n2o - population: 0.807

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-67.png)<!-- -->

    ## total_ghg - population: 0.847

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-68.png)<!-- -->

    ## total_ghg_excluding_lucf - population: 0.807

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-69.png)<!-- -->

    ## population - cement_co2: 0.824

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-70.png)<!-- -->

    ## co2 - cement_co2: 0.751

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-71.png)<!-- -->

    ## cumulative_cement_co2 - cement_co2: 0.941

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-72.png)<!-- -->

    ## cumulative_co2 - cement_co2: 0.764

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-73.png)<!-- -->

    ## cumulative_luc_co2 - cement_co2: 0.708

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-74.png)<!-- -->

    ## cumulative_oil_co2 - cement_co2: 0.753

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-75.png)<!-- -->

    ## methane - cement_co2: 0.793

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-76.png)<!-- -->

    ## nitrous_oxide - cement_co2: 0.788

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-77.png)<!-- -->

    ## oil_co2 - cement_co2: 0.790

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-78.png)<!-- -->

    ## temperature_change_from_ch4 - cement_co2: 0.830

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-79.png)<!-- -->

    ## temperature_change_from_co2 - cement_co2: 0.731

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-80.png)<!-- -->

    ## temperature_change_from_ghg - cement_co2: 0.767

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-81.png)<!-- -->

    ## temperature_change_from_n2o - cement_co2: 0.780

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-82.png)<!-- -->

    ## total_ghg - cement_co2: 0.812

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-83.png)<!-- -->

    ## total_ghg_excluding_lucf - cement_co2: 0.843

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-84.png)<!-- -->

    ## population - co2: 0.703

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-85.png)<!-- -->

    ## cement_co2 - co2: 0.751

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-86.png)<!-- -->

    ## cumulative_cement_co2 - co2: 0.773

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-87.png)<!-- -->

    ## cumulative_co2 - co2: 0.797

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-88.png)<!-- -->

    ## cumulative_luc_co2 - co2: 0.737

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-89.png)<!-- -->

    ## cumulative_oil_co2 - co2: 0.771

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-90.png)<!-- -->

    ## methane - co2: 0.755

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-91.png)<!-- -->

    ## nitrous_oxide - co2: 0.766

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-92.png)<!-- -->

    ## oil_co2 - co2: 0.818

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-93.png)<!-- -->

    ## temperature_change_from_ch4 - co2: 0.784

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-94.png)<!-- -->

    ## temperature_change_from_co2 - co2: 0.761

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-95.png)<!-- -->

    ## temperature_change_from_ghg - co2: 0.775

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-96.png)<!-- -->

    ## temperature_change_from_n2o - co2: 0.766

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-97.png)<!-- -->

    ## total_ghg - co2: 0.783

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-98.png)<!-- -->

    ## total_ghg_excluding_lucf - co2: 0.799

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-99.png)<!-- -->

    ## population - cumulative_cement_co2: 0.779

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-100.png)<!-- -->

    ## cement_co2 - cumulative_cement_co2: 0.941

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-101.png)<!-- -->

    ## co2 - cumulative_cement_co2: 0.773

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-102.png)<!-- -->

    ## cumulative_co2 - cumulative_cement_co2: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-103.png)<!-- -->

    ## cumulative_luc_co2 - cumulative_cement_co2: 0.711

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-104.png)<!-- -->

    ## cumulative_oil_co2 - cumulative_cement_co2: 0.893

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-105.png)<!-- -->

    ## methane - cumulative_cement_co2: 0.775

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-106.png)<!-- -->

    ## nitrous_oxide - cumulative_cement_co2: 0.789

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-107.png)<!-- -->

    ## oil_co2 - cumulative_cement_co2: 0.848

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-108.png)<!-- -->

    ## temperature_change_from_ch4 - cumulative_cement_co2: 0.837

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-109.png)<!-- -->

    ## temperature_change_from_co2 - cumulative_cement_co2: 0.809

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-110.png)<!-- -->

    ## temperature_change_from_ghg - cumulative_cement_co2: 0.827

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-111.png)<!-- -->

    ## temperature_change_from_n2o - cumulative_cement_co2: 0.848

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-112.png)<!-- -->

    ## total_ghg - cumulative_cement_co2: 0.816

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-113.png)<!-- -->

    ## total_ghg_excluding_lucf - cumulative_cement_co2: 0.862

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-114.png)<!-- -->

    ## population - cumulative_co2: 0.716

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-115.png)<!-- -->

    ## cement_co2 - cumulative_co2: 0.764

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-116.png)<!-- -->

    ## co2 - cumulative_co2: 0.797

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-117.png)<!-- -->

    ## cumulative_cement_co2 - cumulative_co2: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-118.png)<!-- -->

    ## cumulative_luc_co2 - cumulative_co2: 0.803

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-119.png)<!-- -->

    ## cumulative_oil_co2 - cumulative_co2: 0.979

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-120.png)<!-- -->

    ## methane - cumulative_co2: 0.800

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-121.png)<!-- -->

    ## nitrous_oxide - cumulative_co2: 0.834

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-122.png)<!-- -->

    ## oil_co2 - cumulative_co2: 0.941

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-123.png)<!-- -->

    ## temperature_change_from_ch4 - cumulative_co2: 0.855

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-124.png)<!-- -->

    ## temperature_change_from_co2 - cumulative_co2: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-125.png)<!-- -->

    ## temperature_change_from_ghg - cumulative_co2: 0.904

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-126.png)<!-- -->

    ## temperature_change_from_n2o - cumulative_co2: 0.901

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-127.png)<!-- -->

    ## total_ghg - cumulative_co2: 0.853

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-128.png)<!-- -->

    ## total_ghg_excluding_lucf - cumulative_co2: 0.889

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-129.png)<!-- -->

    ## population - cumulative_luc_co2: 0.832

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-130.png)<!-- -->

    ## cement_co2 - cumulative_luc_co2: 0.708

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-131.png)<!-- -->

    ## co2 - cumulative_luc_co2: 0.737

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-132.png)<!-- -->

    ## cumulative_cement_co2 - cumulative_luc_co2: 0.711

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-133.png)<!-- -->

    ## cumulative_co2 - cumulative_luc_co2: 0.803

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-134.png)<!-- -->

    ## cumulative_oil_co2 - cumulative_luc_co2: 0.755

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-135.png)<!-- -->

    ## land_use_change_co2 - cumulative_luc_co2: 0.802

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-136.png)<!-- -->

    ## methane - cumulative_luc_co2: 0.925

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-137.png)<!-- -->

    ## nitrous_oxide - cumulative_luc_co2: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-138.png)<!-- -->

    ## oil_co2 - cumulative_luc_co2: 0.850

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-139.png)<!-- -->

    ## share_global_cumulative_luc_co2 - cumulative_luc_co2: 0.810

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-140.png)<!-- -->

    ## share_global_luc_co2 - cumulative_luc_co2: 0.743

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-141.png)<!-- -->

    ## share_of_temperature_change_from_ghg - cumulative_luc_co2: 0.744

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-142.png)<!-- -->

    ## temperature_change_from_ch4 - cumulative_luc_co2: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-143.png)<!-- -->

    ## temperature_change_from_co2 - cumulative_luc_co2: 0.860

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-144.png)<!-- -->

    ## temperature_change_from_ghg - cumulative_luc_co2: 0.881

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-145.png)<!-- -->

    ## temperature_change_from_n2o - cumulative_luc_co2: 0.844

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-146.png)<!-- -->

    ## total_ghg - cumulative_luc_co2: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-147.png)<!-- -->

    ## total_ghg_excluding_lucf - cumulative_luc_co2: 0.846

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-148.png)<!-- -->

    ## population - cumulative_oil_co2: 0.707

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-149.png)<!-- -->

    ## cement_co2 - cumulative_oil_co2: 0.753

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-150.png)<!-- -->

    ## co2 - cumulative_oil_co2: 0.771

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-151.png)<!-- -->

    ## cumulative_cement_co2 - cumulative_oil_co2: 0.893

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-152.png)<!-- -->

    ## cumulative_co2 - cumulative_oil_co2: 0.979

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-153.png)<!-- -->

    ## cumulative_luc_co2 - cumulative_oil_co2: 0.755

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-154.png)<!-- -->

    ## methane - cumulative_oil_co2: 0.758

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-155.png)<!-- -->

    ## nitrous_oxide - cumulative_oil_co2: 0.796

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-156.png)<!-- -->

    ## oil_co2 - cumulative_oil_co2: 0.928

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-157.png)<!-- -->

    ## temperature_change_from_ch4 - cumulative_oil_co2: 0.830

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-158.png)<!-- -->

    ## temperature_change_from_co2 - cumulative_oil_co2: 0.879

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-159.png)<!-- -->

    ## temperature_change_from_ghg - cumulative_oil_co2: 0.876

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-160.png)<!-- -->

    ## temperature_change_from_n2o - cumulative_oil_co2: 0.882

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-161.png)<!-- -->

    ## total_ghg - cumulative_oil_co2: 0.813

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-162.png)<!-- -->

    ## total_ghg_excluding_lucf - cumulative_oil_co2: 0.857

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-163.png)<!-- -->

    ## methane_per_capita - ghg_excluding_lucf_per_capita: 0.752

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-164.png)<!-- -->

    ## cumulative_luc_co2 - land_use_change_co2: 0.802

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-165.png)<!-- -->

    ## methane - land_use_change_co2: 0.735

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-166.png)<!-- -->

    ## share_global_cumulative_luc_co2 - land_use_change_co2: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-167.png)<!-- -->

    ## share_global_luc_co2 - land_use_change_co2: 0.968

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-168.png)<!-- -->

    ## share_of_temperature_change_from_ghg - land_use_change_co2: 0.793

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-169.png)<!-- -->

    ## population - methane: 0.898

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-170.png)<!-- -->

    ## cement_co2 - methane: 0.793

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-171.png)<!-- -->

    ## co2 - methane: 0.755

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-172.png)<!-- -->

    ## cumulative_cement_co2 - methane: 0.775

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-173.png)<!-- -->

    ## cumulative_co2 - methane: 0.800

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-174.png)<!-- -->

    ## cumulative_luc_co2 - methane: 0.925

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-175.png)<!-- -->

    ## cumulative_oil_co2 - methane: 0.758

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-176.png)<!-- -->

    ## land_use_change_co2 - methane: 0.735

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-177.png)<!-- -->

    ## nitrous_oxide - methane: 0.978

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-178.png)<!-- -->

    ## oil_co2 - methane: 0.861

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-179.png)<!-- -->

    ## share_global_cumulative_luc_co2 - methane: 0.742

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-180.png)<!-- -->

    ## share_of_temperature_change_from_ghg - methane: 0.776

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-181.png)<!-- -->

    ## temperature_change_from_ch4 - methane: 0.979

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-182.png)<!-- -->

    ## temperature_change_from_co2 - methane: 0.911

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-183.png)<!-- -->

    ## temperature_change_from_ghg - methane: 0.939

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-184.png)<!-- -->

    ## temperature_change_from_n2o - methane: 0.923

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-185.png)<!-- -->

    ## total_ghg - methane: 0.981

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-186.png)<!-- -->

    ## total_ghg_excluding_lucf - methane: 0.933

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-187.png)<!-- -->

    ## ghg_excluding_lucf_per_capita - methane_per_capita: 0.752

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-188.png)<!-- -->

    ## population - nitrous_oxide: 0.857

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-189.png)<!-- -->

    ## cement_co2 - nitrous_oxide: 0.788

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-190.png)<!-- -->

    ## co2 - nitrous_oxide: 0.766

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-191.png)<!-- -->

    ## cumulative_cement_co2 - nitrous_oxide: 0.789

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-192.png)<!-- -->

    ## cumulative_co2 - nitrous_oxide: 0.834

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-193.png)<!-- -->

    ## cumulative_luc_co2 - nitrous_oxide: 0.892

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-194.png)<!-- -->

    ## cumulative_oil_co2 - nitrous_oxide: 0.796

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-195.png)<!-- -->

    ## methane - nitrous_oxide: 0.978

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-196.png)<!-- -->

    ## oil_co2 - nitrous_oxide: 0.896

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-197.png)<!-- -->

    ## share_of_temperature_change_from_ghg - nitrous_oxide: 0.716

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-198.png)<!-- -->

    ## temperature_change_from_ch4 - nitrous_oxide: 0.987

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-199.png)<!-- -->

    ## temperature_change_from_co2 - nitrous_oxide: 0.944

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-200.png)<!-- -->

    ## temperature_change_from_ghg - nitrous_oxide: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-201.png)<!-- -->

    ## temperature_change_from_n2o - nitrous_oxide: 0.960

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-202.png)<!-- -->

    ## total_ghg - nitrous_oxide: 0.982

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-203.png)<!-- -->

    ## total_ghg_excluding_lucf - nitrous_oxide: 0.962

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-204.png)<!-- -->

    ## population - oil_co2: 0.794

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-205.png)<!-- -->

    ## cement_co2 - oil_co2: 0.790

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-206.png)<!-- -->

    ## co2 - oil_co2: 0.818

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-207.png)<!-- -->

    ## cumulative_cement_co2 - oil_co2: 0.848

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-208.png)<!-- -->

    ## cumulative_co2 - oil_co2: 0.941

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-209.png)<!-- -->

    ## cumulative_luc_co2 - oil_co2: 0.850

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-210.png)<!-- -->

    ## cumulative_oil_co2 - oil_co2: 0.928

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-211.png)<!-- -->

    ## methane - oil_co2: 0.861

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-212.png)<!-- -->

    ## nitrous_oxide - oil_co2: 0.896

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-213.png)<!-- -->

    ## temperature_change_from_ch4 - oil_co2: 0.905

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-214.png)<!-- -->

    ## temperature_change_from_co2 - oil_co2: 0.894

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-215.png)<!-- -->

    ## temperature_change_from_ghg - oil_co2: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-216.png)<!-- -->

    ## temperature_change_from_n2o - oil_co2: 0.895

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-217.png)<!-- -->

    ## total_ghg - oil_co2: 0.894

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-218.png)<!-- -->

    ## total_ghg_excluding_lucf - oil_co2: 0.917

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-219.png)<!-- -->

    ## share_global_cumulative_co2 - share_global_co2: 0.990

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-220.png)<!-- -->

    ## share_global_co2 - share_global_cumulative_co2: 0.990

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-221.png)<!-- -->

    ## cumulative_luc_co2 - share_global_cumulative_luc_co2: 0.810

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-222.png)<!-- -->

    ## land_use_change_co2 - share_global_cumulative_luc_co2: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-223.png)<!-- -->

    ## methane - share_global_cumulative_luc_co2: 0.742

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-224.png)<!-- -->

    ## share_global_luc_co2 - share_global_cumulative_luc_co2: 0.944

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-225.png)<!-- -->

    ## share_of_temperature_change_from_ghg - share_global_cumulative_luc_co2: 0.905

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-226.png)<!-- -->

    ## total_ghg - share_global_cumulative_luc_co2: 0.711

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-227.png)<!-- -->

    ## cumulative_luc_co2 - share_global_luc_co2: 0.743

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-228.png)<!-- -->

    ## land_use_change_co2 - share_global_luc_co2: 0.968

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-229.png)<!-- -->

    ## share_global_cumulative_luc_co2 - share_global_luc_co2: 0.944

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-230.png)<!-- -->

    ## share_of_temperature_change_from_ghg - share_global_luc_co2: 0.826

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-231.png)<!-- -->

    ## cumulative_luc_co2 - share_of_temperature_change_from_ghg: 0.744

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-232.png)<!-- -->

    ## land_use_change_co2 - share_of_temperature_change_from_ghg: 0.793

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-233.png)<!-- -->

    ## methane - share_of_temperature_change_from_ghg: 0.776

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-234.png)<!-- -->

    ## nitrous_oxide - share_of_temperature_change_from_ghg: 0.716

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-235.png)<!-- -->

    ## share_global_cumulative_luc_co2 - share_of_temperature_change_from_ghg: 0.905

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-236.png)<!-- -->

    ## share_global_luc_co2 - share_of_temperature_change_from_ghg: 0.826

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-237.png)<!-- -->

    ## temperature_change_from_co2 - share_of_temperature_change_from_ghg: 0.709

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-238.png)<!-- -->

    ## temperature_change_from_ghg - share_of_temperature_change_from_ghg: 0.707

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-239.png)<!-- -->

    ## total_ghg - share_of_temperature_change_from_ghg: 0.773

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-240.png)<!-- -->

    ## population - temperature_change_from_ch4: 0.888

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-241.png)<!-- -->

    ## cement_co2 - temperature_change_from_ch4: 0.830

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-242.png)<!-- -->

    ## co2 - temperature_change_from_ch4: 0.784

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-243.png)<!-- -->

    ## cumulative_cement_co2 - temperature_change_from_ch4: 0.837

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-244.png)<!-- -->

    ## cumulative_co2 - temperature_change_from_ch4: 0.855

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-245.png)<!-- -->

    ## cumulative_luc_co2 - temperature_change_from_ch4: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-246.png)<!-- -->

    ## cumulative_oil_co2 - temperature_change_from_ch4: 0.830

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-247.png)<!-- -->

    ## methane - temperature_change_from_ch4: 0.979

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-248.png)<!-- -->

    ## nitrous_oxide - temperature_change_from_ch4: 0.987

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-249.png)<!-- -->

    ## oil_co2 - temperature_change_from_ch4: 0.905

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-250.png)<!-- -->

    ## temperature_change_from_co2 - temperature_change_from_ch4: 0.948

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-251.png)<!-- -->

    ## temperature_change_from_ghg - temperature_change_from_ch4: 0.972

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-252.png)<!-- -->

    ## temperature_change_from_n2o - temperature_change_from_ch4: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-253.png)<!-- -->

    ## total_ghg - temperature_change_from_ch4: 0.982

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-254.png)<!-- -->

    ## total_ghg_excluding_lucf - temperature_change_from_ch4: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-255.png)<!-- -->

    ## population - temperature_change_from_co2: 0.750

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-256.png)<!-- -->

    ## cement_co2 - temperature_change_from_co2: 0.731

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-257.png)<!-- -->

    ## co2 - temperature_change_from_co2: 0.761

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-258.png)<!-- -->

    ## cumulative_cement_co2 - temperature_change_from_co2: 0.809

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-259.png)<!-- -->

    ## cumulative_co2 - temperature_change_from_co2: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-260.png)<!-- -->

    ## cumulative_luc_co2 - temperature_change_from_co2: 0.860

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-261.png)<!-- -->

    ## cumulative_oil_co2 - temperature_change_from_co2: 0.879

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-262.png)<!-- -->

    ## methane - temperature_change_from_co2: 0.911

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-263.png)<!-- -->

    ## nitrous_oxide - temperature_change_from_co2: 0.944

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-264.png)<!-- -->

    ## oil_co2 - temperature_change_from_co2: 0.894

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-265.png)<!-- -->

    ## share_of_temperature_change_from_ghg - temperature_change_from_co2: 0.709

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-266.png)<!-- -->

    ## temperature_change_from_ch4 - temperature_change_from_co2: 0.948

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-267.png)<!-- -->

    ## temperature_change_from_ghg - temperature_change_from_co2: 0.996

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-268.png)<!-- -->

    ## temperature_change_from_n2o - temperature_change_from_co2: 0.981

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-269.png)<!-- -->

    ## total_ghg - temperature_change_from_co2: 0.959

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-270.png)<!-- -->

    ## total_ghg_excluding_lucf - temperature_change_from_co2: 0.967

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-271.png)<!-- -->

    ## population - temperature_change_from_ghg: 0.796

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-272.png)<!-- -->

    ## cement_co2 - temperature_change_from_ghg: 0.767

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-273.png)<!-- -->

    ## co2 - temperature_change_from_ghg: 0.775

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-274.png)<!-- -->

    ## cumulative_cement_co2 - temperature_change_from_ghg: 0.827

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-275.png)<!-- -->

    ## cumulative_co2 - temperature_change_from_ghg: 0.904

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-276.png)<!-- -->

    ## cumulative_luc_co2 - temperature_change_from_ghg: 0.881

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-277.png)<!-- -->

    ## cumulative_oil_co2 - temperature_change_from_ghg: 0.876

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-278.png)<!-- -->

    ## methane - temperature_change_from_ghg: 0.939

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-279.png)<!-- -->

    ## nitrous_oxide - temperature_change_from_ghg: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-280.png)<!-- -->

    ## oil_co2 - temperature_change_from_ghg: 0.906

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-281.png)<!-- -->

    ## share_of_temperature_change_from_ghg - temperature_change_from_ghg: 0.707

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-282.png)<!-- -->

    ## temperature_change_from_ch4 - temperature_change_from_ghg: 0.972

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-283.png)<!-- -->

    ## temperature_change_from_co2 - temperature_change_from_ghg: 0.996

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-284.png)<!-- -->

    ## temperature_change_from_n2o - temperature_change_from_ghg: 0.988

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-285.png)<!-- -->

    ## total_ghg - temperature_change_from_ghg: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-286.png)<!-- -->

    ## total_ghg_excluding_lucf - temperature_change_from_ghg: 0.977

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-287.png)<!-- -->

    ## population - temperature_change_from_n2o: 0.807

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-288.png)<!-- -->

    ## cement_co2 - temperature_change_from_n2o: 0.780

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-289.png)<!-- -->

    ## co2 - temperature_change_from_n2o: 0.766

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-290.png)<!-- -->

    ## cumulative_cement_co2 - temperature_change_from_n2o: 0.848

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-291.png)<!-- -->

    ## cumulative_co2 - temperature_change_from_n2o: 0.901

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-292.png)<!-- -->

    ## cumulative_luc_co2 - temperature_change_from_n2o: 0.844

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-293.png)<!-- -->

    ## cumulative_oil_co2 - temperature_change_from_n2o: 0.882

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-294.png)<!-- -->

    ## methane - temperature_change_from_n2o: 0.923

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-295.png)<!-- -->

    ## nitrous_oxide - temperature_change_from_n2o: 0.960

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-296.png)<!-- -->

    ## oil_co2 - temperature_change_from_n2o: 0.895

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-297.png)<!-- -->

    ## temperature_change_from_ch4 - temperature_change_from_n2o: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-298.png)<!-- -->

    ## temperature_change_from_co2 - temperature_change_from_n2o: 0.981

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-299.png)<!-- -->

    ## temperature_change_from_ghg - temperature_change_from_n2o: 0.988

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-300.png)<!-- -->

    ## total_ghg - temperature_change_from_n2o: 0.955

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-301.png)<!-- -->

    ## total_ghg_excluding_lucf - temperature_change_from_n2o: 0.963

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-302.png)<!-- -->

    ## population - total_ghg: 0.847

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-303.png)<!-- -->

    ## cement_co2 - total_ghg: 0.812

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-304.png)<!-- -->

    ## co2 - total_ghg: 0.783

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-305.png)<!-- -->

    ## cumulative_cement_co2 - total_ghg: 0.816

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-306.png)<!-- -->

    ## cumulative_co2 - total_ghg: 0.853

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-307.png)<!-- -->

    ## cumulative_luc_co2 - total_ghg: 0.908

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-308.png)<!-- -->

    ## cumulative_oil_co2 - total_ghg: 0.813

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-309.png)<!-- -->

    ## methane - total_ghg: 0.981

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-310.png)<!-- -->

    ## nitrous_oxide - total_ghg: 0.982

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-311.png)<!-- -->

    ## oil_co2 - total_ghg: 0.894

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-312.png)<!-- -->

    ## share_global_cumulative_luc_co2 - total_ghg: 0.711

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-313.png)<!-- -->

    ## share_of_temperature_change_from_ghg - total_ghg: 0.773

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-314.png)<!-- -->

    ## temperature_change_from_ch4 - total_ghg: 0.982

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-315.png)<!-- -->

    ## temperature_change_from_co2 - total_ghg: 0.959

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-316.png)<!-- -->

    ## temperature_change_from_ghg - total_ghg: 0.975

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-317.png)<!-- -->

    ## temperature_change_from_n2o - total_ghg: 0.955

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-318.png)<!-- -->

    ## total_ghg_excluding_lucf - total_ghg: 0.978

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-319.png)<!-- -->

    ## population - total_ghg_excluding_lucf: 0.807

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-320.png)<!-- -->

    ## cement_co2 - total_ghg_excluding_lucf: 0.843

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-321.png)<!-- -->

    ## co2 - total_ghg_excluding_lucf: 0.799

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-322.png)<!-- -->

    ## cumulative_cement_co2 - total_ghg_excluding_lucf: 0.862

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-323.png)<!-- -->

    ## cumulative_co2 - total_ghg_excluding_lucf: 0.889

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-324.png)<!-- -->

    ## cumulative_luc_co2 - total_ghg_excluding_lucf: 0.846

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-325.png)<!-- -->

    ## cumulative_oil_co2 - total_ghg_excluding_lucf: 0.857

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-326.png)<!-- -->

    ## methane - total_ghg_excluding_lucf: 0.933

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-327.png)<!-- -->

    ## nitrous_oxide - total_ghg_excluding_lucf: 0.962

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-328.png)<!-- -->

    ## oil_co2 - total_ghg_excluding_lucf: 0.917

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-329.png)<!-- -->

    ## temperature_change_from_ch4 - total_ghg_excluding_lucf: 0.966

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-330.png)<!-- -->

    ## temperature_change_from_co2 - total_ghg_excluding_lucf: 0.967

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-331.png)<!-- -->

    ## temperature_change_from_ghg - total_ghg_excluding_lucf: 0.977

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-332.png)<!-- -->

    ## temperature_change_from_n2o - total_ghg_excluding_lucf: 0.963

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-333.png)<!-- -->

    ## total_ghg - total_ghg_excluding_lucf: 0.978

    ## `geom_smooth()` using formula = 'y ~ x'

![](r-notebook_files/figure-gfm/unnamed-chunk-9-334.png)<!-- -->

Cross Dataset Analysis - Correlation Matrix for investigating how global
temperatures are affected by CO2 levels

``` r
#preparing temperature data
temp_data <- df1_clean %>%
  select(Year, `Land and Ocean`) %>%
  rename(year = Year, global_temp = `Land and Ocean`) %>%
  filter(!is.na(global_temp))

#preparing CO2 data - aggregate global totals by year
co2_data <- df2_clean %>%
  group_by(year) %>%
  summarise(
    global_co2 = sum(co2, na.rm = TRUE),
    global_co2_per_capita = mean(co2_per_capita, na.rm = TRUE),
    global_cumulative_co2 = sum(cumulative_co2, na.rm = TRUE),
    global_ghg = sum(total_ghg, na.rm = TRUE),
    global_methane = sum(methane, na.rm = TRUE),
    global_nitrous_oxide = sum(nitrous_oxide, na.rm = TRUE),
    avg_temp_change_from_co2 = mean(temperature_change_from_co2, na.rm = TRUE),
    avg_temp_change_from_ghg = mean(temperature_change_from_ghg, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(global_co2) & global_co2 > 0)

combined_data <- inner_join(temp_data, co2_data, by = "year")

print(paste("Combined dataset has", nrow(combined_data), "years of data"))
```

    ## [1] "Combined dataset has 136 years of data"

``` r
print(paste("Year range:", min(combined_data$year), "-", max(combined_data$year)))
```

    ## [1] "Year range: 1880 - 2015"

``` r
#calculating correlations
correlation_vars <- combined_data %>%
  select(global_temp, global_co2, global_co2_per_capita, global_cumulative_co2, 
         global_ghg, global_methane, global_nitrous_oxide)

cor_matrix <- cor(correlation_vars, use = "complete.obs")
col <- colorRampPalette(brewer.pal(9, "Blues"))(200)
corrplot(cor_matrix, method = "color", col = col,
  addCoef.col = "white", tl.col = "black", tl.srt = 45, 
  title = "Temperature vs CO2 Correlations", 
  mar = c(0,0,3,0), number.cex = 0.8, cl.cex = 0.8, tl.cex = 0.9, 
  outline = TRUE, addgrid.col = NA)
```

![](r-notebook_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Cross Dataset Analysis - Time Series Analysis for investigating how
global temperatures are affected by CO2 levels


<!-- README.md is generated from README.Rmd. Please edit that file -->

# postmastr <img src="man/figures/logo.png" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](https://travis-ci.org/slu-openGIS/postmastr.svg?branch=master)](https://travis-ci.org/slu-openGIS/postmastr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/postmastr?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/postmastr)
[![Coverage
status](https://codecov.io/gh/slu-openGIS/postmastr/branch/master/graph/badge.svg)](https://codecov.io/github/slu-openGIS/postmastr?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/postmastr)](https://cran.r-project.org/package=postmastr)

The goal of postmastr is to provide consistent, tidy parsing of street
address data. The package is primarily oriented towards American-style
street addresses, e.g. “123 East Main Street”. It contains functions for
both standardizing address elements (e.g. converting street names like
“Second” to “2nd” or converting “AV” to “Ave”) and for parsing out
intput strings into separate variables for each input element.

## Motivation

Street addresses can be notoriously difficult to work with. In the
United States, the U.S. Postal Service has [standards for their
composition](https://pe.usps.com/text/pub28/welcome.htm). There is so
much variety, however, that anticipating all of the possible
permutations of addresses is a significant task. When the inaccuracy of
human data entry is added, the challenge of parsing addresses becomes
monumental. The goal of `postmastr` is to provide a uniform workflow for
parsing street address data that allows for sufficient flexibility.

This flexibility is provided in two ways. First, we utilize
“dictionaries” for a number of the key functions that allow users to
provide vectors of data to base parsing on. This enables `postmastr` to
parse potential misspellings and colloquial terms that are hard (or
impossible) to predict. Second, not all aspects of the workflow are
mandatory - if street address data do not contain postal codes, states,
or cities, for example, those functions can be skipped.

## Installation

`postmastr` is not available from CRAN yet. In the meantime, you can
install the development version of `postmastr` from Github with
`remotes`:

``` r
# install.packages("remotes")
remotes
```

## Usage

To illustrate the core components of the `postmastr` workflow, we’ll use
some data included in the package on sushi resturants in the St. Louis,
Missouri region. These are “long” data - some resturants appear multiple
times. Here is a quick preview of the data:

``` r
> sushi1
# A tibble: 30 x 3
   name                            address                                           visit   
   <chr>                           <chr>                                             <chr>   
 1 BaiKu Sushi Lounge              3407 Olive St, St. Louis, Missouri 63103          3/20/18 
 2 Blue Ocean Restaurant           6335 Delmar Blvd, St. Louis, MO 63112             10/26/18
 3 Cafe Mochi                      3221 S Grand Boulevard, St. Louis, MO 63118       10/10/18
 4 Drunken Fish - Ballpark Village 601 Clark Ave #104, St. Louis, MO 63102-1719      4/28/18 
 5 Drunken Fish - Ballpark Village 601 Clark Ave Suite 104, St. Louis, MO 63102-1719 5/10/18 
 6 Drunken Fish - Ballpark Village 601 Clark Ave Suite 104, St. Louis, MO 63102-1719 8/7/18  
 7 Drunken Fish - Central West End 1 Maryland Plaza, St. Louis, MO 63108             12/2/18 
 8 I Love Mr Sushi                 9443 Olive Blvd, St. Louis, Missouri 63132        1/1/18  
 9 Kampai Sushi Bar                4949 W Pine Blvd, St. Louis, MO 63108             2/13/18 
10 Midtown Sushi & Ramen           3674 Forest Park Ave, St. Louis, MO 63108         3/4/18  
# … with 20 more rows
```

For the `sushi1` data, the required dictionaries
are:

``` r
> dirs <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")
> sufs <- pm_dictionary(type = "suffix", locale = "us")
> mo <- pm_dictionary(type = "state", filter = "MO", case = c("title", "upper"), locale = "us")
> cities <- pm_append(type = "city",
+                       input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", 
+                                 "St. Louis", "SAINT LOUIS", "Webster Groves"),
+                       output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))
```

The `sushi1` data are small, and the dictionaries could be developed
simply by looking through the data. However, a typical data set will
require significant exploration to develop these dictionaries.
`postmastr` provides a [full-feature
workflow](https://slu-opengis.github.io/postmastr/articles/postmastr.html)
for working manually through the parsing process to both develop
dictionaries and troubleshoot issues with your data.

Once dictionaries have been developed, the `pm_parse()` function can be
used to fully prep, parse, and reconstruct address strings:

``` r
> postmastr::sushi1 %>%
+   dplyr::filter(name != "Drunken Fish - Ballpark Village") %>%
+   pm_parse(input = "full",
+            var = address,
+            output = "short",
+            dirDict = dirs,
+            suffixDict = sufs,
+            cityDict = cities,
+            stateDict = mo)
# A tibble: 27 x 4
   name                            address                                        visit    pm.address               
   <chr>                           <chr>                                          <chr>    <chr>                    
 1 BaiKu Sushi Lounge              3407 Olive St, St. Louis, Missouri 63103       3/20/18  3407 Olive St            
 2 Blue Ocean Restaurant           6335 Delmar Blvd, St. Louis, MO 63112          10/26/18 6335 Delmar Blvd         
 3 Cafe Mochi                      3221 S Grand Boulevard, St. Louis, MO 63118    10/10/18 3221 S Grand Blvd        
 4 Drunken Fish - Central West End 1 Maryland Plaza, St. Louis, MO 63108          12/2/18  1 Maryland Plz           
 5 I Love Mr Sushi                 9443 Olive Blvd, St. Louis, Missouri 63132     1/1/18   9443 Olive Blvd          
 6 Kampai Sushi Bar                4949 W Pine Blvd, St. Louis, MO 63108          2/13/18  4949 W Pine Blvd         
 7 Midtown Sushi & Ramen           3674 Forest Park Ave, St. Louis, MO 63108      3/4/18   3674 Forest Park Ave     
 8 Mizu Sushi Bar                  1013 Washington Avenue, St. Louis, MO 63101    9/12/18  1013 Washington Ave      
 9 Robata Maplewood                7260 Manchester Road, Maplewood, MO 63143      11/1/18  7260 Manchester Rd       
10 SanSai Japanese Grill Maplewood 1803 Maplewood Commons Dr, St. Louis, MO 63143 2/14/18  1803 Maplewood Commons Dr
# … with 17 more rows
```

## Expansion

The `postmastr` functions all contain a `locale` argument that is only
enabled for American (i.e. `locale = "us"`) addresses. Assistance with
expanding `postmastr` functionality to other countries would be most
welcome. If you work with street address data in another country and
would like to contribute to `postmastr` by extending its functionality,
please [open a feature request
issue](https://github.com/slu-openGIS/postmastr/issues/new/choose) and
introduce yourself\!

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

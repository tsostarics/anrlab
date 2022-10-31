
# anrlab

<!-- badges: start -->
[![R-CMD-check](https://github.com/tsostarics/anrlab/workflows/R-CMD-check/badge.svg)](https://github.com/tsostarics/anrlab/actions)
[![Lifecycle:Retired](https://img.shields.io/badge/Lifecycle-Retired-d45500)](https://github.com/tsostarics/anrlab)
<!-- badges: end -->
=======

***NOTICE:*** Due to our PI's retirement and our lab shutting down, this package
is no longer needed and so will no longer be maintained. However, it will be
available here for future reference.

The goal of `anrlab` is to provide utilities for lab members to work with data 
from our database. We utilize RedCap, which makes data entry simple, but data 
extraction very complex. This package makes use of extensive upfront work in
designing the Redcap database to streamline the data processing pipeline. At its
core, this package converts the extremely wide dataframe from Redcap into a
structure that is similar to a more traditional relational database. 

An example of the workflow this package provides is available in the workflow
vignette.

## Installation

You can install the latest version of `anrlab` from [Github](https://github.com/tsostarics/anrlab) with:

``` r
devtools::install_github("tsostarics/anrlab")
```




# anrlab

<!-- badges: start -->
[![R-CMD-check](https://github.com/tsostarics/anrlab/workflows/R-CMD-check/badge.svg)](https://github.com/tsostarics/anrlab/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/tsostarics/anrlab/badge/master)](https://www.codefactor.io/repository/github/tsostarics/anrlab/overview/master)
<!-- badges: end -->
=======

The goal of `anrlab` is to provide utilities for lab members to work with data 
from our database. We utilize Redcap, which makes data entry simple, but data 
extraction very complex. This package makes use of extensive upfront work in
designing the Redcap database to streamline the data processing pipeline. At its
core, this package converts the extremely wide dataframe from Redcap into a
structure that is similar to a more traditional relational database.

## Installation

You can install the latest version of `anrlab` from [Github](https://github.com/tsostarics/anrlab) with:

``` r
devtools::install_github("tsostarics/anrlab")
```



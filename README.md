
<!-- README.md is generated from README.Rmd. Please edit that file -->

# makeup

<!-- badges: start -->
<!-- badges: end -->

## Overview

makeup helps to format number, character and date values in order to
make them human readable.

The main function of this package is `makeup()`. It takes a date, number
o character value and returns it into a human readable format

## Installation

Install the development version of makeup from GitHub with:

``` r
#install.packages("devtools")
remotes::install_github("datasketch/makeup")
```

## Example

This is a basic example which shows you how this packages work:

Let´s load `makeup` package

``` r
library(makeup)
```

### Making *number* values human readable

``` r
x <- c(1234.56, 432141, 0.12)

makeup(x, 
       sample = "1,432.1")
#> [1] "1,234.6" "432,141" "0.1"
```

### Making *character* values human readable

``` r
x <- c("hello", "WoRlD","Hello world")

makeup(x, sample = "down")
#> [1] "hello"       "world"       "hello world"

makeup(x, sample = "UPPER")
#> [1] "HELLO"       "WORLD"       "HELLO WORLD"

makeup(x, sample = "Title phrase")
#> [1] "Hello"       "World"       "Hello world"

makeup(x, sample = "Title Case")
#> [1] "Hello"       "World"       "Hello World"
```

### Making *date* values human readable

``` r
x <- as.Date(c("2020-03-05","2020-06-20"))

makeup(x)
#> [1] "3/5/2020"  "6/20/2020"
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{herder}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{herder}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
herder::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-01-10 15:53:40 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ────────────────────────────────── herder 0.0.0.9000 ────
#> Duration: 4.3s
#> 
#> ❯ checking whether package ‘herder’ can be installed ... ERROR
#>   See below...
#> 
#> ── Install failure ─────────────────────────────────────────────────────────────
#> 
#> * installing *source* package ‘herder’ ...
#> ** using staged installation
#> ** R
#> ** inst
#> ** byte-compile and prepare package for lazy loading
#> Error in library(shinyWidgets) : 
#>   there is no package called ‘shinyWidgets’
#> Error: unable to load R code in package ‘herder’
#> Execution halted
#> ERROR: lazy loading failed for package ‘herder’
#> * removing ‘/tmp/RtmpYzAhCT/file1c24c9738c08e3/herder.Rcheck/herder’
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```

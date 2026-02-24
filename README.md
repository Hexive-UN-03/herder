
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

You are reading the doc about version : 0.0.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-02-24 08:35:02 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading herder
#> ── R CMD check results ───────────────────────────────────── herder 0.0.1.0 ────
#> Duration: 33.4s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking for executable files ... WARNING
#>   Found the following executable files:
#>     inst/scripts/fast_af
#>     inst/scripts/sample_lister
#>     inst/scripts/vcf_trimmer
#>   Source packages should not contain undeclared executable files.
#>   See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
#> 
#> ❯ checking installed package size ... NOTE
#>     installed size is 21.1Mb
#>     sub-directories of 1Mb or more:
#>       scripts  21.0Mb
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard file/directory found at top level:
#>     ‘dev’
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in ‘NEWS.md’:
#>   No news entries found.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   app_server : reset_all_categories: no visible binding for global
#>     variable ‘unique_breeds’
#>   app_server : reset_all_categories: no visible binding for global
#>     variable ‘unique_ages’
#>   app_server : generate_af_plot: no visible binding for global variable
#>     ‘POS’
#>   app_server : generate_af_plot: no visible binding for global variable
#>     ‘AF’
#>   app_server: no visible binding for global variable ‘processed_csv’
#>   app_server: no visible binding for global variable ‘Breed’
#>   app_server: no visible binding for global variable ‘Sex’
#>   app_server: no visible binding for global variable ‘Age’
#>   app_server: no visible binding for global variable ‘unique_breeds’
#>   app_server: no visible binding for global variable ‘norm_vcf_path’
#>   app_server: no visible global function definition for ‘read.table’
#>   app_server: no visible binding for global variable ‘POS’
#>   app_server: no visible binding for global variable ‘AF’
#>   app_server : <anonymous>: no visible global function definition for
#>     ‘write.table’
#>   app_server : <anonymous>: no visible binding for global variable ‘POS’
#>   app_ui: no visible binding for '<<-' assignment to ‘norm_dataset’
#>   app_ui: no visible binding for '<<-' assignment to ‘norm_vcf_path’
#>   app_ui: no visible binding for global variable ‘norm_dataset’
#>   app_ui: no visible binding for global variable ‘norm_vcf_path’
#>   app_ui: no visible binding for global variable ‘unique_breeds’
#>   app_ui: no visible binding for global variable ‘unique_ages’
#>   process_csv: no visible global function definition for ‘read.csv’
#>   process_csv: no visible binding for global variable ‘Horse_ID’
#>   process_csv: no visible binding for '<<-' assignment to ‘unique_breeds’
#>   process_csv: no visible binding for '<<-' assignment to ‘unique_ages’
#>   process_csv: no visible binding for '<<-' assignment to ‘unique_sexes’
#>   Undefined global functions or variables:
#>     AF Age Breed Horse_ID POS Sex norm_dataset norm_vcf_path
#>     processed_csv read.csv read.table unique_ages unique_breeds
#>     write.table
#>   Consider adding
#>     importFrom("utils", "read.csv", "read.table", "write.table")
#>   to your NAMESPACE file.
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> > library(herder)
#> > 
#> > test_check("herder")
#> Loading required package: shiny
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 7 ]
#> 
#> ══ Failed tests ════════════════════════════════════════════════════════════════
#> ── Error ('test-golem-recommended.R:2:3'): app ui ──────────────────────────────
#> Error in `path.expand(path)`: invalid 'path' argument
#> Backtrace:
#>     ▆
#>  1. └─herder:::app_ui() at test-golem-recommended.R:2:3
#>  2.   └─base::normalizePath(dataset)
#>  3.     └─base::path.expand(path)
#> ── Error ('test-golem-recommended.R:55:1'): (code run outside of `test_that()`) ──
#> Error in `server(input = session$input, output = session$output, session = session)`: object 'processed_csv' not found
#> Backtrace:
#>      ▆
#>   1. ├─shiny::testServer(...) at test-golem-recommended.R:55:1
#>   2. │ ├─shiny:::withMockContext(...)
#>   3. │ │ ├─shiny::isolate(...)
#>   4. │ │ │ ├─shiny::..stacktraceoff..(...)
#>   5. │ │ │ └─ctx$run(...)
#>   6. │ │ │   ├─promises::with_promise_domain(...)
#>   7. │ │ │   │ └─domain$wrapSync(expr)
#>   8. │ │ │   ├─shiny::withReactiveDomain(...)
#>   9. │ │ │   │ └─promises::with_promise_domain(...)
#>  10. │ │ │   │   └─domain$wrapSync(expr)
#>  11. │ │ │   │     └─base::force(expr)
#>  12. │ │ │   ├─shiny::captureStackTraces(...)
#>  13. │ │ │   │ └─promises::with_promise_domain(...)
#>  14. │ │ │   │   └─domain$wrapSync(expr)
#>  15. │ │ │   │     └─base::withCallingHandlers(expr, error = doCaptureStack)
#>  16. │ │ │   └─env$runWith(self, func)
#>  17. │ │ │     └─shiny (local) contextFunc()
#>  18. │ │ │       └─shiny::..stacktraceon..(expr)
#>  19. │ │ ├─shiny::withReactiveDomain(...)
#>  20. │ │ │ └─promises::with_promise_domain(...)
#>  21. │ │ │   └─domain$wrapSync(expr)
#>  22. │ │ │     └─base::force(expr)
#>  23. │ │ └─withr::with_options(...)
#>  24. │ │   └─base::force(code)
#>  25. │ └─herder (local) server(input = session$input, output = session$output, session = session)
#>  26. │   └─shiny::reactiveValues(samples = processed_csv$Horse_ID)
#>  27. │     └─rlang::list2(...)
#>  28. └─base::.handleSimpleError(...)
#>  29.   └─shiny (local) h(simpleError(msg, call))
#> 
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 7 ]
#> Error: Test failures
#> Execution halted
#> 
#> 1 error ✖ | 1 warning ✖ | 5 notes ✖
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```

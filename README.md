<!-- README.md is generated from README.Rmd. Please edit that file -->



# `{herder}`

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/Hexive-UN-03/herder/graph/badge.svg)](https://app.codecov.io/gh/Hexive-UN-03/herder)
<!-- badges: end -->

A Shiny app for subsetting and visualizing large joint-called VCFs against a sample
metadata sheet.

Provide a VCF, a metadata CSV and (optionally) a reference GTF, and you get:

- **Subset selection** — filter your cohort interactively by breed, sex and age. Free-text
  breed and sex spellings are collapsed automatically, and age is inferred from sex terms
  where it's implied (a *yearling* is 1–2, a *filly* is under 4, a *mare* is over 4, and so
  on). Save subsets, reload them, export the sample list, or write out a subset VCF.
- **Allele frequencies** — compute AF over a region for the selected samples, plot it
  against a gene/exon track on a shared axis, brush to zoom, click a variant for its
  REF/ALT and frequency, and download the region as TSV.

**No bcftools, no htslib, no compiler required.** Everything that touches a VCF is done by
three small statically-linked programs shipped inside the package, so installation is just
an R package install.

## Requirements

- R (>= 4.1 — the app uses the native `|>` pipe)
- A **bgzipped and tabix-indexed** VCF (`your.vcf.gz` plus `your.vcf.gz.tbi` beside it).
  An unindexed or uncompressed VCF will not work.
- A metadata CSV with a sample-ID column, and ideally breed / sex / age columns. Columns
  are matched by name — the first column whose name contains `ID` is the sample ID, and
  likewise for `Breed`, `Sex` and `Age` (case-insensitive). Samples in the CSV that aren't
  in the VCF are dropped.
- Optionally, a GTF for your reference genome. Without one the app runs fine; you just
  don't get the gene track.

## Installation

For both windows and linux:

``` r
# install.packages("remotes")
remotes::install_github("Hexive-UN-03/herder")
```

### Checking the install worked


``` r
# should print the path to the bundled binary for your platform
herder:::herder_bin("fast_af")
```

## Running it


``` r
library(herder)

run_app(
  dataset  = "path/to/sample_metadata.csv",
  vcf_path = "path/to/joint_call.vcf.gz",
  gtf_path = "path/to/reference.gtf.gz"   # optional
)
```

`gtf_path` is optional. Leave it out and the app runs without the gene track — the gene
picker and the two annotation panels are simply not shown, and everything else behaves
identically:


``` r
run_app(
  dataset  = "path/to/sample_metadata.csv",
  vcf_path = "path/to/joint_call.vcf.gz"
)
```

Note: a whole-genome GTF takes a minute or two to read.

### Using the app

1. **Select Subset → Manual Selection.** Narrow the cohort with the breed, sex and age
   controls; the sample list on the right updates to match. Name the subset and
   **Save Subset**, or **Generate VCF** to write a subset VCF next to your input.
2. **Saved Subsets.** Focus a saved subset back into the selectors, download its sample
   list, or delete it. You can also upload an existing list of sample names under
   *Select Subset → Upload Sample List*.
3. **Allele Frequencies.** Enter a region as `chr1:1-2000000` and **Calculate** — this runs
   `fast_af` over your selected samples. Then enter a sub-range as `1-2000000` and **View**
   to plot it. Pick genes to draw on the track, drag across the top plot to zoom, and click
   a variant in the zoomed plot to see its details. Both **Download** buttons export TSV.

You can also skip the calculation step and upload a previously computed `roi_af.tsv`.

## Performance

`fast_af` is multithreaded and the cost is dominated by reading and parsing the region
rather than by cohort size. Over a 10 Mb region of a 967-sample joint call, on 4 cores:

| Samples in subset | 1 | 10 | 100 | 400 | 967 |
|---|---|---|---|---|---|
| Wall-clock | 2.6 s | 2.7 s | 3.3 s | 4.7 s | 7.2 s |

That's about 2.6 s of fixed cost whatever the cohort size, then roughly 0.5 s per
additional 100 samples — the full cohort costs only 2.8× a single sample. Threading scales
close to linearly up to the number of physical cores available and then degrades, so don't
ask for more threads than you have cores.

## Development

Built with [`{golem}`](https://thinkr-open.github.io/golem/).


``` r
# from a clone of the repo
pkgload::load_all(".")
testthat::test_dir("tests/testthat")
```

`dev/run_dev.R` launches the app against a local dataset for development.

## About

You are reading the doc about version : 0.0.1.0


<!-- README.md is generated from README.Rmd. Please edit that file -->

# cross

<!-- badges: start -->

[![R-CMD-check](https://github.com/DavisVaughan/cross/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavisVaughan/cross/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/cross/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DavisVaughan/cross?branch=main)

<!-- badges: end -->

The goal of cross is to rerun a single function across a configuration
of package versions or git branches. The typical use case for this is
creating a reprex containing a before and after benchmark, where you’d
like to run the same code on both the `main` branch and your
`branch/my-fix` branch, or using the CRAN version of a package against
the dev version of a package.

## Installation

You can install the development version of cross like so:

``` r
pak::pak("DavisVaughan/cross")
```

## Example

`cross::run()` runs a function across various versions of the same
package. The `pkgs` can be any remote specification supported by
`pak::pkg_install()`.

``` r
library(bench) # So bench classes print nicely

out <- cross::run(pkgs = c("vctrs", "r-lib/vctrs"), ~{
  library(vctrs)
  x <- c(1, NA, 2, 3, NA)
  bench::mark(missing = vec_detect_missing(x)) 
})

out
#> # A tibble: 2 × 2
#>   pkg         result             
#>   <chr>       <list>             
#> 1 vctrs       <bnch_mrk [1 × 13]>
#> 2 r-lib/vctrs <bnch_mrk [1 × 13]>

tidyr::unnest(out, result)
#> # A tibble: 2 × 14
#>   pkg         expression     min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
#>   <chr>       <bch:expr> <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#> 1 vctrs       missing      816ns  968ns   942535.    2.23KB        0 10000     0
#> 2 r-lib/vctrs missing      760ns  959ns   936836.    2.25KB        0 10000     0
#> # ℹ 5 more variables: total_time <bch:tm>, result <list>, memory <list>,
#> #   time <list>, gc <list>
```

`cross::run_branches()` runs a function across various local branches.
It assumes that:

- `usethis::proj_get()` points to an R package using git.
- There are no uncommitted git changes.

If those are true, then it will automatically run the function against
the current branch and the `main` branch, but you can change this with
the `current` and `branches` arguments.

``` r
# Not run here, but assuming we are in the vctrs repo this would run the
# function against whatever branch we are on and the `main` branch

cross::run_branches(~{
  library(vctrs)
  x <- c(1, NA, 2, 3, NA)
  bench::mark(missing = vec_detect_missing(x)) 
})
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# cross

<!-- badges: start -->

[![R-CMD-check](https://github.com/DavisVaughan/cross/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavisVaughan/cross/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of cross is to run a single expression across a configuration
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

`cross::bench_versions()` runs an expression across various versions of
the same package. The expression itself must end with a call to either
`bench::mark()` or `bench::press()`. The `pkgs` can be any remote
specification supported by `pak::pkg_install()`.

``` r
cross::bench_versions(pkgs = c("vctrs", "r-lib/vctrs"), {
  library(vctrs)
  x <- c(1, NA, 2, 3, NA)
  bench::mark(missing = vec_detect_missing(x))
})
#> # A tibble: 2 × 14
#>   pkg         expression      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
#>   <chr>       <bch:expr> <bch:tm> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#> 1 vctrs       missing       287ns  369ns  2060916.    2.23KB        0 10000     0
#> 2 r-lib/vctrs missing       230ns  320ns  2338528.    2.25KB        0 10000     0
#> # ℹ 5 more variables: total_time <bch:tm>, result <list>, memory <list>,
#> #   time <list>, gc <list>
```

If you have a more complex combination of packages to benchmark against,
you can supply a data frame where each row of the data frame represents
a package combination.

``` r
pkgs <- tibble::tribble(
  ~vctrs, ~purrr,
  "vctrs", "purrr",
  "r-lib/vctrs", "purrr",
  "r-lib/vctrs", "tidyverse/purrr"
)

cross::bench_versions(pkgs = pkgs, {
  library(purrr)
  x <- list(1, 2)
  bench::mark(map(x, is.double))
})
#> # A tibble: 3 × 14
#>   pkg            expression    min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
#>   <chr>          <bch:expr> <bch:> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#> 1 vctrs, purrr   map(x, is… 39.2µs 40.2µs    24005.     410KB     116.  9952    48
#> 2 r-lib/vctrs, … map(x, is…   39µs 39.9µs    24225.     410KB     117.  9952    48
#> 3 r-lib/vctrs, … map(x, is… 10.1µs 10.5µs    91029.     142KB     164.  9982    18
#> # ℹ 5 more variables: total_time <bch:tm>, result <list>, memory <list>,
#> #   time <list>, gc <list>
```

`cross::bench_branches()` runs an expression across various local
branches. It assumes that:

- `usethis::proj_get()` points to an R package using git.
- There are no uncommitted git changes.

If those are true, then it will automatically run the expression against
the current branch and the `main` branch, but you can change this with
the `current` and `branches` arguments.

``` r
# Not run here, but assuming we are in the vctrs repo this would run the
# expression against whatever branch we are on and the `main` branch

cross::bench_branches({
  library(vctrs)
  x <- c(1, NA, 2, 3, NA)
  bench::mark(missing = vec_detect_missing(x))
})
```

The underlying engines that powers these are `cross::run_versions()` and
`cross::run_branches()`, which allow you to run any arbitrary expression
across multiple package versions or local branches.
`cross::bench_versions()` and `cross::bench_branches()` are small
wrappers around these that work seamlessly with `bench::mark()` or
`bench::press()` results.

#' Benchmark a function across different package versions
#'
#' @description
#' `bench_versions()` allows you to run a single function, `fn`, multiple times
#' in separate R sessions, where each R session has different versions of
#' packages installed. A typical use case is running a before/after benchmark,
#' comparing the CRAN version of a package with a development version of the
#' same package.
#'
#' For example, `bench_versions(fn, pkgs = c("vctrs", "r-lib/vctrs#100"))` would
#' run `fn` in 2 separate R sessions, one with CRAN vctrs installed, and one
#' with the pull request installed.
#'
#' When using `bench_versions()`, each call to `fn` must return a `<bench_mark>`
#' data frame from the bench package. This is typically generated using either a
#' call to [bench::mark()] or [bench::press()].
#'
#' `bench_versions()` is similar to [run_versions()], but is specifically
#' designed to be useful in conjunction with the bench package when benchmarking
#' across multiple package versions.
#'
#' @inherit run_versions details
#' @inheritSection run_versions Global options
#'
#' @inheritParams run_versions
#'
#' @returns
#' A `<bench_mark>`.
#'
#' @export
#' @examplesIf FALSE
#' # Run a benchmark across 2 different versions of vctrs
#' bench_versions(pkgs = c("vctrs", "r-lib/vctrs"), ~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
#'
#' # You can also use `bench::press()` to generate a grid
#' bench_versions(pkgs = c("vctrs", "r-lib/vctrs"), ~{
#'   library(vctrs)
#'   set.seed(123)
#'   x <- sample(100)
#'
#'   bench::press(
#'     table = c(10, 20, 30),
#'     {
#'       table <- seq_len(table)
#'       bench::mark(vec_match(x, table))
#'     }
#'   )
#' })
bench_versions <- function(
  fn,
  ...,
  pkgs,
  args = list(),
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  # {bench} must be loaded in the main R session so that vctrs methods for
  # `bench_expr` are registered, otherwise `vec_c()` won't work correctly.
  load_bench()

  check_dots_empty0(...)

  out <- run_versions(
    fn = fn,
    pkgs = pkgs,
    args = args,
    libpath = libpath,
    args_pak = args_pak,
    args_callr = args_callr
  )

  pkg <- out[["pkg"]]
  bench_marks <- out[["result"]]

  combine_bench_marks(
    bench_marks = bench_marks,
    extra = pkg,
    extra_name = "pkg"
  )
}

#' Benchmark a function across different local package branches
#'
#' @description
#' `bench_branches()` is similar to [run_branches()], but is specifically
#' designed to be useful when benchmarking across different local package
#' branches.
#'
#' When using `bench_branches()`, each call to `fn` must return a `<bench_mark>`
#' data frame from the bench package. This is typically generated using either a
#' call to [bench::mark()] or [bench::press()].
#'
#' @inherit run_branches details
#' @inheritSection run_branches Global options
#'
#' @inheritParams run_branches
#'
#' @returns
#' A `<bench_mark>`.
#'
#' @export
#' @examplesIf FALSE
#' # Similar to `bench_versions()`, but this runs the function across
#' # 2 local branches.
#' # To run this:
#' # - The working directory is set to the RStudio project for vctrs
#' # - There can't be any uncommitted git changes
#' # - You are currently on a branch, say `fix/performance-bug`
#' # - You'd like to run that branch against `main`
#' bench_branches(~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
bench_branches <- function(
  fn,
  ...,
  args = list(),
  current = TRUE,
  branches = "main",
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  # {bench} must be loaded in the main R session so that vctrs methods for
  # `bench_expr` are registered, otherwise `vec_c()` won't work correctly.
  load_bench()

  check_dots_empty0(...)

  out <- run_branches(
    fn = fn,
    args = args,
    current = current,
    branches = branches,
    libpath = libpath,
    args_pak = args_pak,
    args_callr = args_callr
  )

  branch <- out[["branch"]]
  bench_marks <- out[["result"]]

  combine_bench_marks(
    bench_marks = bench_marks,
    extra = branch,
    extra_name = "branch"
  )
}

combine_bench_marks <- function(
  bench_marks,
  extra,
  extra_name,
  call = caller_env()
) {
  check_list_of_bench_marks(bench_marks, call = call)

  # Convert individual results to data frame to avoid any funny business,
  # we will make one big <bench_mark> again at the end
  bench_marks <- purrr::map(bench_marks, function(bench_mark) {
    size <- vec_size(bench_mark)
    tibble::as_tibble(bench_mark, .rows = size)
  })

  sizes <- list_sizes(bench_marks)
  size <- sum(sizes)

  # Combine bench marks into one tibble
  out <- list_unchop(bench_marks)

  # Append `extra` to the front of the tibble
  extra <- vec_rep_each(extra, sizes)
  extra <- list(extra)
  names(extra) <- extra_name
  extra <- tibble::new_tibble(extra, nrow = size)

  out <- vec_cbind(extra, out)

  # Convert final result back to <bench_mark>. It's like `bench::press()`,
  # faceted by either the installed pkg or the branch.
  out <- bench::as_bench_mark(out)

  out
}

load_bench <- function() {
  check_installed("bench")
  loadNamespace("bench")
}

check_list_of_bench_marks <- function(x, call = caller_env()) {
  if (!all(purrr::map_lgl(x, is_bench_mark))) {
    abort("`fn` must return a <bench_mark>.", call = call)
  }
}

is_bench_mark <- function(x) {
  inherits(x, "bench_mark")
}

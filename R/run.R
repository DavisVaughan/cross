#' Evaluate a function across different package versions
#'
#' @description
#' `run_versions()` allows you to run a single function, `fn`, multiple times in
#' separate R sessions, where each R sessions has different versions of packages
#' installed. If you're looking to run benchmarks across different versions of
#' packages, you likely want [bench_versions()] instead.
#'
#' For example, `run_versions(fn, pkgs = c("vctrs", "r-lib/vctrs#100"))` would
#' run `fn` in 2 separate R sessions, one with CRAN vctrs installed, and one
#' with the pull request installed.
#'
#' @details
#' The installed packages are placed in temporary directories that act as an
#' extra library path. The temporary directory is prepended to `libpath`, which
#' is then passed along to [callr::r()] as the `libpath` argument there. This
#' ensures that your personal package libraries remain untouched.
#'
#' @section Global options:
#'
#' - `cross.quiet`: `[TRUE / FALSE]`
#'
#'   Should cross specific messages about the process be shown?
#'
#'   Defaults to `!rlang::is_interactive()` if not set, so messages are only
#'   shown when running interactively.
#'
#' - `cross.pak_quiet`: `[TRUE / FALSE]`
#'
#'   Should pak installation messages be shown?
#'
#'   Defaults to `TRUE` if not set.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param fn `[function]`
#'
#'   A function to evaluate. The function is passed along to [callr::r()], so
#'   it is evaluated in a fresh R session and must be self-contained.
#'
#'   Read the `func` docs of [callr::r()] for the full set of restrictions on
#'   `fn`.
#'
#'   `fn` is converted to a function with [rlang::as_function()], so it can be
#'   a lambda function.
#'
#' @param args `[list]`
#'
#'   An optional list of arguments to pass to the function.
#'
#' @param pkgs `[character]`
#'
#'   A character vector of package names or remote package specifications to
#'   install and run `fn` against. Passed along to [pak::pkg_install()].
#'
#' @param libpath `[character]`
#'
#'   The base library path to use.
#'
#' @param args_pak `[named list]`
#'
#'   A named list of arguments to pass on to [pak::pkg_install()].
#'
#'   Can't include:
#'   - `pkg`
#'   - `lib`
#'   - `ask`
#'
#' @param args_callr `[named list]`
#'
#'   A named list of arguments to pass on to [callr::r()].
#'
#'   Can't include:
#'   - `func`
#'   - `args`
#'   - `libpath`
#'
#' @returns
#' A data frame with two columns:
#' - `pkg`, a character vector containing `pkgs`.
#' - `result`, a list column containing the result of calling `fn` for that
#'   version of the package.
#'
#' @export
#' @examplesIf FALSE
#' # Run a benchmark across 2 different versions of vctrs
#' run_versions(pkgs = c("vctrs", "r-lib/vctrs"), ~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
run_versions <- function(
  fn,
  ...,
  pkgs,
  args = list(),
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  load_callr()

  check_dots_empty0(...)

  check_character(pkgs)
  check_character(libpath)
  vec_check_list(args_pak)
  check_named2(args_pak)

  n <- length(pkgs)
  libs <- local_libdirs(n)
  install_pkgs(pkgs, libs, args_pak)

  libpaths <- lapply(libs, function(lib) {
    c(lib, libpath)
  })

  results <- run_libpaths(
    fn = fn,
    args = args,
    libpaths = libpaths,
    args_callr = args_callr
  )

  out <- list(pkg = pkgs, result = results)
  out <- tibble::new_tibble(out, nrow = n)

  out
}

install_pkgs <- function(pkgs, libs, args_pak) {
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]

    args <- list(
      pkg = pkg,
      lib = libs[[i]],
      ask = FALSE
    )

    args_pak[names(args)] <- args

    ui_done("Installing package {usethis::ui_value(pkg)}")

    pak_suppress({
      inject(pak::pkg_install(!!!args_pak))
    })
  }
}

# ------------------------------------------------------------------------------

#' Evaluate a function across different local package branches
#'
#' @description
#' `run_branches()` is similar to [run_versions()], except it allows you to run
#' `fn` across different local branches corresponding to the same package,
#' rather than different CRAN or GitHub versions of that package.
#'
#' The default behavior runs the current branch against the `main` branch.
#'
#' @inherit run_versions details
#' @inheritSection run_versions Global options
#'
#' @inheritParams run_versions
#'
#' @param current `[TRUE / FALSE]`
#'
#'   Should the current git branch be included in the vector of `branches`?
#'
#' @param branches `[character]`
#'
#'   A character vector of git branch names to check out, install, and run `fn`
#'   against.
#'
#'   It is expected that your working directory is set to the git directory
#'   of the package you want to install different branches of. This is typically
#'   the case whenever you open an RStudio project for the package in question.
#'   Technically, the path is determined by [usethis::proj_get()].
#'
#'   Your git tree must be completely clean to use `branches`. If there are any
#'   uncommitted changes, an error will be thrown because `run_branches()` must
#'   swap between the branches to install the package, potentially resulting in
#'   a loss of information. Note that untracked files are not included in this
#'   check - they should never be lost when the branch is changed, but they
#'   could affect the results.
#'
#'   After the last branch is installed, the original branch is checked out.
#'
#' @returns
#' A data frame with two columns:
#' - `branch`, a character vector containing `branches`.
#' - `result`, a list column containing the result of calling `fn` for that
#'   branch of the package.
#'
#' @export
#' @examplesIf FALSE
#' # Similar to `run_versions()`, but this runs the function across
#' # 2 local branches.
#' # To run this:
#' # - The working directory is set to the RStudio project for vctrs
#' # - There can't be any uncommitted git changes
#' # - You are currently on a branch, say `fix/performance-bug`
#' # - You'd like to run that branch against `main`
#' run_branches(~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
run_branches <- function(
  fn,
  ...,
  args = list(),
  current = TRUE,
  branches = "main",
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  load_callr()

  check_dots_empty0(...)

  check_bool(current)
  check_character(branches)
  check_character(libpath)
  vec_check_list(args_pak)
  check_named2(args_pak)

  path <- with_usethis_quiet({
    usethis::proj_get()
  })

  if (!is_package(path)) {
    message <- c(
      "Must be within a package to install a branch.",
      i = "{.fn usethis::proj_get} reported the project path as {path}."
    )
    cli::cli_abort(message)
  }

  if (!uses_git(path)) {
    message <- c(
      "The package must use git to install a branch.",
      i = "{.fn usethis::proj_get} reported the project path as {path}."
    )
    cli::cli_abort(message)
  }

  if (git_has_changes(path)) {
    message <- c(
      "Can't use {.fn run_branches} when there are uncommited changes between the working directory and the git index.",
      i = "Commit your changes first!"
    )
    cli::cli_abort(message)
  }

  original <- gert::git_branch(path)

  if (current) {
    branches <- c(original, branches)
  }

  n <- length(branches)
  libs <- local_libdirs(n)

  install_branches(
    branches = branches,
    libs = libs,
    original = original,
    path = path,
    args_pak = args_pak
  )

  libpaths <- lapply(libs, function(lib) {
    c(lib, libpath)
  })

  results <- run_libpaths(
    fn = fn,
    args = args,
    libpaths = libpaths,
    args_callr = args_callr
  )

  out <- list(branch = branches, result = results)
  out <- tibble::new_tibble(out, nrow = n)

  out
}

install_branches <- function(branches, libs, original, path, args_pak) {
  withr::defer(gert::git_branch_checkout(original, repo = path))

  for (i in seq_along(branches)) {
    branch <- branches[[i]]

    gert::git_branch_checkout(branch, repo = path)

    args_pak[["pkg"]] <- path
    args_pak[["lib"]] <- libs[[i]]
    args_pak[["ask"]] <- FALSE

    ui_done("Installing branch {usethis::ui_value(branch)}")

    pak_suppress({
      inject(pak::pkg_install(!!!args_pak))
    })
  }
}

is_package <- function(path) {
  # `usethis:::is_package()`
  tryCatch(
    expr = {
      rprojroot::find_package_root_file(path = path)
      TRUE
    },
    error = function(cnd) {
      FALSE
    }
  )
}

uses_git <- function(path) {
  # `usethis:::uses_git()`
  tryCatch(
    expr = {
      gert::git_find(path = path)
      TRUE
    },
    error = function(cnd) {
      FALSE
    }
  )
}

git_has_changes <- function(path) {
  # `ref = NULL` to dif against the working directory
  diff <- gert::git_diff(ref = NULL, repo = path)
  nrow(diff) > 0L
}

with_usethis_quiet <- function(expr) {
  local_options(usethis.quiet = TRUE)
  expr
}

# ------------------------------------------------------------------------------

run_libpaths <- function(
  fn,
  ...,
  args = list(),
  libpaths = list(),
  args_callr = list(),
  error_call = caller_env()
) {
  check_dots_empty0(...)

  fn <- as_function(fn, call = error_call)

  vec_check_list(args, call = error_call)
  vec_check_list(libpaths, call = error_call)
  vec_check_list(args_callr, call = error_call)
  check_named2(args_callr, call = error_call)

  args_callr[["func"]] <- fn
  args_callr[["args"]] <- args

  n <- length(libpaths)
  out <- vector("list", length = n)

  ui_done("Running {usethis::ui_code('fn')} across variants")

  for (i in seq_len(n)) {
    args_callr[["libpath"]] <- libpaths[[i]]

    elt <- inject(callr::r(!!!args_callr))

    if (is.null(elt)) {
      # Avoid `out[[i]] <- NULL` shortening the result
      next
    }

    out[[i]] <- elt
  }

  out
}

# ------------------------------------------------------------------------------

local_libdirs <- function(n, frame = caller_env()) {
  out <- vector("character", length = n)

  for (i in seq_len(n)) {
    out[[i]] <- withr::local_tempdir(.local_envir = frame)
  }

  out
}

pak_suppress <- function(expr) {
  if (option_pak_quiet()) {
    without_callr_messages(expr)
  } else {
    expr
  }
}
option_pak_quiet <- function() {
  is_true(getOption("cross.pak_quiet", default = TRUE))
}
without_callr_messages <- function(expr) {
  withCallingHandlers(
    expr,
    callr_message = function(cnd) {
      cnd_muffle(cnd)
    }
  )
}

ui_done <- function(x, frame = caller_env()) {
  if (!option_quiet()) {
    usethis::ui_done(x, .envir = frame)
  }
  invisible()
}
option_quiet <- function() {
  is_true(getOption("cross.quiet", default = !rlang::is_interactive()))
}

load_callr <- function() {
  # Force callr to be loaded before pak to avoid S3 method overwriting message
  # https://github.com/r-lib/callr/issues/254
  dummy <- callr::r
  invisible()
}

check_named2 <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (is_named2(x)) {
    return(invisible(NULL))
  }
  cli::cli_abort("{.arg {arg}} must be fully named.", call = call)
}

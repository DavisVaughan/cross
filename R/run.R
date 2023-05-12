#' Evaluate a function across different package versions
#'
#' @description
#' `run()` allows you to run a single function, `fn`, multiple times in separate
#' R sessions, where each R sessions has different versions of packages
#' installed. A typical use case is running a before/after benchmark, comparing
#' the CRAN version of a package with a development version of the same package.
#'
#' For example, `run(fn, pkgs = c("vctrs", "r-lib/vctrs#100"))` would run `fn`
#' in 2 separate R sessions, one with CRAN vctrs installed, and one with the
#' pull request installed.
#'
#' @details
#' The packages installed by `pkgs` and `branches` are placed in temporary
#' directories that act as an extra library path. The temporary directory is
#' prepended to `libpath`, which is then passed along to [callr::r()] as the
#' `libpath` argument there. This ensures that your personal package libraries
#' remain untouched.
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
#'   uncommitted changes, an error will be thrown because `run()` must swap
#'   between the branches to install the package, potentially resulting in a
#'   loss of information. Note that untracked files are not included in this
#'   check - they should never be lost when the branch is changed, but they
#'   could affect the results.
#'
#'   After the last branch is installed, the original branch is checked out.
#'
#' @param libpath `[character]`
#'
#'   The base library path to use. The temporary library path that `pkgs` and
#'   `branches` is installed into will be prepended to this list.
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
#' - `source`, a character vector of `c(pkgs, branches)`.
#' - `result`, a list column containing the result of calling `fn` for that
#'   version of the package.
#'
#' @export
#' @examplesIf FALSE
#' # Run a benchmark across 2 different versions of vctrs
#' run(pkgs = c("vctrs", "r-lib/vctrs"), ~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
#'
#' # Similar to above, but this runs it across 2 different local branches.
#' # To run this:
#' # - The working directory is set to the RStudio project for vctrs
#' # - There must be a branch called `fix/performance-bug`
#' # - There can't be any uncommitted git changes
#' run(branches = c("main", "fix/performance-bug"), ~{
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
run <- function(fn,
                ...,
                args = list(),
                pkgs = character(),
                branches = character(),
                libpath = .libPaths(),
                args_pak = list(),
                args_callr = list()) {
  # Force callr to be loaded before pak to avoid S3 method overwriting message
  # https://github.com/r-lib/callr/issues/254
  dummy <- callr::r

  check_dots_empty0(...)

  fn <- as_function(fn)

  vec_check_list(args)
  check_character(pkgs)
  check_character(branches)
  check_character(libpath)
  vec_check_list(args_pak)
  vec_check_list(args_callr)

  if (!is_named2(args_pak)) {
    cli::cli_abort("{.arg args_pak} must be fully named.")
  }
  if (!is_named2(args_callr)) {
    cli::cli_abort("{.arg args_callr} must be fully named.")
  }

  n <- 0L
  sources <- character()
  libs <- character()

  n_pkgs <- length(pkgs)
  if (n_pkgs > 0L) {
    libs_pkgs <- local_libdirs(n_pkgs)
    install_pkgs(pkgs, libs_pkgs, args_pak)

    n <- n + n_pkgs
    sources <- c(sources, pkgs)
    libs <- c(libs, libs_pkgs)
  }

  n_branches <- length(branches)
  if (n_branches > 0L) {
    libs_branches <- local_libdirs(n_branches)
    install_branches(branches, libs_branches, args_pak)

    n <- n + n_branches
    sources <- c(sources, branches)
    libs <- c(libs, libs_branches)
  }

  results <- vector("list", length = n)
  ui_done("Running {usethis::ui_code('fn')} across variants")

  args <- list(
    func = fn,
    args = args
  )
  args_callr[names(args)] <- args

  for (i in seq_len(n)) {
    args_callr[["libpath"]] <- c(libs[[i]], libpath)

    elt <- inject(callr::r(!!!args_callr))

    if (is.null(elt)) {
      # Avoid `results[[i]] <- NULL` shortening the result
      next
    }

    results[[i]] <- elt
  }

  out <- list(source = sources, result = results)
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

install_branches <- function(branches,
                             libs,
                             args_pak,
                             ...,
                             error_call = caller_env()) {
  check_dots_empty0(...)

  path <- with_usethis_quiet({
    usethis::proj_get()
  })

  if (!is_package(path)) {
    message <- c(
      "Must be within a package to install a branch.",
      i = "{.fn usethis::proj_get} reported the project path as {path}."
    )
    cli::cli_abort(message, call = error_call)
  }

  if (!uses_git(path)) {
    message <- c(
      "The package must use git to install a branch.",
      i = "{.fn usethis::proj_get} reported the project path as {path}."
    )
    cli::cli_abort(message, call = error_call)
  }

  if (git_has_changes(path)) {
    message <- c(
      "Can't use {.arg branches} when there are uncommited changes between the working directory and the git index.",
      i = "Commit your changes first!"
    )
    cli::cli_abort(message, call = error_call)
  }

  original_branch <- gert::git_branch(repo = path)
  withr::defer(gert::git_branch_checkout(original_branch, repo = path))

  for (i in seq_along(branches)) {
    branch <- branches[[i]]

    gert::git_branch_checkout(branch, repo = path)

    args <- list(
      pkg = path,
      lib = libs[[i]],
      ask = FALSE
    )

    args_pak[names(args)] <- args

    ui_done("Installing branch {usethis::ui_value(branch)}")

    pak_suppress({
      inject(pak::pkg_install(!!!args_pak))
    })
  }
}

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
  suppressMessages(expr, classes = "callr_message")
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

ui_done <- function(x, frame = caller_env()) {
  if (!option_quiet()) {
    usethis::ui_done(x, .envir = frame)
  }
  invisible()
}
option_quiet <- function() {
  is_true(getOption("cross.quiet", default = !rlang::is_interactive()))
}

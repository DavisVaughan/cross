#' Evaluate an expression across different package versions
#'
#' @description
#' `run_versions()` allows you to run a single expression, `expr`, multiple
#' times in separate R sessions, where each R sessions has different versions of
#' packages installed. If you're looking to run benchmarks across different
#' versions of packages, you likely want [bench_versions()] instead.
#'
#' For example, `run_versions(expr, pkgs = c("vctrs", "r-lib/vctrs#100"))` would
#' run `expr` in 2 separate R sessions, one with CRAN vctrs installed, and one
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
#' @param expr `[expression]`
#'
#'   An expression to evaluate. The expression is passed along to [callr::r()]
#'   as the body of a function with zero arguments, so it is evaluated in a
#'   fresh R session and must be self-contained.
#'
#'   Read the `func` docs of [callr::r()] for the full set of restrictions on
#'   `expr`.
#'
#' @param pkgs `[character / data frame of character columns]`
#'
#'   A character vector of package names or remote package specifications to
#'   install and run `expr` against. Passed along to [pak::pkg_install()].
#'
#'   For more complex installations, also accepts a data frame of character
#'   columns of package names. Each row in the data frame specifies a package
#'   combination to run against.
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
#' - `pkg`, a character vector containing the packages installed.
#' - `result`, a list column containing the result of calling `expr` for that
#'   combination of packages.
#'
#' @export
#' @examplesIf FALSE
#' # Run a benchmark across 2 different versions of vctrs
#' # (See `bench_versions()` for an even easier way)
#' run_versions(pkgs = c("vctrs", "r-lib/vctrs"), {
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
#'
#' # Run a benchmark against a combination of development versions
#' pkgs <- tibble::tribble(
#'   ~vctrs, ~purrr,
#'   "vctrs", "purrr",
#'   "r-lib/vctrs", "purrr",
#'   "r-lib/vctrs", "tidyverse/purrr"
#' )
#'
#' run_versions(pkgs = pkgs, {
#'   library(purrr)
#'   x <- list(1, 2)
#'   bench::mark(map(x, is.double))
#' })
run_versions <- function(
  expr,
  ...,
  pkgs,
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  load_callr()

  check_dots_empty0(...)

  expr <- enexpr(expr)

  pkgs <- normalize_pkgs(pkgs)
  pkg <- pkgs[["pkg"]]
  list_of_pkgs <- pkgs[["list_of_pkgs"]]

  check_character(libpath)
  obj_check_list(args_pak)
  check_named2(args_pak)

  n <- length(list_of_pkgs)
  libs <- local_libdirs(n)
  install_pkgs(list_of_pkgs, libs, args_pak)

  libpaths <- lapply(libs, function(lib) {
    c(lib, libpath)
  })

  results <- run_libpaths(
    expr = expr,
    libpaths = libpaths,
    args_callr = args_callr
  )

  out <- list(pkg = pkg, result = results)
  out <- tibble::new_tibble(out, nrow = n)

  out
}

install_pkgs <- function(list_of_pkgs, libs, args_pak) {
  for (i in seq_along(list_of_pkgs)) {
    pkg <- list_of_pkgs[[i]]

    args <- list(
      pkg = pkg,
      lib = libs[[i]],
      ask = FALSE
    )

    args_pak[names(args)] <- args

    ui_done(cli::format_inline("Installing package{?s} {.pkg {pkg}}"))

    pak_suppress({
      inject(pak::pkg_install(!!!args_pak))
    })
  }
}

# Normalize `pkgs`
#
# Outputs a data frame of two columns:
#
# - `pkg`:
#
#   A character vector describing each package installation combination. If
#   multiple packages are being installed in one combination, they are separated
#   by a comma. This is the human readable column used in the `pkg` column of
#   the result. Smushing multiple packages into one string is more friendly than
#   returning a df-col, both for users and for the autoplot methods that bench
#   provides! For bench in particular, anecdotal evidence suggests that faceting
#   on 1 column describing each package combination generates a plot that is
#   easier to understand than `N` columns (one per package you are installing
#   variants of). I also like that it ensures that `pkg` is a fixed predictable
#   column name in the output, always of type character, regardless of the form
#   of `pkgs` the user provided.
#
# - `list_of_pkgs`:
#
#   A list of character vectors, where each character vector is a combination
#   of packages to install via pak.
normalize_pkgs <- function(pkgs, error_call = caller_env()) {
  if (is_character(pkgs)) {
    return(normalize_character_pkgs(pkgs))
  }

  if (is.data.frame(pkgs)) {
    return(normalize_data_frame_pkgs(pkgs, error_call))
  }

  stop_input_type(
    x = pkgs,
    what = "a character vector or data frame of character vectors",
    call = error_call
  )
}

normalize_character_pkgs <- function(pkgs) {
  data_frame(
    pkg = pkgs,
    list_of_pkgs = vec_chop(pkgs)
  )
}

normalize_data_frame_pkgs <- function(pkgs, error_call) {
  if (!all(purrr::map_lgl(pkgs, is_character))) {
    abort(
      "Every column of `pkgs` must be a character vector.",
      call = error_call
    )
  }

  # Turn data frame into bare list for the transpose
  attributes(pkgs) <- NULL

  list_of_pkgs <- list_transpose(pkgs)

  # Collapse into 1 string per combination for simplicity and
  # compatibility with bench's autoplot methods
  pkgs <- purrr::map_chr(list_of_pkgs, function(pkgs) {
    paste0(pkgs, collapse = ", ")
  })

  data_frame(
    pkg = pkgs,
    list_of_pkgs = list_of_pkgs
  )
}

# Transpose a list of vectors
#
# Invariants:
#
# - `x` must be a list
# - Names of `x` are used in error messages, but are then discarded
# - Each element of `x` is coerced to the common type of the elements, or
#   `ptype`
# - Each element of `x` is recycled to the common size of the elements
# - Output is a list of size `size`. Each element is a vector of size `x_size`
#   and type `ptype`.
list_transpose <- function(
  x,
  ...,
  ptype = NULL,
  x_arg = caller_arg(x),
  error_call = caller_env()
) {
  obj_check_list(x, arg = x_arg, call = error_call)
  list_check_all_vectors(x, arg = x_arg, call = error_call)

  x_size <- vec_size(x)

  ptype <- vec_ptype_common(
    !!!x,
    .ptype = ptype,
    .arg = x_arg,
    .call = error_call
  )
  size <- vec_size_common(
    !!!x,
    .arg = x_arg,
    .call = error_call
  )

  x <- vec_cast_common(
    !!!x,
    .to = ptype,
    .arg = x_arg,
    .call = error_call
  )
  x <- vec_recycle_common(
    !!!x,
    .size = size,
    .arg = x_arg,
    .call = error_call
  )

  # Don't want outer names after doing common type / size determination,
  # could use `.name_spec = "inner"` with new vctrs in `vec_interleave()`
  x <- unname(x)

  # Combine pieces of size `size` into one big vector via interleaving
  x <- vec_interleave(!!!x, .ptype = ptype)

  # Chop the one big vector into transposed pieces of size `x_size`
  x <- vec_chop(x, sizes = vec_rep(x_size, times = size))

  x
}

# ------------------------------------------------------------------------------

#' Evaluate an expression across different local package branches
#'
#' @description
#' `run_branches()` is similar to [run_versions()], except it allows you to run
#' `expr` across different local branches corresponding to the same package,
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
#'   A character vector of git branch names to check out, install, and run
#'   `expr` against.
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
#' - `result`, a list column containing the result of calling `expr` for that
#'   branch of the package.
#'
#' @export
#' @examplesIf FALSE
#' # Similar to `run_versions()`, but this runs the expression across
#' # 2 local branches.
#' # To run this:
#' # - The working directory is set to the RStudio project for vctrs
#' # - There can't be any uncommitted git changes
#' # - You are currently on a branch, say `fix/performance-bug`
#' # - You'd like to run that branch against `main`
#' run_branches({
#'   library(vctrs)
#'   x <- c(TRUE, FALSE, NA, TRUE)
#'   bench::mark(vec_detect_missing(x))
#' })
run_branches <- function(
  expr,
  ...,
  current = TRUE,
  branches = "main",
  libpath = .libPaths(),
  args_pak = list(),
  args_callr = list()
) {
  load_callr()

  check_dots_empty0(...)

  expr <- enexpr(expr)

  check_bool(current)
  check_character(branches)
  check_character(libpath)
  obj_check_list(args_pak)
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
    expr = expr,
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

    args_pak[["pkg"]] <- paste0("local::", path)
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
  expr,
  ...,
  libpaths = list(),
  args_callr = list(),
  error_call = caller_env()
) {
  check_dots_empty0(...)

  obj_check_list(libpaths, call = error_call)
  obj_check_list(args_callr, call = error_call)
  check_named2(args_callr, call = error_call)

  # `callr::r()` will set the function environment to the `.GlobalEnv` anyways
  func <- new_function(
    args = list(),
    body = expr,
    env = global_env()
  )

  args_callr[["func"]] <- func

  n <- length(libpaths)
  out <- vector("list", length = n)

  ui_done("Running {usethis::ui_code('expr')} across variants")

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

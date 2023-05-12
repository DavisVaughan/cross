#' @export
run <- function(fn,
                ...,
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

  check_character(pkgs)
  check_character(branches)
  check_character(libpath)
  vec_check_list(args_pak)
  vec_check_list(args_callr)

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

  for (i in seq_len(n)) {
    args <- list(
      func = fn,
      libpath = c(libs[[i]], libpath)
    )
    args_callr[names(args)] <- args

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
    args <- list(
      pkg = pkgs[[i]],
      lib = libs[[i]],
      ask = FALSE
    )

    args_pak[names(args)] <- args

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
      "Can't have any changes between the working directory and the git index when using {.arg branches}.",
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
  if (option_pak_verbose()) {
    expr
  } else {
    without_callr_messages(expr)
  }
}

option_pak_verbose <- function() {
  is_true(getOption("cross.pak_verbose", default = FALSE))
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

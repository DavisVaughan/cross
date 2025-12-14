git <- function(
  ...,
  echo_cmd = FALSE,
  echo = FALSE,
  dry_run = FALSE,
  stderr_to_stdout = FALSE
) {
  if (dry_run) {
    cat("git", c(...), "\n")
  } else {
    processx::run(
      "git",
      c(...),
      echo_cmd = echo_cmd,
      echo = echo,
      stderr_to_stdout = stderr_to_stdout
    )
  }
}

git_worktree_add <- function(dir, branch, dry_run = FALSE) {
  git("worktree", "add", dir, branch, dry_run = dry_run)
}

git_worktree_remove <- function(dir, dry_run = FALSE) {
  git("worktree", "remove", "--force", dir, dry_run = dry_run)
}

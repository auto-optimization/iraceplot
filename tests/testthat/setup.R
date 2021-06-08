testthat_old_opts <- options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
testthat_old_opts <- lapply(testthat_old_opts, function(x) if (is.null(x)) FALSE else x)

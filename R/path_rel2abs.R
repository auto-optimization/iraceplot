path_rel2abs <- function(path, cwd = getwd()) {
  gsub.all <- function(pattern, repl, x, ...) {
    repeat {
      newx <- gsub(pattern, repl, x, ...)
      if (newx == x) {
        return(newx)
      }
      x <- newx
    }
  }
  irace.normalize.path <- function(path) {
    return(suppressWarnings(normalizePath(path,
      winslash = "/",
      mustWork = NA
    )))
  }
  if (is.null(path) || (length(path) == 1 && suppressWarnings(is.na(path)))) {
    return(NULL)
  }
  else if (path == "") {
    return("")
  }
  s <- "/"
  path <- path.expand(path)
  path <- gsub("\\", s, path, fixed = TRUE)
  windrive.regex <- "^[A-Za-z]:"
  windrive <- ""
  if (grepl(
    paste0(windrive.regex, "($|", s, ")"),
    path
  )) {
    m <- regexpr(windrive.regex, path)
    windrive <- regmatches(path, m)
    path <- sub(windrive.regex, "", path)
  }
  path <- gsub.all(paste0(s, ".", s), s, path, fixed = TRUE)
  path <- gsub(paste0(s, s, "+"), s, path)
  path <- sub(paste0(s, "\\.$"), s, path)
  path <- sub(paste0(s, "$"), "", path)
  if (path == "") {
    path <- s
  }
  if (path == "." || !grepl(paste0("^", s), path)) {
    if (!missing(cwd)) {
      cwd <- path_rel2abs(cwd)
    }
    if (path == ".") {
      return(irace.normalize.path(cwd))
    }
    path <- sub(paste0("^\\.", s), "", path)
    if (substring(cwd, nchar(cwd)) == s) {
      path <- paste0(cwd, path)
    } else {
      path <- paste0(cwd, s, path)
    }
    if (!grepl(paste0(s, "\\.\\."), path)) {
      return(irace.normalize.path(path))
    }
    if (grepl(
      paste0(windrive.regex, "($|", s, ")"),
      path
    )) {
      m <- regexpr(windrive.regex, path)
      windrive <- regmatches(path, m)
      path <- sub(windrive.regex, "", path)
    }
  }
  prevdir.regex <- paste0(
    s, "[^", s, "]+", s,
    "\\.\\."
  )
  repeat {
    tmp <- sub(paste0(prevdir.regex, s), s, path)
    if (tmp == path) {
      break
    }
    path <- tmp
  }
  path <- sub(paste0(prevdir.regex, "$"), s, path)
  repeat {
    tmp <- sub(
      paste0("^", s, "\\.\\.", s), s,
      path
    )
    if (tmp == path) {
      break
    }
    path <- tmp
  }
  path <- sub(paste0("^", s, "\\.\\.$"), s, path)
  path <- paste0(windrive, path)
  return(irace.normalize.path(path))
}

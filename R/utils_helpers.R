#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# where the compiled helpers actually live. app_sys is golem's wrapper around system.file,
# so this works both when the package is installed and when you're running load_all().
# windows sticks .exe on the end of everything, so ask for it there
herder_bin <- function(name){
  if (.Platform$OS.type == "windows"){
    name <- paste0(name, ".exe")
  }
  path <- app_sys(file.path("scripts", name))
  if (!nzchar(path)){
    stop("Couldn't find the '", name, "' helper program. It should be in inst/scripts.")
  }
  path
}

# a per-session scratch folder for the bits we shell out to write (sample lists, the af
# tsv). this used to be a hardcoded "./output" next to the installed package, which meant
# two people using the same install would stomp on each other's files, and it fell over
# entirely anywhere the package directory isn't writable
herder_scratch <- function(...){
  dir <- file.path(tempdir(), "herder")
  if (!dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }
  file.path(dir, ...)
}

# run one of the compiled helpers. system2 with a real argument vector rather than
# paste()-ing a command line, otherwise any path with a space in it (which is basically
# every path on windows) gets chopped in half by the shell
run_herder_bin <- function(name, args){
  status <- system2(herder_bin(name), args = as.character(args))
  if (!identical(status, 0L)){
    warning(name, " exited with status ", status)
  }
  invisible(status)
}

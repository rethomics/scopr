#' Get path to scopr example
#'
#' `scopr` comes with a sample DAM2 files in its `inst/extdata`
#' directory. `scopr_example` allow  make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' # list all files
#' scopr_example()

scopr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "scopr"))
  } else {
    system.file("extdata", path, package = "scopr", mustWork = TRUE)
  }
}


#' @rdname scopr_example
#' @export
scopr_example_dir <- function() {
  system.file("extdata",package = "scopr")
}


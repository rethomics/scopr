#
# parse_date <- function(x, format="", tz="UTC"){
#   out <- lapply(x, parse_one_date, format = format, tz = tz)
#   do.call(c, out)
# }

parse_date <- function(x, format="", tz="UTC"){

  if(is.factor(x))
    x <-  as.character(x)

  if(any(c("POSIXct","numeric") %in% class(x)))
    return(x)
  if(is.character(x)){

    out <- readr::parse_date(x, format=format, locale = readr::locale(tz = tz))

    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}


# parse_time <- function(x, format="", tz="UTC"){
#   out <- lapply(x, parse_one_time, format = format, tz = tz)
#   do.call(c, out)
# }

parse_time <- function(x, format="", tz="UTC"){
  if(is.factor(x))
    x <-  as.character(x)
  if("difftime" %in% class(x))
    return(x)
  if(is.character(x)){
    out <- readr::parse_time(x, format=format, locale = readr::locale(tz = tz))
    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}

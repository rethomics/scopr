

# a <- parse_datetime(c("2016-01-01", "2016-31-01"))
# a

#
# date_str_to_POSIX <- function(date,tz="UTC"){
#   date_char = ifelse(is.finite(date),as.character(date),date)
#   ldt <- lapply(date_char,function(x,tz){as.data.table(parse_datetime(x,tz))},tz)
#   data.table::rbindlist(ldt)[,date]
# }
#
# parse_datetime <- function(str, tz="UTC"){
#   if(is.infinite(str))
#     return(list(date=str,has_time=TRUE))
#
#
#   if(length(str) >1){
#     stop("Dates must be checked one by one, you are providing several dates")
#   }
#   if(! tz %in% c('', OlsonNames()))
#     stop("This time zone does not exist. Use `OlsonNames()` to list time zones")
#
#   pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}( [0-9]{2}:[0-9]{2}:[0-9]{2}){0,1}$"
#   match = grep(pattern, str)
#   if (length(match) != 1){
#     stop(sprintf("Date '%s' is not formated correctly.
#                  It must be either 'yyyy-mm-dd' or 'yyyy-mm-dd hh:mm:ss'",str))
#   }
#
#   out <- list()
#
#   if(nchar(str) == 10){
#     date <- as.POSIXct(str, "%Y-%m-%d",tz=tz)
#     out$date <- date
#     out$has_time <- FALSE
#   }
#   else{
#     date <- as.POSIXct(str, "%Y-%m-%d %H:%M:%S",tz=tz)
#     out$date <- date
#     out$has_time <- TRUE
#   }
#   if(is.na(date)){
#     stop(sprintf("Date '%s' seems to be formated correctly,
#                  but we cannot read it as a date.
#                  Probably the numbers are wrong (e.g. 2015-30-02 does not exist)",str))
#   }
#
#   out
# }

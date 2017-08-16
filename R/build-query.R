#' @noRd
build_query <- function(result_dir,
                                  query = NULL,
                                  # use_cached = FALSE,
                                  index_file = NULL){

  files_info <- list_result_files(result_dir, index_file)
  unique_fi = files_info[,.SD[.N],by=key(files_info)]

  key <- c("date", "time","machine_name")

  if(!is.null(query)){
    q <- data.table::copy(data.table::as.data.table(query))

    check_columns(c("date","machine_name"), q)
    if(!"time" %in% colnames(q))
      q[, time := NA_character_]

    q[, date := scopr:::parse_date(date, tz="UTC")]
    q[, time := scopr:::parse_time(time, tz="UTC")]
    data.table::setkeyv(q,key)
  }


  # first of all, we retreive the files for which time was not specified (NA)
  q_no_time <- q[is.na(time)]
  q_no_time[, time:=NULL]
  unique_fi_last_of_day <- data.table::copy(unique_fi)
  # n is the numbr of duplicated dates for each query
  unique_fi_last_of_day[, n := .N, by=c("date", "machine_name")]
  unique_fi_last_of_day <- unique(unique_fi_last_of_day, by=c("date", "machine_name"), fromLast = TRUE)



# here display duplicated experiments on same date
  duplicated_queries <- unique_fi_last_of_day[q_no_time, on=c("date", "machine_name")][n>1]
  duplicated_queries <- unique(duplicated_queries, by=c("date", "machine_name"))
  if(nrow(duplicated_queries) > 0){
    for( i in 1:nrow(duplicated_queries)){
      str <- "Several files (%i) in machine %s and date %s.
      Keeping last file (%s). Use a `time` column if this is not intended.}"
      str <- sprintf(str,duplicated_queries[i, n],
                     duplicated_queries[i, machine_name],
                     duplicated_queries[i, as.character(date)],
                     duplicated_queries[i, as.character(datetime)])
      warning(str)
    }
  }

  # we don't need "n" anymore
  unique_fi_last_of_day[,n:=NULL]
  out_no_time <-  unique_fi_last_of_day[q_no_time, on=c("date", "machine_name")]


  # now, we process the query row with time
  q_time <- q[!is.na(time)]
  out_time <- unique_fi[q_time]
  out <- rbind(out_time, out_no_time)
  out <- out[,.SD, by=c("date", "machine_name")]

  nas <- out[,is.na(path)]
    if(any(nas)){
      out_nas <- out[nas,]

      for(i in 1:nrow(out_nas)){
        warning(sprintf("No result for machine_name == %s, date == %s and time == %s. Omiting query",
                        out_nas[i,machine_name],
                        out_nas[i,date],
                        out_nas[i,time]
                        ))
      }
    }
    out[, date := NULL]
    out[, time := NULL]
    #out[, i.time := NULL]
    out <- na.omit(out)
    data.table::setkeyv(out, colnames(out))
    out
}

#   if(is.null(query))
#     return(files_info)
#
#   key <- c("date", "time","machine_name")
#   #use_date <- FALSE
#   if(!is.null(query)){
#     q <- data.table::copy(data.table::as.data.table(query))
#
#     check_columns(c("date","machine_name"), q)
#     if(!"time" %in% colnames(q))
#       q[, time := NA_character_]
#
#     q[, date := parse_date(date, tz="UTC")]
#     q[, time := parse_time(time, tz="UTC")]
#     data.table::setkeyv(q,key)
#     q_original <- data.table::copy(q)
#     #q <- unique(q, by=key)
#   }
#
#
#   unique_fi = files_info[,.SD[.N],by=key(files_info)]
#
#   # first of all, we retreive the files for which time was not specified (NA)
#
#   out <- unique_fi[q, on=c("machine_name","date")]
#   q[unique_fi, on=c("machine_name","date")]
#
#   q <- out[time == i.time | is.na(i.time)]
#
#   q_unique <- unique(q, by=c("date", "time","machine_name"))
#   out <- q_unique[, .(machine_name,date, time, i.time)]
#   out <- unique_fi[out, on=c("machine_name","date", "time")]
#
#   out <- out[, .(n=.N, time=time), by=c("machine_name", "date", "i.time")]
#
#   out <- unique_fi[out, on=c("machine_name","date", "time")]
#
# #return(out)
#   out <- unique(out, by=c("machine_id", "datetime"))
#
#   duplicated_queries <- unique(out[n>1,.(n, date, datetime,machine_name)],
#                                by=c("date", "machine_name"),
#                                fromLast = TRUE)
#
#   if(nrow(duplicated_queries) > 0){
#     for( i in 1:nrow(duplicated_queries)){
#       str <- "Several files (%i) in machine %s and date %s.
#       Keeping last file (%s). Use a `time` column if this is not intended.}"
#       str <- sprintf(str,duplicated_queries[i, n],
#                      duplicated_queries[i, machine_name],
#                      duplicated_queries[i, as.character(date)],
#                      duplicated_queries[i, as.character(datetime)])
#       warning(str)
#     }
#   }
#   out = unique(out,by=c("machine_id", "date", "i.time"),fromLast = T)
#
#   o <- out[,.(machine_id, datetime)]
#   out <- q[o, on=c("machine_id", "datetime")]
#
#   # we don't need the column 'n' any longer
#
#
#   nas <- out[,is.na(path)]
#   if(any(nas)){
#     out_nas <- out[nas,]
#
#     for(i in 1:nrow(out_nas)){
#       warning(sprintf("No result for machine_name == %s, date == %s and time == %s. Omiting query",
#                       out_nas[i,machine_name],
#                       out_nas[i,date],
#                       out_nas[i,i.time]
#                       ))
#     }
#   }
#
#   out[, date := NULL]
#   out[, time := NULL]
#   out[, i.time := NULL]
#   out <- na.omit(out)
#
#   data.table::setkeyv(out, colnames(out))
#   out
# }

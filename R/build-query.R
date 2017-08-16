#' Build a functional query for loading ethoscope data using the date of experiments and device names
#' to retreive result files
#'
#' This function is designed to list and select experimental files.
#' In general, end-users will want to retrieve  path to their experimental files
#' according to the date and ID of the ethoscope without having to understand the underlying directory structure.
#'
#' @param result_dir The location of the result directory (i.e. the folder containing all the data).
#@param use_cached whether cache files should be used
#' @param query An optional query formatted as a dataframe (see details).
#' @param index_file An optional file listing all experiments.
#' @return Query extended with the requested paths.
#' When `query` is not specified, the function returns a table with all available files.
#' @details
#' The optional argument `query` is expected to be a table where each row maps an experiment.
#' In many respects, it is similar to the `what` argument in [loadEthoscopeData].
#' The only difference is that it does not have a `"path"` column.
#' Instead, it must contain two columns:
#'
#'  * `date`  the date and time when the experiment started formatted either as `yyyy-mm-dd` or
#'  `yyyy-mm-dd_hh:mm:ss`. In the former case, there may be several matching experiments to a
#'  single time (starting the same day). When this happens, **only the last** is returned,
#'  and a warning message is displayed.
#'  * `machine_name` The name of the machine that acquired the data.
#'
#' The result is meant to be used directly, as the `what` argument, in [loadEthoscopeData] (see examples).
#' @note The ethoscope platform store data in a hard-coded directory structure
#' `"/root_dir/machine_id/machine_name/datetime/file.db"`, where:
#'
#'  * `machine_id` Is, in principle, a universally unique identifier of the acquisition device.
#'  * `machine_name` a human friendly name for acquisition device. In practice, this is expected to be unique within laboratory.
#'  * `datetime` the date and time of the start of the experiment
#'
#' @seealso
#' * [loadEthoscopeData], example case #3
#' * Tutorial for this function \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' * What queries are \url{http://gilestrolab.github.io/rethomics/tutorial/todo}
#' @noRd
build_query <- function(result_dir,
                                  query = NULL,
                                  # use_cached = FALSE,
                                  index_file = NULL){
  #todo
  if(is.null(index_file))
    check_dir_exists(result_dir)

  files_info <- list_result_files(result_dir)

  if(is.null(query))
    return(files_info)

  key <- c("date", "time","machine_name")
  #use_date <- FALSE
  if(!is.null(query)){
    q <- data.table::copy(data.table::as.data.table(query))

    check_columns(c("date","machine_name"), q)
    if(!"time" %in% colnames(q))
      q[, time := NA_character_]

    q[, date := parse_date(date, tz="UTC")]
    q[, time := parse_time(time, tz="UTC")]
    data.table::setkeyv(q,key)
  }

  unique_fi = files_info[,.SD[.N],by=key(files_info)]

  out <- unique_fi[q, on=c("machine_name","date")]

  q <- out[time == i.time | is.na(i.time)]

  q_unique <- unique(q, by=c("date", "time","machine_name"))
  out <- q_unique[, .(machine_name,date, time, i.time)]
  out <- unique_fi[out, on=c("machine_name","date", "time")]

  out <- out[, .(n=.N, time=time), by=c("machine_name", "date", "i.time")]

  out <- unique_fi[out, on=c("machine_name","date", "time")]

#return(out)
  out <- unique(out, by=c("machine_id", "datetime"))

  duplicated_queries <- unique(out[n>1,.(n, date, datetime,machine_name)],
                               by=c("date", "machine_name"),
                               fromLast = TRUE)

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
  out = unique(out,by=c("machine_id", "date", "i.time"),fromLast = T)

  o <- out[,.(machine_id, datetime)]
  out <- q[o, on=c("machine_id", "datetime")]

  # we don't need the column 'n' any longer


  nas <- out[,is.na(path)]
  if(any(nas)){
    out_nas <- out[nas,]

    for(i in 1:nrow(out_nas)){
      warning(sprintf("No result for machine_name == %s, date == %s and time == %s. Omiting query",
                      out_nas[i,machine_name],
                      out_nas[i,date],
                      out_nas[i,i.time]
                      ))
    }
  }

  out[, date := NULL]
  out[, time := NULL]
  out[, i.time := NULL]
  out <- na.omit(out)

  data.table::setkeyv(out, colnames(out))
  out
}

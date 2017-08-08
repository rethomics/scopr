#' Build a functional query for loading ethoscope data using the date of experiments and device names
#' to retreive result files
#'
#' This function is designed to list and select experimental files.
#' In general, end-users will want to retrieve  path to their experimental files
#' according to the date and ID of the ethoscope without having to understand the underlying directory structure.
#'
#' @param result_dir The location of the result directory (i.e. the folder containing all the data).
#' @param use_cached whether cache files should be used
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
#' @export
build_ethoscope_query <- function(result_dir, query=NULL, use_cached=FALSE, index_file=NULL){
  if(is.null(index_file))
    checkDirExists(result_dir)
  key <- c("date","machine_name")
  use_date <- F
  if(!is.null(query)){
    q <- copy(data.table::as.data.table(query))
    checkColumns(key, q)
    query_date <- q[, dateStrToPosix(date, tz="GMT")]
    q[, date := query_date]
    use_date <- T
    setkeyv(q,key)
  }

  if(is.null(index_file)){
    if(use_cached)
      all_db_files <- list.files(result_dir,recursive=T, pattern="*\\.rdb$")
    else
      all_db_files <- list.files(result_dir,recursive=T, pattern="*\\.db$")
  }
  else{
    all_db_files <- scan(index_file, what="character")
  }

  fields <- strsplit(all_db_files,"/")
  valid_files <- sapply(fields,length) == 4

  all_db_files <- all_db_files[valid_files]

  invalids = fields[!valid_files]
  if(length(invalids) > 0){
    warning("There are some invalid files:")

    for(i in 1:length(invalids)){
      warning(paste(invalids[[i]]),sep='/')
    }
  }
  fields <- fields[valid_files]
  files_info <- do.call("rbind",fields)

  if(length(all_db_files) == 0){
    stop(sprintf("No .db files detected in the directory '%s'. Ensure it is not empty.",result_dir))
  }
  files_info <- do.call("rbind",fields)
  files_info <- data.table::as.data.table(files_info)
  setnames(files_info, c("machine_id", "machine_name", "date","file"))

  if(use_date)
    files_info[,date:=as.POSIXct(date, "%Y-%m-%d", tz="GMT")]
  else
    files_info[,date:=as.POSIXct(date, "%Y-%m-%d_%H-%M-%S", tz="GMT")]

  files_info[,path := paste(result_dir,all_db_files,sep="/")]
  setkeyv(files_info,key)

  if(is.null(query))
    return(files_info)
  files_info[,n:=.N,by=key(files_info)]
  unique_fi = files_info[,.SD[.N],by=key(files_info)]
  #unique_fi = unique(files_info,fromLast = T)

  out <- unique_fi[q]

  duplicated_queries <- unique(out[n>1,.(n, date,machine_name)])
  if(nrow(duplicated_queries) > 0){
    for( i in 1:nrow(duplicated_queries)){
      str <- "Several files(%i) in machine %s and date %s. Keeping last file. Use date and time if it is not intended.}"
      str <- sprintf(str,duplicated_queries[i,n], duplicated_queries[i,machine_name],duplicated_queries[i,as.character(date)])
      warning(str)
    }
  }
  # we don't need the column 'n' any longer
  out[, n:=NULL]
  nas <- is.na(out[,path])
  if(any(nas)){
    out_nas <- out[nas,]
    for(i in 1:nrow(out_nas)){
      warning(sprintf("No result for machine_name == %s and date == %s. Omiting query",out_nas[i,machine_name],out_nas[i,date]))
    }
  }
  out <- na.omit(out)
  setkeyv(out, union(key(out),colnames(q)))
  out
}

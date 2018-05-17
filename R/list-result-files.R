#' List all available result files
#'
#' This function discovers all ethoscope result files and put them in a [data.table::data.table].
#' This is useful to figure out when and which experiments were performed.
#'
#' @param result_dir the root directory where all data are saved, or the path to a remote directory.
#' @param index_file the name of an index_file, in `result_dir` (needed for loading remote data).
#' @return a [data.table::data.table].
#' Each row is a single experimental file, and columns describe details such as its `path`, start `date` and `time`,
#' and the name and id of the ethoscope used.
#' @seealso
#' * [load_ethoscope] -- to load the actual data
#' * [experiment_info] -- to show the metadata of a specific experiment
#' @export
list_result_files <- function(result_dir, index_file=NULL){
  path = NULL
  key <- c("date", "time","machine_name")

  if(!is.null(index_file)){
    index_file <- paste(result_dir, index_file, sep="/")
    tryCatch({dt_all_files  <- data.table::fread(index_file,
                                                 header = FALSE,
                                                 verbose = FALSE,
                                                 showProgress = FALSE)},
             error = function(e) stop(sprintf("Could not find index file: %s",
                                         index_file)))

    all_db_files <- grep(".*\\.db", dt_all_files$V1, value = TRUE)
  }
  else{
    all_db_files <- list.files(result_dir,recursive=T, pattern="*\\.db$")
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
  data.table::setnames(files_info, c("machine_id", "machine_name", "datetime","file"))
  files_info[, file := NULL]
  parse_datetime <- function(x){
    match <- stringr::str_split(x, "_", simplify=TRUE)
    d <- parse_date(match[,1])
    t <- parse_time(match[,2],format="%H-%M-%S")
    data.table::data.table(date=d,
                           time = t )
  }
  datetime <- parse_datetime(files_info$datetime)

  #return(list(files_info, datetime))
  files_info <- cbind(files_info,datetime)

  files_info[, datetime := as.POSIXct(datetime, "%Y-%m-%d_%H-%M-%S", tz="UTC")]

  files_info[,path := paste(result_dir,all_db_files,sep="/")]

  data.table::setkeyv(files_info,key)
}

# for memoisation
# we obtain data from one ROI and optionaly preanalyse it, by applying FUN.
parse_single_roi <- function(data,
                        min_time = 0,
                        max_time = +Inf,
                        reference_hour = NULL,
                        verbose = TRUE,
                        columns = NULL,
                        cache=NULL,
                        FUN = NULL,
                        ...){

  region_id <- data$region_id
  path <- data$file_info[[1]]$path


  # we get the columns to get from the method used itself
  if(is.null(columns) & !is.null(FUN)){
    needed_columns <- attr(FUN, "needed_columns")
    if(!is.null(needed_columns))
      columns <- needed_columns(...)
  }
  if(verbose)
    cat(sprintf("Loading ROI number %i from:\n\t%s\n",region_id,path))
  if(tools::file_ext(path) != "db")
    stop(sprintf("Unsuported file extention in %s",path))

  fs = file.info(path)["size"]

  if(!is.null(cache)){
    db <- memoise::cache_filesystem(cache, algo="md5")
    parse_single_roi_wrapped_memo <- memoise::memoise(parse_single_roi_wrapped, cache=db)
  }
  else{
    parse_single_roi_wrapped_memo <- parse_single_roi_wrapped
  }

  parse_single_roi_wrapped_memo( data,
                                 min_time,
                                 max_time,
                                 reference_hour,
                                 columns,
                                 file_size= fs,
                                 FUN,
                                 ...
                                 )
}



parse_single_roi_wrapped <- function(data,
                                     min_time = 0,
                                     max_time = +Inf,
                                     reference_hour = NULL,
                                     columns = NULL,
                                     file_size=0,
                                     FUN = NULL,
                                     ...
                                     ){
  region_id <- data$region_id
  id <- data$id
  path <- data$file_info[[1]]$path

  out <- read_single_roi(path,
                         region_id=region_id,
                         min_time = min_time,
                         max_time = max_time,
                         reference_hour = reference_hour,
                         columns=columns,
                         time_stamp = time_stamp)

  if(is.null(out) || nrow(out) == 0){
    warning(sprintf("No data in ROI %i, from FILE %s. Skipping",region_id, path))
    return(NULL)
  }


  #id <- as.factor(sprintf("%02d|%s",region_id,experiment_id))

  old_cols <- data.table::copy(names(out))
  out[,id := id]
  data.table::setcolorder(out,c("id", old_cols))
  data.table::setkeyv(out, "id")

  behavr::setbehavr(out, data)

  if(!is.null(FUN)){
    out <- FUN(out,...)

    is_empty <- is.null(out)
    if(!is_empty)
      is_empty <- nrow(out) == 0
    if(is_empty){
      warning(sprintf("No data in ROI %i after running FUN, from FILE %s. Skipping",region_id, path))
      return(NULL)
    }

  }
  out
}

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
  experiment_id <- data$experiment_id
  path <- data$path

  if(verbose)
    cat(sprintf("Loading ROI number %i from:\n\t%s\n",region_id,path))

  if(tools::file_ext(path) != "db")
    stop(sprintf("Unsuported file extention in %s",path))

  # for memoisation
  if(!is.null(cache)){
    time_stamp = file.info(path)["mtime"]
    db <- memoise::cache_filesystem(cache, algo="md5")
    read_single_roi_memo <- memoise::memoise(read_single_roi, cache=db)
  }

  else{
    time_stamp = NULL
    read_single_roi_memo <- read_single_roi
  }


  out <- read_single_roi_memo(path,
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


  id <- sprintf("%02d|%s",region_id,experiment_id)

  old_cols <- data.table::copy(names(out))
  out[,id := id]
  data.table::setcolorder(out,c("id", old_cols))
  data.table::setkeyv(out, "id")

  meta <- data.table::as.data.table(data)
  meta <- cbind(id=id,meta)
  meta[, path_list := lapply(path, function(x){
    list(list(file=basename(x),path=x))
    })]
  meta[, path:=NULL]
  data.table::setnames(meta, "path_list", "path")
  data.table::setkeyv(meta, "id")

  out <- behavr::behavr(out, meta)

  if(!is.null(FUN)){
    out <- FUN(out,...)
    if(is.null(out)){
      warning(sprintf("No data in ROI %i after running FUN, from FILE %s. Skipping",region_id, path))
      return(NULL)
    }
  }
  out
}

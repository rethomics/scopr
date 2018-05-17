#' @rdname link_ethoscope_metadata
#' @param remote_dir the url of the result directory on the data server
#' @param overwrite_local whether to download all files.
#' The default, `FALSE`, is to only fetch files if they are newer on the remote.
#' @inheritParams link_ethoscope_metadata
#' @inheritParams load_ethoscope
#' @export
link_ethoscope_metadata_remote <- function(x,
                                           remote_dir,
                                           result_dir,
                                           index_file="index.txt",
                                           overwrite_local=FALSE,
                                           verbose=TRUE){


  path = dst_path = machine_id = machine_name = datetime = V2 = id = file_size__ = NULL

  query <- x
  # if query is a readable csv file, we parse it
  if(is.character(query) & length(query) == 1)
    query <- data.table::fread(query)


  if(!"path"  %in% colnames(query)){
    check_columns(c("machine_name", "date"), query)

    remote_query <- build_query(result_dir = remote_dir,
                              query = query,
                              index_file = index_file)
  }

  remote_query[, file := basename(path)]
  remote_query[,
               dst_path := paste(result_dir,
                                 machine_id,
                                 machine_name,
                                 format(datetime,"%Y-%m-%d_%H-%M-%S"),
                                 file,sep="/")
               ]
  last_points  <- data.table::fread(paste(remote_dir, index_file, sep="/"),
                                    header=F,
                                    verbose = FALSE,
                                    showProgress = verbose)
  if(!"V2" %in% names(last_points)){
    warning("No time stamp on remote index. All the files will be downloaded each time!")
    last_points[, V2:=+Inf]
  }
  data.table::setnames(last_points,c("V1", "V2"), c("path", "file_size__" ))
  last_points[, path:=paste(remote_dir,path,sep="/")]

  remote_query <-last_points[remote_query, on="path"]


  remote_query <- unique(remote_query, by="path")
  remote_query[, id := 1:nrow(remote_query)]

  remote_query[, mirror_ethoscope_results(path,
                                          dst_path,
                                          overwrite_local = overwrite_local,
                                          remote_size = file_size__,
                                          verbose=verbose) ,
               by=id]
  remote_query[, file_size__:=NULL]
  link_ethoscope_metadata(query, result_dir = result_dir)
}


mirror_ethoscope_results <- function(remote,
                                     local,
                                     remote_size = NA,
                                     overwrite_local = FALSE,
                                     verbose=TRUE){

  if(overwrite_local |
     is_remote_newer(local, remote_size)){
    if(verbose)
      message(sprintf("Downloading %s to %s", remote, local))
    download_create_dir(remote, local)
  }
  else{
  if(verbose)
    message(sprintf("Skipping %s", remote))
  }
}



download_create_dir <-function(src,dst){
  tmp <- tempfile()
  tryCatch({
    download.file(src,tmp, mode="wb", method="libcurl",quiet = TRUE)
    dir.create(dirname(dst), recursive=T, showWarnings = FALSE)
    file.copy(tmp, dst)
    },
    finally = {unlink(tmp)}
    )

}


is_remote_newer <- function(local, remote_size){
  local_size <- file_size(local)
  if(is.na(local_size))
    return(TRUE)

  if(local_size != remote_size & !is.na(remote_size))
    return(TRUE)
  return(FALSE)

}


file_size <- function(FILE){
  as.numeric(file.info(FILE)["size"])
}

#' @rdname parse_query
#' @param remote_dir the url of the result directory on the data server
#' @param overwrite_local whether to download all files.
#' The default, `FALSE`, is to only fetch files if they are newer on the remote.
#' @inheritParams query_ethoscopes
#' @export
parse_remote_query <- function(x,
                                remote_dir,
                                result_dir,
                                index_file="index.txt",
                                overwrite_local=FALSE,
                                verbose=TRUE){

  query <- x
  # if query is a readable csv file, we parse it
  if(is.character(query) & length(query) == 1)
    query <- data.table::fread(query)


  if(!"path"  %in% colnames(query)){
    check_columns(c("machine_name", "date"), query)




  remote_query <- build_query(query,
                                      result_dir = remote_dir,
                                      index_file = index_file)
  }

  remote_query[,
               dst_path := paste(result_dir,
                                 machine_id,
                                 machine_name,
                                 format(datetime,"%Y-%m-%d_%H-%M-%S"),
                                 file,sep="/")
               ]
  last_points  <- data.table::fread(paste(remote_dir, index_file, sep="/"), header=F)
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
                                          overwrite_local=TRUE,
                                          remote_size = file_size__,
                                          verbose=verbose) ,
               by=id]
  remote_query[, file_size__:=NULL]
  parse_query(query, result_dir = result_dir)
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
  dir.create(dirname(dst), recursive=T, showWarnings = FALSE)
  download.file(src,dst, mode="wb", method="libcurl",quiet = TRUE)
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
# last_point_db <- function(FILE){
#   max_t <- NA_integer_
#   tryCatch({
#     con = NULL
#     rois <- list_all_rois(FILE)
#     con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags=RSQLite::SQLITE_RO)
#     max_t <- max(sapply( rois, function(x){
#       command <- sprintf("SELECT t FROM ROI_%i ORDER BY id DESC LIMIT 1", x)
#       as.integer(RSQLite::dbGetQuery(con, command)$t)
#     }))
#   }, error = function(e){},
#   finally = {if(!is.null(con)) RSQLite::dbDisconnect(con)})
#   max_t
# }
#
#

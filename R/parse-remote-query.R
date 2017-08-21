#' @inheritParams parse_query
#' @export
parse_remote_query <- function(query,
                                remote_dir,
                                result_dir,
                                index_file="index.txt",
                                overwrite_local=FALSE,
                                verbose=TRUE){

  remote_query <- build_query(query,
                                      result_dir = remote_dir,
                                      index_file = index_file)

  # todo, fetch timestamps
  remote_query[,
               dst_path := paste(result_dir,machine_id, machine_name, format(datetime,"%Y-%m-%d_%H-%M-%S"), file,sep="/")
               ]

  remote_query[, id := dst_path]
  remote_query[, mirror_ethoscope_results(path, dst_path, overwrite_local=TRUE, verbose=verbose) , by=dst_path]
  parse_query(query, result_dir = result_dir)
}


mirror_ethoscope_results <- function(remote,
                                     local,
                                     time_stamp_remote=+Inf,
                                     overwrite_local=FALSE,
                                     verbose=TRUE){



  if(overwrite_local |
     is_remote_newer(remote, local, time_stamp_remote)){
    if(verbose)
      message(sprintf("Downloading %s to %s", remote, local))
    download_create_dir(remote, local)
  }
  if(verbose)
    message(sprintf("Skipping %s", remote))
}



download_create_dir <-function(src,dst){
  dir.create(dirname(dst), recursive=T, showWarnings = FALSE)
  download.file(src,dst, mode="wb", method="libcurl",quiet = TRUE)
}


is_remote_newer <- function(remote, local, time_stamp_remote){
  if(! file.exists(local))
    return(TRUE)
  time_stamp_local <- file.info(local)["mtime"]
  if(time_stamp_local < time_stamp_remote)
    return(TRUE)
  return(FALSE)

}
#
# result_dir = "/tmp/test"
# query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
#                     date = c("2016-01-25", "2016-02-17","2016-01-25"),
#                     time = c("21:46:14", NA, NA),
#                     test=c(1,2,3)
#                     #                   lifespan=c(10,12, NA)
# )
# parse_remote_query(query,
#                       remote_dir = "https://raw.githubusercontent.com/rethomics/scopr/master/inst/extdata/ethoscope_results",
#                       result_dir = result_dir,
#                       overwrite_local = T)
#

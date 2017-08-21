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
  remote_query <- unique(remote_query, by="path")
  remote_query[, id := 1:nrow(remote_query)]
  remote_query[, mirror_ethoscope_results(path, dst_path, overwrite_local=TRUE, verbose=verbose) ,
               by=id]
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
  else{
  if(verbose)
    message(sprintf("Skipping %s", remote))
  }
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
# q <- data.table::fread("~/Desktop/test_remote/query-rutabaga.txt")
# out <- parse_remote_query(q[1:50],
#                      remote_dir = "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/",
#                      result_dir = result_dir,
#                      overwrite_local = T)
#
# test <- query_ethoscopes(out,max_time = behavr::days(1), reference_hour = 9.0)
# test <- query_ethoscopes(out,max_time = behavr::days(1), reference_hour = 9.0, cache = "/tmp/etho_cache")
# test2 <- query_ethoscopes(out,max_time = behavr::days(1), reference_hour = 9.0, cache = "/tmp/etho_cache")
#
# sum(!test == test2)
# out[, id:= 1:nrow(out)]
# out[, scopr:::mirror_ethoscope_results(path, dst_path, overwrite_local=TRUE, verbose=T),
#     by=id]
# #
#
#
#
# out

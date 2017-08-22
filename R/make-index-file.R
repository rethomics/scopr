#' Used by platform administrators to make an index file listing all experiments
#'
#' This function builds an index which lists the available experimental file.
#' It is useful to build indices when outing data on a remote server (e.g. FTP).
#' @return the path to the index file written (in `result_dir`))
#' @inheritParams list_result_files
#' @export
make_index_file <- function(result_dir, index_file="index.txt"){
  files <- list_result_files(result_dir)
  out <- files[, .(last_point = last_point_db(path)), by=c(key(files), "machine_id", "datetime", "file")]
  out[,
      path := paste( machine_id,
                     machine_name,
                     format(datetime,"%Y-%m-%d_%H-%M-%S"),
                     file,sep="/")
      ]
  out <- out[, .(path, last_point)]

  out_file <- paste(result_dir, index_file, sep="/")
  write.table(out,
              out_file,
              sep=",",
              row.names = FALSE,
              col.names = FALSE)
  out_file
}


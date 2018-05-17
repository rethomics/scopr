#' Used by platform administrators to make an index file listing all experiments
#'
#' This function builds an index which lists the available experimental file.
#' It is useful to build indices when outing data on a remote server (e.g. FTP).
#' @return the path to the index file written (in `result_dir`))
#' @inheritParams list_result_files
#' @noRd
make_index_file <- function(result_dir, index_file="index.txt"){

  path = dst_path = machine_id = machine_name = datetime = V2 = id = file_size__ =NULL

  path = . = machine_id = machine_name = datetime = last_point = write.table = NULL


  files <- list_result_files(result_dir)
  files[, file := basename(path)]

  out <- files[, .(last_point = file_size(path)), by=c(data.table::key(files), "machine_id", "datetime", "file")]
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


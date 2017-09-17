#' Link ethoscope metadata to the matching result files
#'
#' These functions augment metadata so it can be subsequently loaded (via [load_ethoscope]).
#'
#' @param x object such as a [data.frame], or the name of a file (see detail)
#' @param result_dir the directory where all data are saved
#' @param index_file the name of an index_file, in `result_dir` (useful for loading remote data).
#' @return a [data.table::data.table] with the same rows as x, and extra columns for further data loading
#' @details
#' These function will augment metadata from three different types of inputs (`x`):
#' 1. A vector of names of `.db` files (only for local queries with `parse_query`)
#' 2. A [data.frame] (recomended)
#' In this case, the function will try to match requested data with data available on `result_dir`.
#' The provided [data.table] has typically one row per requested individial and the columns
#' (not necessarily in this order):
#'     * `machine_name` -- the name of the machine in which the individual was (e.g. `"ETHOSCOPE_001"`)
#'     * `date` -- the start date of the experiment formated as `"YYYY-MM-DD"`
#'     * `region_id` -- the ROI in which the animal was. When *not provided, all regions are queried*.
#'     * `time` -- the start time of the experiment formated as "HH:MM:SS".
#'        When *not provided*, and multiple expriment for the same machine exist, *only the last one is loaded*.
#'     * `???` -- any number of arbitrary columns* to associate `conditions`/`treatments`/`genotypes`/... to the previous columns.
#' 3. The name of a CSV file that contains a table as described in `2`.
#' @examples
#' # metadata with no region_id, all regions will be loaded with the same metadata
#' dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
#' data(no_region_id_metadata)
#' metadata <- link_ethoscope_metadata(no_region_id_metadata, dir)
#' print(metadata)
#' # metadata with region_id, only stated regions will be loaded with specific metadata
#' data(region_id_metadata)
#' metadata <- link_ethoscope_metadata(region_id_metadata, dir)
#' print(metadata)
#' \dontrun{
#' # if your files are stored on a remote server,
#' # this will download to a local directory only the needed files
#' REMOTE <- "ftp://a/remote/server/"
#' LOCAL_DIR <- "/where/I/store/the/data/"
#' metadata <- link_ethoscope_metadata_remote(region_id_metadata,
#'                                            REMOTE,
#'                                            LOCAL_DIR)
#'
#' }
#' @name link_ethoscope_metadata
#' @seealso
#' * [load_ethoscope] -- to load the actual data
#' * [list_result_files] -- to list available file
#' @references
#' * [metadata tutorial](https://rethomics.github.io/metadata.html) -- how to work with metadata
#' @export
link_ethoscope_metadata <- function(x, result_dir=NULL, index_file=NULL){
  query <- x
  # if query is a readable csv file, we parse it
  if(is.character(query) & length(query) == 1)
    tryCatch( query <- data.table::fread(query), error=function(e){})

  # case 1 query is a file, or a vector of files
  if(is.character(query)){
    # todo check whether file exists
    out <- data.table::data.table(path=query, experiment_id=basename(query))
    # We load all available ROIs since user did not provide ROI info
    out <- out[,list(
      region_id=list_all_rois(path),
      experiment_id=experiment_id),by=path]
  }
  else if(is.data.frame(query)){
    if(!"path"  %in% colnames(query)){
      if(is.null(result_dir))
        stop("You must specify a result_dir to lookup results!")
      check_columns(c("machine_name", "date"), query)
      query <- build_query(result_dir, query, index_file)
    }

    check_columns("path", query)
    out <- data.table::copy(data.table::as.data.table(query))
    #fixme check uniqueness of file/use path as key?
    out[, path := as.character(path)]
    out[, experiment_id := basename(path)]

    data.table::setkey(out,experiment_id)

    # when user did not specify ROIs, we load them all.

    if(!"region_id" %in% colnames(query)){
      m <- out[,list(region_id = list_all_rois(path)),by=key(out)]
      out <- m[out]
    }

  }
  else{
    stop("Unexpected `x` argument!")
  }

  check_columns(c("experiment_id","region_id","path"), out)
  data.table::setkeyv(out,c("experiment_id", "region_id"))
  out[,n:=.N,keyby=key(out)]
  out <- unique(out, by=key(out))
  duplicated_rows <- out[n>1]

  if(nrow(duplicated_rows)>0){
    for(i in 1:nrow(duplicated_rows)){
      str <- "Duplicated rows in metadata! experiment %s and region %i}"
      str <- sprintf(str, duplicated_rows[i,experiment_id],duplicated_rows[i,region_id])
      warning(str)
    }

  }

  #out[, id := as.factor(sprintf("%02d|%s",region_id,experiment_id))]
  out[, id := as.factor(sprintf("%s|%02d",substr(experiment_id,1,26), region_id))]

  file_info <- out[,.(file_info =  list(list(path = path, file = basename(path)))), by="id"]
  out <- file_info[out, on="id"]
  out[, path := NULL]
  out[, n := NULL]
  out[, experiment_id := NULL]
  data.table::setkeyv(out, "id")

  return(out)
}

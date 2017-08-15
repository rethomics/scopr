parse_query <- function(query, result_dir=NULL){

  # case 1 query is a file, or a vector of files
  if(is.character(query)){
    # todo check whether file exists
    out <- data.table(path=query, experiment_id=basename(query))
    # We load all available ROIs since user did not provide ROI info
    out <- out[,list(
      region_id=list_all_rois(path),
      experiment_id=experiment_id),by=path]
  }
  else if(is.data.frame(query)){
    if(!"path"  %in% colanmes(query)){
      checkColumns(c("machine_name", "date"), query)
      query <- build_query(result_dir)
    }

    checkColumns("path", colnames(query))
    out <- copy(as.data.table(query))
    #fixme check uniqueness of file/use path as key?
    out[,path := as.character(path)]
    out[,experiment_id := basename(path)]

    setkey(out,experiment_id)

    # when user did not specify ROIs, we load them all.
    if(!"region_id" %in% colnames(query)){
      m <- out[,list(region_id = list_all_rois(path)),by=key(out)]
      out <- m[out]
    }

  }
  else{
    stop("Unexpected `query` argument!")
  }

  checkColumns(c("experiment_id","region_id","path"),colnames(out))

  setkeyv(out,c("experiment_id","region_id"))
  out[,n:=.N,by=key(out)]
  out <- unique(out, by=key(out))
  duplicated_rows <- out[n>1]

  if(nrow(duplicated_rows)>0){
    for(i in 1:nrow(duplicated_rows)){
      str <- "Duplicated rows in query! experiment %s and region %i}"
      str <- sprintf(str, duplicated_rows[i,experiment_id],duplicated_rows[i,region_id])
      warning(str)
    }

  }
  out[,n:=NULL]
  return(out)
}

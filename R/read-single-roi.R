
read_single_roi <- function( FILE,
                             region_id,
                             min_time = 0,
                             max_time = +Inf,
                             reference_hour = NULL,
                             columns = NULL,
                             time_stamp=NULL # only used for memoisation
                             ){

  roi_idx = var_name = rois_idx = id = w = h = functional_type = sql_type = is_inferred = has_interacted = NULL
  experiment_info <- experiment_info(FILE)

  if(min_time >= max_time)
    stop("min_time can only be lower than max_time!")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags=RSQLite::SQLITE_RO)

  tryCatch({
    var_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM VAR_MAP"))
    data.table::setkey(var_map, var_name)
    roi_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_MAP"))
    roi_row <- roi_map[roi_idx == region_id]

    if(nrow(roi_row) == 0 ){
      warning(sprintf("ROI %i does not exist, skipping",region_id))
      return(NULL)
    }
    if(max_time == Inf)
      max_time_condition <- ""
    else
      max_time_condition <-  sprintf("AND t < %e", max_time * 1000)

    min_time <- min_time * 1000
    if(is.null(columns)){
      selected_cols = "*"
    }
    else{
      if("is_inferred" %in% var_map$var_name)
        columns <- unique(c(columns, "is_inferred"))

      if(any(!columns %in% var_map$var_name))
        stop(sprintf("Some of the requested columns are NOT available. Available columns are: %s",
                     paste(var_map$var_name, collapse = " ")))
      selected_cols = paste(unique(c("t",columns)),collapse=", ")
      var_map <- var_map[columns]
      data.table::setkey(var_map, var_name)
    }
    # todo filter here is inferred
    sql_query <- sprintf("SELECT %s FROM ROI_%i WHERE t >= %e %s",
                         selected_cols, region_id,
                         min_time, max_time_condition )
    result <- RSQLite::dbGetQuery(con, sql_query)

    # todo here, use setDT!!
    # however, bottlenexk is sqlite 10times slower than reading equivalent csv!!!!
    roi_dt <- data.table::as.data.table(result)
    if("id" %in% colnames(roi_dt))
      roi_dt[, id := NULL]

    if(!is.null(reference_hour)){
      p <- experiment_info$date_time
      hour_start <- as.numeric(format(p, "%H")) +
                    as.numeric(format(p, "%M")) / 60 +
                    as.numeric(format(p, "%S")) / 3600
      ms_after_ref <- ((hour_start - reference_hour) %% 24) * 3600 * 1000
      # standardised time time_in_seconds
      roi_dt[, t:= (t + ms_after_ref)/ 1e3 ]
    }
    else{
      # just time_in_seconds
      roi_dt[, t:= t/1e3]
    }


    roi_width <- max(c(roi_row[,w], roi_row[,h]))
    for(var_n in var_map$var_name){
      if(var_map[var_n, functional_type] == "distance"){
        roi_dt[, (var_n) := get(var_n) / roi_width]
      }
      if(var_map[var_n, sql_type] == "BOOLEAN"){
        roi_dt[, (var_n) := as.logical(get(var_n))]
      }
    }


    if(all(c("is_inferred", "has_interacted") %in% colnames(roi_dt))){
      # we keep infered cols if interaction happened!
      roi_dt <- roi_dt[is_inferred==F | has_interacted == T]
    }
    else if("is_inferred" %in% colnames(roi_dt))
      roi_dt <- roi_dt[is_inferred==F]
        roi_dt[, is_inferred := NULL]
    return(roi_dt)
  },
  finally= {RSQLite::dbDisconnect(con)}
  )
}



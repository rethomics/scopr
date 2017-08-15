#' Checks if the expected columns are all in a given character vector. Through error if not
#' @param expected_colnames the colnmaes that should be in a dt
#' @param df the dataframe in which the columns are expected
#' @noRd
check_columns <- function(expected_colnames, df){
  cols <- colnames(df)
  col_not_found <- expected_colnames[!expected_colnames %in% cols]
  if(length(col_not_found)>0)
    stop(sprintf("The following columns are needed, but not found: %s",col_not_found))
}

context("query_ethoscopes")

test_that("query ethoscopes works", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
                      )
  query <- parse_query(query,dir)
  dt <- query_ethoscopes(query, verbose=F)
  expect_equal(nrow(dt[meta=TRUE]), 60)
})


test_that("query ethoscopes works with multiple cores", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
  )
  query <- parse_query(query, dir)
  dt <- query_ethoscopes(query, verbose=F)
  dt_m <- query_ethoscopes(query, ncores=2, verbose=F)
  expect_identical(dt_m, dt)
})
#
#dir <- "/data/ethoscope_results"
#QUERY_FILE = "/home/quentin/comput/sleep_analysis_experiments-git/ethoscope_paper/20160404_overnight_dsd/query.csv"
#q <- parse_query(QUERY_FILE, dir)[status=="OK"]
# dt <- query_ethoscopes(q[machine_name=="ETHOSCOPE_018"], dir, columns=c("x"), max_time=100)
# system.time(dt <- query_ethoscopes(q[1:100], dir, ncores=8))
# system.time(dt <- query_ethoscopes(q[1:100], dir, ncores=1))
#
#
#
# Error in find.package(if (is.null(package)) loadedNamespaces() else package,  :
#                         there is no package called ‘.GlobalEnv’
#                       11.
#                       as.character(jsub[[1L]]) %chin% c("list", ".")
#                       10.
#                       `[.data.table`(roi_dt, , `:=`(t, t/1000)) at read-single-roi.R#69
#                       9.
#                       roi_dt[, `:=`(t, t/1000)] at read-single-roi.R#69
#                       8.
#                       tryCatchList(expr, classes, parentenv, handlers)
#                       7.
#                       tryCatch({
#                         var_map <- data.table::as.data.table(RSQLite::dbGetQuery(con,
#                                                                                  "SELECT * FROM VAR_MAP"))
#                         data.table::setkey(var_map, var_name) ... at read-single-roi.R#16
#                         6.
#                         loading_FUN(path, region_id = region_id, min_time = min_time,
#                                     max_time = max_time, reference_hour = reference_hour, columns = columns) at parse-single-roi.R#27
#                         5.
#                         FUN(X[[i]], ...)
#                         4.
#                         lapply(l_rows, parse_single_roi, min_time, max_time, reference_hour,
#                                verbose, columns = columns, FUN, ...) at query-ethoscopes.R#62
#                         3.
#                         FUN(X[[i]], ...)
#                         2.
#                         lapply(q_l, load_fun) at query-ethoscopes.R#75
#                         1.
#                         query_ethoscopes(q, dir, columns = c("x"), max_time = 100)
#

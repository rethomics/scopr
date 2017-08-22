test_that("parse_remote_query with date and machine name", {

  remote_dir <-"https://raw.githubusercontent.com/rethomics/scopr/master/inst/extdata/ethoscope_results"
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
                      #                   lifespan=c(10,12, NA)
  )
  result_dir <- tempdir()
  out1 <- scopr:::parse_remote_query(query,
                                     remote_dir = remote_dir,
                                     result_dir = result_dir,
                                     overwrite_local = TRUE)

  out2 <- scopr:::parse_query(query,
                              dir)

  dt1 <- query_ethoscopes(out1)
  dt2 <- query_ethoscopes(out2)

  dt2[,path:=NULL, meta=T]
  dt1[,path:=NULL, meta=T]

  expect_identical(dt2, dt1)
})





test_that("parse_query with date, machine name, and ROIs", {
  remote_dir <-"https://raw.githubusercontent.com/rethomics/scopr/master/inst/extdata/ethoscope_results"
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)

  )

  # no_region_id_query <- as.data.frame(query)
  # devtools::use_data(no_region_id_query, overwrite = TRUE)

  query <- data.table::as.data.table(query)
  query <- query[,.(region_id=1:5),by=c(colnames(query))]
  query[, treatment := 1:3]

  result_dir <- tempdir()
  out1 <- scopr:::parse_remote_query(query,
                                     remote_dir = remote_dir,
                                     result_dir = result_dir,
                                     overwrite_local = TRUE)



  out2 <- scopr:::parse_query(query,
                              dir)

  dt1 <- query_ethoscopes(out1)
  dt2 <- query_ethoscopes(out2)


  dt2[,path:=NULL, meta=T]
  dt1[,path:=NULL, meta=T]

  expect_identical(dt2, dt1)
})




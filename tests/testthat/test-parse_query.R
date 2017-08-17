context("parse_query")

test_that("parse_query with single file", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  out <- scopr:::parse_query(test_file)
  expect_equal(nrow(out), 20)
  expect_equal(unique(out$path), test_file)
  expect_equal(sort(out$region_id), 1:20)
  out
})




test_that("parse_query with date and machine name", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
   #                   lifespan=c(10,12, NA)
  )
  out <- scopr:::parse_query(query, dir)

  #no_region_id_query <- as.data.frame(out)
  #devtools::use_data(no_region_id_query)
  expect_equal(nrow(out), 60)
})





test_that("parse_query with date, machine name, and ROIs", {

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

  #region_id_query <- as.data.frame(query)
  #devtools::use_data(region_id_query, overwrite = TRUE)

  out <- scopr:::parse_query(query, dir)
  expect_equal(nrow(out), 3*5)
})


test_that("parse_query with path", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  query <- data.frame(path =test_file,
                      test=c(1,2,3),
                      region_id=1:3)


  out <- scopr:::parse_query(query, dir)
  expect_equal(nrow(out), 3)
  expect_equal(unique(out$path), test_file)
  expect_equal(sort(out$region_id), 1:3)
})


test_that("parse_query detect duplicated rows", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)

  )

  query <- data.table::as.data.table(query)
  query <- query[,.(region_id=1:3),by=c(colnames(query))]
  # duplicate
  query[machine_name=="E_029" & region_id==3, region_id := 2]


  expect_warning(out <- scopr:::parse_query(query, dir), "Duplicated row")
  expect_equal(nrow(out), 3*3-1)
})


context("load_ethoscope")

test_that("query ethoscopes works", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
                      )
  query <- link_ethoscope_metadata(query,dir)
  dt <- load_ethoscope(query, verbose=F)
  expect_equal(nrow(dt[meta=TRUE]), 60)
})


test_that("query ethoscopes works with multiple cores", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
  )
  query <- link_ethoscope_metadata(query, dir)
  dt <- load_ethoscope(query, verbose=F)
  dt_m <- load_ethoscope(query, ncores=2, verbose=F)
  expect_identical(dt_m, dt)
})

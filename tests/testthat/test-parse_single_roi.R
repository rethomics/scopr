context("parse_single_roi")

test_that("parse_single_roi works in normal conditions", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  data <- data.table::data.table(region_id=1, experiment_id="test", path=test_file)
  a <- scopr:::parse_single_roi(data)
  a <- scopr:::parse_single_roi(data, FUN= function(x){behavr::bin_apply_all(x,y = x)})
  a[meta=T]
})


test_that("parse_single_roi works with memosiation", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  cache <- tempfile("scopr_test_cache")

  data <- data.table::data.table(region_id=1, experiment_id="test", path=test_file)
  a <- scopr:::parse_single_roi(data)
  b <- scopr:::parse_single_roi(data, cache = cache)
  c <- scopr:::parse_single_roi(data, cache = cache)

  ?identical(b, c,ignore.bytecode = T)



  expect_identical(a, b)
  expect_true(all(c == b) & identical(c[meta=T], b[meta=T]))
})



test_that("parse_single_roi works with autocolumn finding", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  data <- data.table::data.table(region_id=1, experiment_id="test", path=test_file)

  foo <- function(x){behavr::bin_apply_all(x,y = x)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }
  a <- scopr:::parse_single_roi(data, FUN= foo)
  a[meta=T]
  attr(foo, "needed_columns") <- function(...){
    "w"
  }
  expect_error(scopr:::parse_single_roi(data, FUN= foo))

})
